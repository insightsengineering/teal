ui_teal_1.0 <- function(id,
                        data,
                        modules,
                        title = build_app_title(),
                        header = tags$p(),
                        footer = tags$p()) {
  checkmate::assert_character(id, max.len = 1, any.missing = FALSE)
  checkmate::assert_multi_class(data, c("teal_data", "teal_data_module"))
  checkmate::assert(
    .var.name = "title",
    checkmate::check_string(title),
    checkmate::check_multi_class(title, c("shiny.tag", "shiny.tag.list", "html"))
  )
  checkmate::assert(
    .var.name = "header",
    checkmate::check_string(header),
    checkmate::check_multi_class(header, c("shiny.tag", "shiny.tag.list", "html"))
  )
  checkmate::assert(
    .var.name = "footer",
    checkmate::check_string(footer),
    checkmate::check_multi_class(footer, c("shiny.tag", "shiny.tag.list", "html"))
  )

  if (is.character(title)) {
    title <- build_app_title(title)
  } else {
    validate_app_title_tag(title)
  }

  if (checkmate::test_string(header)) {
    header <- tags$p(header)
  }

  if (checkmate::test_string(footer)) {
    footer <- tags$p(footer)
  }

  ns <- NS(id)

  # show busy icon when `shiny` session is busy computing stuff
  # based on https://stackoverflow.com/questions/17325521/r-shiny-display-loading-message-while-function-is-running/22475216#22475216 # nolint: line_length.
  shiny_busy_message_panel <- conditionalPanel(
    condition = "(($('html').hasClass('shiny-busy')) && (document.getElementById('shiny-notification-panel') == null))", # nolint: line_length.
    tags$div(
      icon("arrows-rotate", "spin fa-spin"),
      "Computing ...",
      # CSS defined in `custom.css`
      class = "shinybusymessage"
    )
  )
  # todo: add reporter previewer to the list of the module
  data_elem <- ui_data(ns("data"), data = data, title = title, header = header, footer = footer)
  tabs_elem <- ui_teal_module(id = ns("root_module"), modules = modules)

  fluidPage(
    title = title,
    theme = get_teal_bs_theme(),
    include_teal_css_js(),
    tags$header(header),
    tags$hr(class = "my-2"),
    shiny_busy_message_panel,
    data_elem,
    tabs_elem,
    tags$hr(),
    tags$footer(
      tags$div(
        footer,
        teal.widgets::verbatim_popup_ui(ns("sessionInfo"), "Session Info", type = "link"),
        br(),
        downloadLink(ns("lockFile"), "Download .lock file"),
        textOutput(ns("identifier"))
      )
    )
  )
}

srv_teal_1.0 <- function(id, data, modules, filter = teal_slices()) {
  checkmate::assert_character(id, max.len = 1, any.missing = FALSE)
  checkmate::assert_multi_class(data, c("teal_data", "teal_data_module"))
  checkmate::assert_class(modules, "teal_modules")
  checkmate::assert_class(filter, "teal_slices")

  moduleServer(id, function(input, output, session) {
    logger::log_trace("srv_teal_with_splash initializing module with data.")

    output$identifier <- renderText(
      paste0("Pid:", Sys.getpid(), " Token:", substr(session$token, 25, 32))
    )

    teal.widgets::verbatim_popup_srv(
      "sessionInfo",
      verbatim_content = utils::capture.output(utils::sessionInfo()),
      title = "SessionInfo"
    )

    output$lockFile <- teal_lockfile_downloadhandler()

    # `JavaScript` code
    run_js_files(files = "init.js")

    # set timezone in shiny app
    # timezone is set in the early beginning so it will be available also
    # for `DDL` and all shiny modules
    get_client_timezone(session$ns)
    observeEvent(
      eventExpr = input$timezone,
      once = TRUE,
      handlerExpr = {
        session$userData$timezone <- input$timezone
        logger::log_trace("srv_teal@1 Timezone set to client's timezone: { input$timezone }.")
      }
    )

    data_rv <- srv_data("data", data = data, modules = modules, filter = filter)

    # Restore filter from bookmarked state, if applicable.
    filter_restored <- restoreValue("filter_state_on_bookmark", filter)
    if (!is.teal_slices(filter_restored)) {
      filter_restored <- as.teal_slices(filter_restored)
    }

    datasets_rv <- if (!isTRUE(attr(filter, "module_specific"))) {
      eventReactive(data_rv(), {
        logger::log_trace("srv_teal_module@1 initializing FilteredData")
        # Otherwise, FilteredData will be created in the modules' scope later
        progress_data <- Progress$new(
          max = length(unlist(module_labels(modules)))
        )
        on.exit(progress_data$close())
        progress_data$set(message = "Preparing data filtering", detail = "0%")
        filtered_data <- teal_data_to_filtered_data(data_rv())
        # todo: We need a bookmark feature back to test filter_restored
        # todo: (question) what filters should be restored after refreshing of the data:
        #    1. filters that were set by the app developer
        #    2. filters that were set by the user (preferable by @gogonzo)
        filtered_data$set_filter_state(filter_restored)
        filtered_data
      })
    }

    modules_out <- srv_teal_module(
      id = "root_module",
      data_rv = data_rv,
      datasets = datasets_rv,
      modules = modules,
      filter = filter_restored
    )
  })
}
