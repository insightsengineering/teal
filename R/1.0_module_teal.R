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

  data_elem <- ui_data(ns("data"), data = data, title = title, header = header, footer = footer)
  tabs_elem <- ui_teal_module(id = ns("root_module"), modules = modules)

  fluidPage(
    title = title,
    theme = get_teal_bs_theme(),
    include_teal_css_js(),
    tags$header(header),
    tags$hr(class = "my-2"),
    shiny_busy_message_panel,
    tabs_elem,
    tags$div(
      id = "teal-util-icons",
      style = "margin-left: auto;",
      data_elem,
      actionButton(ns("filter_manager"), NULL, icon = icon("filter")),
      actionButton(ns("snapshot_manager"), NULL, icon = icon("floppy-disk")),
      actionButton(ns("bookmark_manager"), NULL, icon = icon("bookmark")),
      tags$button(
        class = "btn action-button filter_hamburger", # see sidebar.css for style filter_hamburger
        href = "javascript:void(0)",
        onclick = "toggleFilterPanel();", # see sidebar.js
        title = "Toggle filter panel",
        icon("fas fa-bars")
      )
    ),
    tags$script(HTML("
      $(document).ready(function() {
        $('#teal-util-icons').appendTo('#root_module-active_tab');
      });
    ")),
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
    logger::log_trace("srv_teal initializing.")

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

    # Resolve mapping list to keep it constistent for filter manager.
    # - when !module_specific then all filters are global
    # - global_filters ids needs to be repeated in mapping for each module
    if (!isTRUE(attr(filter_restored, "module_specific"))) {
      attr(filter_restored, "mapping") <- list(
        global_filters = isolate(sapply(filter_restored, `[[`, "id"))
      )
    }
    module_labs <- module_labels(modules)
    new_mapping <- sapply(
      unlist(module_labs, use.names = FALSE),
      simplify = FALSE,
      function(module_lab) {
        unlist(attr(filter_restored, "mapping")[c(module_lab, "global_filters")], use.names = FALSE)
      }
    )
    attr(filter_restored, "mapping") <- new_mapping

    # singleton controlled by filter-manager
    session$userData$slices_global <- structure(
      reactiveVal(filter_restored),
      slices_mapping = list()
    )

    # todo: bookmark store/restore of teal_slices should be implemented here
    #       Move it from snapshot_manager_srv to here or to filter_manager
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
        filtered_data
      })
    }

    active_module <- srv_teal_module(
      id = "root_module",
      data_rv = data_rv,
      datasets = datasets_rv,
      modules = modules,
      filter = filter_restored
    )

    # todo: make a module containing for this observer and for an icon on the UI side?
    observeEvent(input$filter_manager, {
      showModal(
        modalDialog(
          tags$div(
            filter_manager_ui(session$ns("filter_manager"))
          )
        )
      )
    })
    filter_manager_srv("filter_manager", is_module_specific = isTRUE(attr(filter, "module_specific")))

    # todo: connect snapshot manager with slices_global
    observeEvent(input$snapshot_manager, {
      print("snapshot_manager clicked!")
    })

    # todo: bring back bookmark manager
    observeEvent(input$bookmark_manager, {
      print("bookmark_manager clicked!")
    })
  })
}
