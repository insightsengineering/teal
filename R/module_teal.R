#' `teal` main module
#'
#' @description
#' `r lifecycle::badge("stable")`
#' Module to create a `teal` app. This module can be called directly instead of [init()] and
#' included in your custom application. Please note that [init()] adds `reporter_previewer_module`
#' automatically, which is not a case when calling `ui/srv_teal` directly.
#'
#' @details
#'
#' Module is responsible for creating the main `shiny` app layout and initializing all the necessary
#' components. This module establishes reactive connection between the input `data` and every other
#' component in the app. Reactive change of the `data` passed as an argument, reloads the app and
#' possibly keeps all input settings the same so the user can continue where one left off.
#'
#' ## data flow in `teal` application
#'
#' This module supports multiple data inputs but eventually, they are all converted to `reactive`
#' returning `teal_data` in this module. On this `reactive teal_data` object several actions are
#' performed:
#' - data loading in [`module_init_data`]
#' - data filtering in [`module_filter_data`]
#' - data transformation in [`module_transform_data`]
#'
#' @rdname module_teal
#' @name module_teal
#'
#' @inheritParams module_init_data
#' @inheritParams init
#'
#' @return `NULL` invisibly
NULL

#' @rdname module_teal
#' @export
ui_teal <- function(id,
                    modules,
                    data = NULL,
                    title = build_app_title(),
                    header = tags$p(),
                    footer = tags$p()) {
  checkmate::assert_character(id, max.len = 1, any.missing = FALSE)
  checkmate::assert_multi_class(data, "teal_data_module", null.ok = TRUE)
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
      icon("arrows-rotate", class = "fa-spin", prefer_type = "solid"),
      "Computing ...",
      # CSS defined in `custom.css`
      class = "shinybusymessage"
    )
  )

  bookmark_panel_ui <- ui_bookmark_panel(ns("bookmark_manager"), modules)
  if (!is.null(data)) {
    data_module <- ui_init_data(ns("data"), data = data)
    modules$children <- c(list(teal_data_module = data_module), modules$children)
  }
  tabs_elem <- ui_teal_module(id = ns("teal_modules"), modules = modules)

  fluidPage(
    id = id,
    title = title,
    theme = get_teal_bs_theme(),
    include_teal_css_js(),
    tags$header(header),
    tags$hr(class = "my-2"),
    shiny_busy_message_panel,
    ui_validate_reactive_teal_data(ns("validate")), # to display validation of the input data
    tags$div(
      id = ns("tabpanel_wrapper"),
      class = "teal-body",
      tabs_elem
    ),
    tags$div(
      id = ns("options_buttons"),
      style = "position: absolute; right: 10px;",
      bookmark_panel_ui,
      tags$button(
        class = "btn action-button filter_hamburger", # see sidebar.css for style filter_hamburger
        href = "javascript:void(0)",
        onclick = sprintf("toggleFilterPanel('%s');", ns("tabpanel_wrapper")),
        title = "Toggle filter panel",
        icon("fas fa-bars")
      ),
      ui_snapshot_manager_panel(ns("snapshot_manager_panel")),
      ui_filter_manager_panel(ns("filter_manager_panel"))
    ),
    tags$script(
      HTML(
        sprintf(
          "
            $(document).ready(function() {
              $('#%s').appendTo('#%s');
            });
          ",
          ns("options_buttons"),
          ns("teal_modules-active_tab")
        )
      )
    ),
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

#' @rdname module_teal
#' @export
srv_teal <- function(id, data, modules, filter = teal_slices()) {
  checkmate::assert_character(id, max.len = 1, any.missing = FALSE)
  checkmate::assert_multi_class(data, c("teal_data", "teal_data_module", "reactive"))
  checkmate::assert_class(modules, "teal_modules")
  checkmate::assert_class(filter, "teal_slices")

  moduleServer(id, function(input, output, session) {
    logger::log_debug("srv_teal initializing.")

    output$identifier <- renderText(
      paste0("Pid:", Sys.getpid(), " Token:", substr(session$token, 25, 32))
    )

    teal.widgets::verbatim_popup_srv(
      "sessionInfo",
      verbatim_content = utils::capture.output(utils::sessionInfo()),
      title = "SessionInfo"
    )

    output$lockFile <- teal_lockfile_downloadhandler()

    if (getOption("teal.show_js_log", default = FALSE)) {
      shinyjs::showLog()
    }

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
        logger::log_debug("srv_teal@1 Timezone set to client's timezone: { input$timezone }.")
      }
    )

    data_pulled <- srv_init_data("data", data = data)
    data_validated <- srv_validate_reactive_teal_data(
      "validate",
      data = data_pulled,
      modules = modules,
      validate_shiny_silent_error = FALSE
    )
    is_failed <- reactive(!inherits(data_pulled(), "teal_data"))
    if (inherits(data, "teal_data_module")) {
      observeEvent(is_failed(), {
        if (is_failed()) {
          shinyjs::disable(selector = ".nav li a:not(.active)")
        } else {
          showNotification("Data loaded successfully.", duration = 5)
          shinyjs::enable(selector = ".nav li a:not(.active)")
        }
      })
    } else {
      observeEvent(is_failed(), {
        if (is_failed()) {
          shinyjs::hide("tabpanel_wrapper")
        } else {
          showNotification("Data loaded successfully.", duration = 5)
          shinyjs::show("tabpanel_wrapper")
        }
      })
    }

    observeEvent(data_validated(), once = TRUE, {
      if (isTRUE(attr(data, "once"))) {
        # Hiding the data module tab.
        shinyjs::hide(selector = sprintf("#%s a[data-value='teal_data_module']", session$ns("tabpanel_wrapper")))
        # Clicking the second tab, which is the first module.
        shinyjs::runjs(
          sprintf(
            "document.querySelector('#%s .nav li:nth-child(2) a').click();",
            session$ns("tabpanel_wrapper")
          )
        )
      }

      # Excluding the ids from teal_data_module using full namespace and global shiny app session.
      app_session <- .subset2(shiny::getDefaultReactiveDomain(), "parent")
      setBookmarkExclude(
        session$ns(
          grep(
            pattern = "teal_data_module-",
            x = names(reactiveValuesToList(input)),
            value = TRUE
          )
        ),
        session = app_session
      )
    })

    data_init <- reactive({
      if (inherits(data_validated(), "teal_data")) {
        .add_signature_to_data(data_validated())
      } else {
        data_validated()
      }
    })

    datasets_rv <- if (!isTRUE(attr(filter, "module_specific"))) {
      eventReactive(data_init(), {
        req(inherits(data_init(), "teal_data"))
        logger::log_debug("srv_teal@1 initializing FilteredData")
        teal_data_to_filtered_data(data_init())
      })
    }


    module_labels <- unlist(module_labels(modules), use.names = FALSE)
    slices_global <- methods::new(".slicesGlobal", filter, module_labels)
    modules_output <- srv_teal_module(
      id = "teal_modules",
      data_rv = data_init,
      datasets = datasets_rv,
      modules = modules,
      slices_global = slices_global
    )
    mapping_table <- srv_filter_manager_panel("filter_manager_panel", slices_global = slices_global)
    snapshots <- srv_snapshot_manager_panel("snapshot_manager_panel", slices_global = slices_global)
    srv_bookmark_panel("bookmark_manager", modules)

    if (inherits(data, "teal_data_module")) {
      setBookmarkExclude(c("teal_modules-active_tab"))
    }
  })

  invisible(NULL)
}
