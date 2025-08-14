#' `teal` main module
#'
#' @description
#' Module to create a `teal` app as a Shiny Module.
#'
#' @details
#' This module can be used instead of [init()] in custom Shiny applications. Unlike [init()], it doesn't
#' automatically include [`module_session_info`].
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
#' ## Fallback on failure
#'
#' `teal` is designed in such way that app will never crash if the error is introduced in any
#' custom `shiny` module provided by app developer (e.g. [teal_data_module()], [teal_transform_module()]).
#' If any module returns a failing object, the app will halt the evaluation and display a warning message.
#' App user should always have a chance to fix the improper input and continue without restarting the session.
#'
#' @rdname module_teal
#' @name module_teal
#'
#' @inheritParams init
#' @param id (`character(1)`) `shiny` module instance id.
#' @param data (`teal_data`, `teal_data_module`, or `reactive` returning `teal_data`)
#' The data which application will depend on.
#' @param modules (`teal_modules`)
#'   `teal_modules` object. These are the specific output modules which
#'   will be displayed in the `teal` application. See [modules()] and [module()] for
#'   more details.
#'
#' @return `NULL` invisibly
NULL

#' @rdname module_teal
#' @export
ui_teal <- function(id, modules) {
  checkmate::assert_character(id, max.len = 1, any.missing = FALSE)
  checkmate::assert_class(modules, "teal_modules")
  ns <- NS(id)

  mod <- extract_module(modules, class = "teal_module_previewer")
  reporter_opts <- if (length(mod)) .get_reporter_options(mod[[1]]$server_args)
  modules <- drop_module(modules, "teal_module_landing")
  modules <- drop_module(modules, "teal_module_previewer")

  shiny_busy_message_panel <- conditionalPanel(
    condition = "(($('html').hasClass('shiny-busy')) && (document.getElementById('shiny-notification-panel') == null))", # nolint: line_length.
    tags$div(
      icon("arrows-rotate", class = "fa-spin", prefer_type = "solid"),
      "Computing ...",
      style = "position: fixed; bottom: 0; right: 0;
          width: 140px; margin: 15px; padding: 5px 0 5px 10px;
          text-align: left; font-weight: bold; font-size: 100%;
          color: #ffffff; background-color: #347ab7; z-index: 105;"
    )
  )

  navbar <- ui_teal_module(id = ns("teal_modules"), modules = modules)
  module_items <- ui_teal_module(id = ns("teal_modules"), modules = modules)
  nav_elements <- list(
    withr::with_options(reporter_opts, { # for backwards compatibility of the report_previewer_module$server_args
      shinyjs::hidden(
        tags$div(
          id = ns("reporter_menu_container"),
          .teal_navbar_menu(
            label = "Report",
            icon = "file-text-fill",
            class = "reporter-menu",
            if ("preview" %in% getOption("teal.reporter.nav_buttons")) {
              teal.reporter::preview_report_button_ui(ns("preview_report"), label = "Preview Report")
            },
            tags$hr(style = "margin: 0.5rem;"),
            if ("download" %in% getOption("teal.reporter.nav_buttons")) {
              teal.reporter::download_report_button_ui(ns("download_report"), label = "Download Report")
            },
            if ("load" %in% getOption("teal.reporter.nav_buttons")) {
              teal.reporter::report_load_ui(ns("load_report"), label = "Load Report")
            },
            tags$hr(style = "margin: 0.5rem;"),
            if ("reset" %in% getOption("teal.reporter.nav_buttons")) {
              teal.reporter::reset_report_button_ui(ns("reset_reports"), label = "Reset Report")
            }
          )
        )
      )
    }),
    tags$span(style = "margin-left: auto;"),
    ui_bookmark_panel(ns("bookmark_manager"), modules),
    ui_snapshot_manager_panel(ns("snapshot_manager_panel")),
    ui_filter_manager_panel(ns("filter_manager_panel"))
  )
  navbar <- .teal_navbar_append(navbar, nav_elements)

  bslib::page_fluid(
    id = id,
    theme = get_teal_bs_theme(),
    include_teal_css_js(),
    shinyjs::useShinyjs(),
    shiny_busy_message_panel,
    tags$div(id = ns("tabpanel_wrapper"), class = "teal-body", navbar),
    tags$hr(style = "margin: 1rem 0 0.5rem 0;")
  )
}

#' @rdname module_teal
#' @export
srv_teal <- function(id, data, modules, filter = teal_slices()) {
  checkmate::assert_character(id, max.len = 1, any.missing = FALSE)
  checkmate::assert_multi_class(data, c("teal_data", "teal_data_module", "reactive"))
  checkmate::assert_class(modules, "teal_modules")
  checkmate::assert_class(filter, "teal_slices")

  modules <- drop_module(modules, "teal_module_landing")
  modules <- drop_module(modules, "teal_module_previewer")

  moduleServer(id, function(input, output, session) {
    logger::log_debug("srv_teal initializing.")

    if (getOption("teal.show_js_log", default = FALSE)) {
      shinyjs::showLog()
    }

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

    data_handled <- srv_init_data("data", data = data)

    validate_ui <- tags$div(
      id = session$ns("validate_messages"),
      class = "teal_validated",
      ui_check_class_teal_data(session$ns("class_teal_data")),
      ui_validate_error(session$ns("silent_error")),
      ui_check_module_datanames(session$ns("datanames_warning"))
    )
    srv_check_class_teal_data("class_teal_data", data_handled)
    srv_validate_error("silent_error", data_handled, validate_shiny_silent_error = FALSE)
    srv_check_module_datanames("datanames_warning", data_handled, modules)

    data_validated <- .trigger_on_success(data_handled)

    data_signatured <- reactive({
      req(inherits(data_validated(), "teal_data"))
      is_filter_ok <- check_filter_datanames(filter, names(data_validated()))
      if (!isTRUE(is_filter_ok)) {
        showNotification(
          "Some filters were not applied because of incompatibility with data. Contact app developer.",
          type = "warning",
          duration = 10
        )
        warning(is_filter_ok)
      }
      .add_signature_to_data(data_validated())
    })

    data_load_status <- reactive({
      if (inherits(data_handled(), "teal_data")) {
        shinyjs::enable(id = "close_teal_data_module_modal")
        "ok"
      } else if (inherits(data, "teal_data_module")) {
        shinyjs::disable(id = "close_teal_data_module_modal")
        "teal_data_module failed"
      } else {
        "external failed"
      }
    })

    if (inherits(data, "teal_data_module")) {
      setBookmarkExclude(c("teal_data_module_ui", "open_teal_data_module_ui"))
      .teal_navbar_insert_ui(
        ui = .expand_button(
          id = session$ns("open_teal_data_module_ui"),
          label = "Load Data",
          icon = "database-fill"
        )
      )
      observeEvent(
        input$open_teal_data_module_ui,
        ignoreInit = TRUE,
        ignoreNULL = FALSE, # should be shown on startup
        {
          showModal(
            div(
              class = "teal teal-data-module-popup",
              modalDialog(
                id = session$ns("teal_data_module_ui"),
                size = "xl",
                tags$div(
                  ui_init_data(session$ns("data")),
                  validate_ui
                ),
                easyClose = FALSE,
                footer = tags$div(id = session$ns("close_teal_data_module_modal"), modalButton("Dismiss"))
              )
            )
          )
          if (data_load_status() == "ok") {
            shinyjs::enable(id = "close_teal_data_module_modal")
          } else {
            shinyjs::disable(id = "close_teal_data_module_modal")
          }
        }
      )

      if (isTRUE(attr(data, "once"))) {
        # when once = TRUE we pull data once and then remove data button and a modal
        shiny::removeUI(selector = sprintf(".teal.expand-button:has(#%s)", session$ns("open_teal_data_module_ui")))
        observeEvent(data_signatured(), once = TRUE, {
          logger::log_debug("srv_teal@2 removing data tab.")
          shiny::removeModal()
        })
      }
    } else {
      # when no teal_data_module then we want to display messages above tabsetPanel (because there is no data-tab)
      insertUI(
        selector = sprintf("#%s", session$ns("tabpanel_wrapper")),
        where = "beforeBegin",
        ui = tags$div(validate_ui)
      )
    }

    if (is_arg_used(modules, "reporter")) {
      shinyjs::show("reporter_menu_container")
    } else {
      removeUI(selector = sprintf("#%s", session$ns("reporter_menu_container")))
    }
    reporter <- teal.reporter::Reporter$new()$set_id(attr(filter, "app_id"))
    teal.reporter::preview_report_button_srv("preview_report", reporter)
    teal.reporter::report_load_srv("load_report", reporter)
    teal.reporter::download_report_button_srv(id = "download_report", reporter = reporter)
    teal.reporter::reset_report_button_srv("reset_reports", reporter)

    datasets_rv <- if (!isTRUE(attr(filter, "module_specific"))) {
      eventReactive(data_signatured(), {
        req(inherits(data_signatured(), "teal_data"))
        logger::log_debug("srv_teal@1 initializing FilteredData")
        teal_data_to_filtered_data(data_signatured())
      })
    }
    module_labels <- unlist(modules_slot(modules, "label"), use.names = FALSE)
    slices_global <- methods::new(".slicesGlobal", filter, module_labels)

    modules_output <- srv_teal_module(
      "teal_modules",
      data = data_signatured,
      modules = modules,
      datasets = datasets_rv,
      slices_global = slices_global,
      reporter = reporter,
      data_load_status = data_load_status
    )

    mapping_table <- srv_filter_manager_panel("filter_manager_panel", slices_global = slices_global)
    snapshots <- srv_snapshot_manager_panel("snapshot_manager_panel", slices_global = slices_global)
    srv_bookmark_panel("bookmark_manager", modules)
  })

  invisible(NULL)
}
