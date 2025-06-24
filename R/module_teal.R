#' `teal` main module
#'
#' @description
#' `r lifecycle::badge("stable")`
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

  uiOutput(ns("main_teal_ui"))
}

#' @rdname module_teal
#' @export
srv_teal <- function(id, data, modules, filter = teal_slices()) {
  checkmate::assert_character(id, max.len = 1, any.missing = FALSE)
  checkmate::assert_multi_class(data, c("teal_data", "teal_data_module", "reactive"))
  checkmate::assert_class(modules, "teal_modules")
  checkmate::assert_class(filter, "teal_slices")

  modules <- drop_module(modules, "teal_module_landing")
  modules <- append_reporter_module(modules)

  moduleServer(id, function(input, output, session) {
    logger::log_debug("srv_teal initializing.")

    if (getOption("teal.show_js_log", default = FALSE)) {
      shinyjs::showLog()
    }

    output$main_teal_ui <- renderUI({
      modules <- drop_module(modules, "teal_module_landing")
      modules <- append_reporter_module(modules)

      # show busy icon when `shiny` session is busy computing stuff
      # based on https://stackoverflow.com/questions/17325521/r-shiny-display-loading-message-while-function-is-running/22475216#22475216 # nolint: line_length.
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
      bslib::page_fluid(
        id = id,
        theme = get_teal_bs_theme(),
        include_teal_css_js(),
        shiny_busy_message_panel,
        tags$div(
          id = session$ns("options_buttons"),
          style = "position: absolute; right: 10px;",
          ui_bookmark_panel(session$ns("bookmark_manager"), modules),
          ui_snapshot_manager_panel(session$ns("snapshot_manager_panel")),
          ui_filter_manager_panel(session$ns("filter_manager_panel"))
        ),
        uiOutput(session$ns("teal_data_module_ui")),
        tags$div(
          id = session$ns("tabpanel_wrapper"),
          class = "teal-body",
          ui_teal_module(id = session$ns("teal_modules"), modules = modules)
        ),
        tags$script(
          HTML(
            sprintf(
              "
            $(document).ready(function() {
              $('#%s').appendTo('#%s');
            });
          ",
              session$ns("options_buttons"),
              session$ns("teal_modules-active_tab")
            )
          )
        ),
        tags$hr(style = "margin: 1rem 0 0.5rem 0;")
      )
    })

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
        "ok"
      } else if (inherits(data, "teal_data_module")) {
        "teal_data_module failed"
      } else {
        "external failed"
      }
    })

    datasets_rv <- if (!isTRUE(attr(filter, "module_specific"))) {
      eventReactive(data_signatured(), {
        req(inherits(data_signatured(), "teal_data"))
        logger::log_debug("srv_teal@1 initializing FilteredData")
        teal_data_to_filtered_data(data_signatured())
      })
    }


    if (inherits(data, "teal_data_module")) {
      observeEvent(data_handled(), {
        if (inherits(data_handled(), "teal_data")) {
          showNotification("Data updated")
          .enable_sidebar_overlay_close(session$ns("teal_data_module_sidebar"))
          .glow_sidebar_overlay_close(session$ns("teal_data_module_sidebar"))
        }
      })
      setBookmarkExclude(c("teal_modules-active_tab"))
      insertUI(
        selector = ".teal-modules-wrapper .nav-item-custom",
        where = "beforeBegin",
        .sidebar_overlay(
          id = session$ns("teal_data_module_sidebar"),
          toggle_ui = actionButton(session$ns("show_teal_data_module"), NULL, icon = icon("database")),
          ui_content = tags$div(
            ui_init_data(session$ns("data")),
            validate_ui
          ),
          shown = TRUE
        )
      )

      if (attr(data, "once")) {
        observeEvent(data_signatured(), once = TRUE, {
          logger::log_debug("srv_teal@2 removing data tab.")
          # when once = TRUE we pull data once and then remove data tab
          .hide_sidebar_overlay(session$ns("teal_data_module_sidebar"))
          removeUI(sprintf("#%s", session$ns("show_teal_data_module")))
        })
      }
    } else {
      # when no teal_data_module then we want to display messages above tabsetPanel (because there is no data-tab)
      insertUI(
        selector = sprintf("#%s", session$ns("tabpanel_wrapper")),
        where = "beforeBegin",
        ui = tags$div(validate_ui, tags$br())
      )
    }

    reporter <- teal.reporter::Reporter$new()$set_id(attr(filter, "app_id"))
    module_labels <- unlist(module_labels(modules), use.names = FALSE)
    slices_global <- methods::new(".slicesGlobal", filter, module_labels)
    modules_output <- srv_teal_module(
      id = "teal_modules",
      data = data_signatured,
      datasets = datasets_rv,
      modules = modules,
      slices_global = slices_global,
      data_load_status = data_load_status,
      reporter = reporter
    )
    mapping_table <- srv_filter_manager_panel("filter_manager_panel", slices_global = slices_global)
    snapshots <- srv_snapshot_manager_panel("snapshot_manager_panel", slices_global = slices_global)
    srv_bookmark_panel("bookmark_manager", modules)
  })

  invisible(NULL)
}


#' @keywords internal
.sidebar_overlay_deps <- function() {
  htmltools::htmlDependency(
    name = "sidebar-navigation",
    version = utils::packageVersion("teal"),
    package = "teal",
    src = "sidebar-navigation",
    stylesheet = "sidebar-navigation.css"
  )
}

#' Create Sidebar Overlay Widget
#'
#' Generates a slide-out sidebar overlay that can be toggled open and closed.
#' The sidebar slides in from the left or right side of the screen and contains
#' custom UI content with a close button.
#'
#' @details
#' This function creates a complete sidebar overlay system with the following components:
#' - A toggle ui (provided as input) that opens the sidebar when clicked
#' - A sidebar panel that slides in from the specified direction
#' - A close button (X) within the sidebar to hide it
#' - Automatic CSS class management for show/hide animations
#' - Required CSS dependencies for styling and animations
#'
#' The sidebar uses CSS classes to control visibility and animations:
#' - `teal-custom-sidebar`: Base styling for the sidebar
#' - `show`: Controls visibility (added/removed to show/hide)
#' - `left`/`right`: Controls which side the sidebar slides from
#'
#' JavaScript click handlers are attached inline to manage the show/hide behavior
#' without requiring additional server-side logic.
#'
#' @param id (`character(1)`) Unique identifier for the sidebar overlay element.
#'   Used to target the sidebar for programmatic show/hide operations.
#'   This id can later be used to perform actions on the sidebar with the help of the helper functions:
#'   - `.hide_sidebar_overlay(id)`
#'   - `.show_sidebar_overlay(id)`
#'   - `.glow_sidebar_overlay_close(id)`
#'   - `.enable_sidebar_overlay_close(id)`
#' @param toggle_ui (`shiny.tag`) UI element that will trigger the sidebar to open
#'   when clicked. Typically an `actionButton` or similar interactive element.
#' @param ui_content (`shiny.tag` or `tagList`) The content to display inside the
#'   sidebar panel. Can be any valid Shiny UI elements.
#' @param shown (`logical(1)`) Whether the sidebar should be initially visible.
#'   Defaults to `FALSE` (hidden).
#' @param direction (`character(1)`) Side of the screen from which the sidebar slides.
#'   Must be either `"left"` or `"right"`. Defaults to `"left"`.
#' @keywords internal
.sidebar_overlay <- function(id, toggle_ui, ui_content, shown = FALSE, direction = "left") {
  if (!direction %in% c("left", "right")) {
    stop("direction must be either 'left' or 'right'")
  }

  sidebar_classes <- c("teal-custom-sidebar", direction)
  if (shown) sidebar_classes <- c(sidebar_classes, "show")

  tags$div(
    .sidebar_overlay_deps(),
    tags$div(
      toggle_ui,
      onclick = 'this.nextElementSibling.classList.add("show");'
    ),
    div(
      id = id,
      class = paste(sidebar_classes, collapse = " "),
      div(
        class = "sidebar-content",
        shinyjs::disabled(
          tags$button(
            bsicons::bs_icon("x"),
            class = "btn close-button",
            onclick = 'this.parentElement.parentElement.classList.remove("show");'
          )
        ),
        ui_content
      )
    )
  )
}

#' @keywords internal
.hide_sidebar_overlay <- function(id) {
  shinyjs::runjs(
    sprintf("document.getElementById('%s').classList.remove('show')", id)
  )
}

#' @keywords internal
.show_sidebar_overlay <- function(id) {
  shinyjs::runjs(
    sprintf("document.getElementById('%s').classList.add('show')", id)
  )
}

#' @keywords internal
.glow_sidebar_overlay_close <- function(id) {
  shinyjs::runjs(
    sprintf(
      "document.getElementById('%s').querySelector('.close-button').classList.remove('glow');
      requestAnimationFrame(() => {
        document.getElementById('%s').querySelector('.close-button').classList.add('glow');
      });",
      id, id
    )
  )
}

#' @keywords internal
.enable_sidebar_overlay_close <- function(id) {
  shinyjs::enable(selector = sprintf("#%s .close-button", id))
}
