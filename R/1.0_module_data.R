#' Data module for teal
#'
#' Fundamental data class for teal is [teal.data::teal_data()]. Data can be
#' passed in multiple ways:
#' 1. Directly as a [teal.data::teal_data()] object.
#' 2. As a `reactive` object returning [teal.data::teal_data()]. [See section](#reactive-teal_data).
#'
#' @section Reactive `teal_data`:
#'
#' [teal.data::teal_data()] can change depending on the reactive context and `srv_teal` will rebuild
#' the app accordingly. There are two ways of interacting with the data:
#' 1. Using a `reactive` object passed from outside the `teal` application. In this case, reactivity
#' is controlled by external module and `srv_teal` will trigger accordingly to the changes.
#' 2. Using [teal_data_module()] which is embedded in the `teal` application and data can be
#' resubmitted when needed by the user.
#'
#' Since server of [teal_data_module()] must return `reactive` `teal_data` object, it means that
#' both scenarios (1) and (2) are having the same effect for the reactivity of a `teal` application.
#' The difference is that in the first case the data is controlled from outside the app and in the
#' second case the data is controlled from custom module called inside of the app.
#'
#' see [`validate_reactive_teal_data`] for more details.
#'
#' @inheritParams init
#'
#' @param data (`teal_data`, `teal_data_module` or `reactive` returning `teal_data`)
#' @return A `reactiveVal` which is set to:
#' - `teal_data` when the object is validated
#' - `NULL` when not validated.
#' Important: `srv_data` suppress validate messages and returns `NULL` so that `srv_teal` can
#' stop the reactive cycle as `observeEvent` calls based on the data have `ignoreNULL = TRUE`.
#'
#' @rdname module_data
#' @name module_data
NULL

#' @rdname module_data
#' @keywords internal
ui_data <- function(id, data, title, header, footer) {
  ns <- shiny::NS(id)
  shiny::div(
    style = "display: inline-block;",
    if (inherits(data, "teal_data_module")) {
      actionButton(ns("open_teal_data_module"), NULL, icon = icon("database"))
    } else {
      NULL
    }
  )
}

#' @rdname module_data
#' @keywords internal
srv_data <- function(id, data, modules, filter = teal_slices()) {
  checkmate::assert_character(id, max.len = 1, any.missing = FALSE)
  checkmate::assert_multi_class(data, c("teal_data", "teal_data_module", "reactive", "reactiveVal"))
  checkmate::assert_class(modules, "teal_modules")
  checkmate::assert_class(filter, "teal_slices")

  moduleServer(id, function(input, output, session) {
    logger::log_trace("srv_data initializing.")

    if (getOption("teal.show_js_log", default = FALSE)) {
      shinyjs::showLog()
    }

    # data_rv contains teal_data object
    # either passed to teal::init or returned from teal_data_module
    data_validated <- if (inherits(data, "teal_data_module")) {
      srv_teal_data_module(
        "teal_data_module",
        data = reactive(NULL),
        transformer = data,
        modules = modules,
        validate_shiny_silent_error = FALSE
      )
    } else if (inherits(data, "teal_data")) {
      reactiveVal(data)
    } else if (inherits(data, c("reactive", "reactiveVal"))) {
      data
    }

    setBookmarkExclude("open_teal_data_module")

    observeEvent(input$open_teal_data_module, {
      if (input$open_teal_data_module > 1) {
        footer <- modalButton("Dismiss")
        easy_close <- TRUE
      } else {
        footer <- NULL
        easy_close <- FALSE
      }
      showModal(
        modalDialog(
          class = ifelse(easy_close, "blur_background", "hide_background"),
          tags$div(
            ui_teal_data_module(session$ns("teal_data_module"), transformer = data)
          ),
          footer = footer,
          easyClose = easy_close
        )
      )
    })

    if (inherits(data, "teal_data_module")) {
      shinyjs::disable(selector = "#teal_modules-active_tab.nav-tabs a")
      shinyjs::click(id = "open_teal_data_module")
    }

    data_rv <- reactiveVal(NULL)
    observeEvent(data_validated(), {
      showNotification("Data loaded successfully.", duration = 5)
      shinyjs::enable(selector = "#teal_modules-active_tab.nav-tabs a")
      data_rv(data_validated())
    })

    observeEvent(data_validated(), once = TRUE, {
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

    data_rv
  })
}
