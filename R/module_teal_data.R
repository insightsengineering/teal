#' Execute and validate `teal_data_module`
#'
#' This is a low level module to handle `teal_data_module` execution and validation.
#' [teal_transform_module()] inherits from [teal_data_module()] so it is handled by this module too.
#' [srv_teal()] accepts various `data` objects and eventually they are all transformed to `reactive`
#' [teal.data::teal_data()] which is a standard data class in whole `teal` framework.
#'
#' @section data validation:
#'
#' Executed [teal_data_module()] is validated and output is validated for consistency.
#' Output `data` is invalid if:
#' 1. [teal_data_module()] is invalid if server doesn't return `reactive`. **Immediately crashes an app!**
#' 2. `reactive` throws a `shiny.error` - happens when module creating [teal.data::teal_data()] fails.
#' 3. `reactive` returns `qenv.error` - happens when [teal.data::teal_data()] evaluates a failing code.
#' 4. `reactive` object doesn't return [teal.data::teal_data()].
#' 5. [teal.data::teal_data()] object lacks any `datanames` specified in the `modules` argument.
#'
#' `teal` (observers in `srv_teal`) always waits to render an app until `reactive` `teal_data` is
#' returned. If error 2-4 occurs, relevant error message is displayed to the app user. Once the issue is
#' resolved, the app will continue to run. `teal` guarantees that errors in data don't crash the app
#' (except error 1).
#'
#' @inheritParams module_teal_module
#' @param data_module (`teal_data_module`)
#' @param datanames_required (`named list`) list of datanames required by modules. List element per module, named
#'  after module's label.
#' @param validate_shiny_silent_error (`logical`) If `TRUE`, then `shiny.silent.error` is validated and
#' @param is_transform_failed (`reactiveValues`) contains `logical` flags named after each transformator.
#' Help to determine if any previous transformator failed, so that following transformators can be disabled
#' and display a generic failure message.
#'
#' @return `reactive` `teal_data`
#'
#' @rdname module_teal_data
#' @name module_teal_data
#' @keywords internal
NULL

#' @keywords internal
ui_check_teal_data <- function(id) {
  ns <- NS(id)
  uiOutput(ns("message"))
}

#' @keywords internal
srv_check_teal_data <- function(id, data, validate_shiny_silent_error) {
  checkmate::assert_string(id)
  moduleServer(id, function(input, output, session) {
    data_handled <- reactive({
      d <- tryCatch(data(), error = function(e) e)
      if (inherits(d, "shiny.silent.error") && identical(d$message, "")) {
        if (validate_shiny_silent_error) {
          simpleError(message = "Error when evaluating a module. Please contact an app developer if error persist")
        } else {
          d
        }
      } else if (inherits(d, "error")) {
        simpleError(
          message = sprintf(
            "Error when evaluating a module:\n\n %s \n\nPlease contact an app developer if error persist",
            trimws(paste(d$message, collapse = " "))
          )
        )
      } else if (!inherits(d, "teal_data")) {
        simpleError(message = "Did not receive `teal_data` object. Cannot proceed further.")
      } else {
        d
      }
    })
    output$message <- renderUI({
      validate(need(!inherits(data_handled(), "error"), data_handled()$message))
    })
    data_handled
  })
}

#' @keywords internal
ui_check_required_datanames <- function(id) {
  ns <- NS(id)
  uiOutput(NS(id, "message"))
}

#' @keywords internal
srv_check_required_datanames <- function(id, data, datanames_required = list(), show_modules_info = FALSE) {
  checkmate::assert_string(id)
  checkmate::assert_class(data, "reactive")
  checkmate::assert_list(datanames_required, types = c("character", "NULL"))
  moduleServer(id, function(input, output, session) {
    output$message <- renderUI({
      if (inherits(data(), "teal_data")) {
        is_datanames_ok <- check_required_datanames_html(
          datanames_required = datanames_required,
          datanames_available = names(data()),
          show_modules_info = show_modules_info
        )
        if (!isTRUE(is_datanames_ok)) {
          tags$div(is_datanames_ok, class = "teal-output-warning")
        }
      }
    })
    data
  })
}

.trigger_on_success <- function(data) {
  out <- reactiveVal(NULL)
  observeEvent(data(), {
    if (inherits(data(), "teal_data")) {
      if (!identical(data(), out())) {
        out(data())
      }
    }
  })

  out
}
