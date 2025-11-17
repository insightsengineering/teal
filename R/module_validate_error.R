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
#' @inheritParams module_teal
#' @param modules (`teal_modules` or `teal_module`) For `datanames` validation purpose
#' @param validate_shiny_silent_error (`logical`) If `TRUE`, then `shiny.silent.error` is validated and
#'
#' @return `reactive` `teal_data`
#'
#' @rdname module_validate_error
#' @name module_validate_error
#' @keywords internal
NULL

#' @rdname module_validate_error
#' @keywords internal
ui_validate_error <- function(id) {
  ns <- NS(id)
  uiOutput(ns("message"))
}

#' @rdname module_validate_error
#' @keywords internal
srv_validate_error <- function(id, data, validate_shiny_silent_error) {
  checkmate::assert_string(id)
  checkmate::assert_flag(validate_shiny_silent_error)
  moduleServer(id, function(input, output, session) {
    output$message <- renderUI({
      is_shiny_silent_error <- inherits(data(), "shiny.silent.error") && identical(data()$message, "")
      if (inherits(data(), "qenv.error")) {
        validate(
          need(
            FALSE,
            paste(
              "Error when executing the `data` module:",
              cli::ansi_strip(paste(data()$message, collapse = "\n")),
              "\nCheck your inputs or contact app developer if error persists.",
              collapse = "\n"
            )
          )
        )
      } else if (inherits(data(), "error")) {
        if (is_shiny_silent_error && !validate_shiny_silent_error) {
          return(NULL)
        }
        validate(
          need(
            FALSE,
            sprintf(
              "Shiny error when executing the `data` module.\n%s\n%s",
              data()$message,
              "Check your inputs or contact app developer if error persists."
            )
          )
        )
      }
    })
  })
}

#' @rdname module_validate_error
#' @keywords internal
ui_check_class_teal_data <- function(id) {
  ns <- NS(id)
  uiOutput(ns("message"))
}

#' @rdname module_validate_error
#' @keywords internal
srv_check_class_teal_data <- function(id, data) {
  checkmate::assert_string(id)
  moduleServer(id, function(input, output, session) {
    output$message <- renderUI({
      validate(
        need(
          inherits(data(), c("teal_data", "error")),
          "Did not receive `teal_data` object. Cannot proceed further."
        )
      )
    })
  })
}

#' @rdname module_validate_error
#' @keywords internal
ui_check_module_datanames <- function(id) {
  ns <- NS(id)
  uiOutput(NS(id, "message"))
}

#' @rdname module_validate_error
#' @keywords internal
srv_check_module_datanames <- function(id, data, modules) {
  checkmate::assert_string(id)
  moduleServer(id, function(input, output, session) {
    output$message <- renderUI({
      if (inherits(data(), "teal_data")) {
        is_modules_ok <- check_modules_datanames_html(
          modules = modules, datanames = names(data())
        )
        if (!isTRUE(is_modules_ok)) {
          tags$div(is_modules_ok, class = "teal-output-warning")
        }
      }
    })
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
