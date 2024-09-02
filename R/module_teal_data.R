#' Execute and validate `teal_data_module`
#'
#' This is a low level module to handle `teal_data_module` execution and validation.
#' [teal_transform_module()] inherits from [teal_data_module()] so it is handled by this module too.
#' [srv_teal()] accepts various `data` objects and eventually they are all transformed to `reactive`
#' [teal_data()] which is a standard data class in whole `teal` framework.
#'
#' @section data validation:
#'
#' Executed [teal_data_module()] is validated and output is validated for consistency.
#' Output `data` is invalid if:
#' 1. [teal_data_module()] is invalid if server doesn't return `reactive`. **Immediately crashes an app!**
#' 2. `reactive` throws a `shiny.error` - happens when module creating [teal_data()] fails.
#' 3. `reactive` returns `qenv.error` - happens when [teal_data()] evaluates a failing code.
#' 4. `reactive` object doesn't return [teal_data()].
#' 5. [teal_data()] object lacks any `datanames` specified in the `modules` argument.
#'
#' `teal` (observers in `srv_teal`) always waits to render an app until `reactive` `teal_data` is
#' returned. If error 2-4 occurs, relevant error message is displayed to app user and after issue is
#' resolved app will continue to run. `teal` guarantees that errors in a data don't crash an app
#' (except error 1). This is possible by catching the error and returning an empty `teal_data` object.
#' The error message is displayed in the app and the module will eventually get an empty `teal_data`.
#'
#'
#' @param id (`character(1)`) Module id
#' @param data (`reactive teal_data`)
#' @param data_module (`teal_data_module`)
#' @param modules (`teal_modules` or `teal_module`) For `datanames` validation purpose
#' @param validate_shiny_silent_error (`logical`) If `TRUE`, then `shiny.silent.error` is validated and
#' error message is displayed.
#' Default is `FALSE` to handle empty reactive cycle on `init`.
#'
#' @return `reactive` `teal_data`
#'
#' @rdname module_teal_data
#' @name module_teal_data
#' @keywords internal
NULL

#' @rdname module_teal_data
ui_teal_data <- function(id, data_module) {
  checkmate::assert_string(id)
  checkmate::assert_class(data_module, "teal_data_module")
  ns <- NS(id)
  shiny::tagList(
    data_module$ui(id = ns("data")),
    ui_validate_reactive_teal_data(ns("validate"))
  )
}

#' @rdname module_teal_data
srv_teal_data <- function(id,
                          data,
                          data_module,
                          modules = NULL,
                          validate_shiny_silent_error = TRUE) {
  checkmate::assert_string(id)
  checkmate::assert_class(data_module, "teal_data_module")
  checkmate::assert_multi_class(modules, c("teal_modules", "teal_module"), null.ok = TRUE)

  moduleServer(id, function(input, output, session) {
    logger::log_debug("srv_teal_data initializing.")

    data_in <- reactive({
      if (inherits(data(), "teal_data")) {
        srv_is_empty_teal_data("is_empty_teal_data", data())
      }
      data()
    })

    data_out <- if (is_arg_used(data_module$server, "data")) {
      data_module$server(id = "data", data = data_in)
    } else {
      data_module$server(id = "data")
    }

    data_validated <- srv_validate_reactive_teal_data(
      id = "validate",
      data = data_out,
      modules = modules,
      validate_shiny_silent_error = validate_shiny_silent_error
    )

    data_catch <- reactive(tryCatch(data_validated(), error = function(e) e))
    data_rv <- reactive({
      if (inherits(data_catch(), "shiny.silent.error")) {
        return(teal_data())
      }
      data_catch()
    })
  })
}

#' @rdname module_teal_data
ui_validate_reactive_teal_data <- function(id) {
  ns <- NS(id)
  div(
    class = "teal_validated",
    ui_validate_silent_error(ns("silent_error")),
    ui_validate_qenv_error(ns("qenv_error")),
    ui_check_class_teal_data(ns("class_teal_data")),
    ui_check_shiny_warnings(ns("shiny_warnings"))
  )
}

#' @rdname module_teal_data
srv_validate_reactive_teal_data <- function(id, # nolint: object_length
                                            data,
                                            modules = NULL,
                                            validate_shiny_silent_error = FALSE) {
  checkmate::assert_string(id)
  checkmate::assert_multi_class(modules, c("teal_modules", "teal_module"), null.ok = TRUE)
  checkmate::assert_flag(validate_shiny_silent_error)

  moduleServer(id, function(input, output, session) {
    data_rv <- reactive(tryCatch(data(), error = function(e) e))

    # there is an empty reactive cycle on init!
    srv_validate_silent_error("silent_error", data_rv(), validate_shiny_silent_error)

    srv_validate_qenv_error("qenv_error", data_rv())

    srv_check_class_teal_data("class_teal_data", data_rv())

    srv_check_shiny_warnings("shiny_warnings", data_rv(), modules)

    data_rv
  })
}

#' @keywords internal
ui_validate_silent_error <- function(id) {
  ns <- NS(id)
  uiOutput(ns("error"))
}

#' @keywords internal
srv_validate_silent_error <- function(id, data, validate_shiny_silent_error) {
  checkmate::assert_string(id)
  checkmate::assert_flag(validate_shiny_silent_error)
  moduleServer(id, function(input, output, session) {
    output$error <- renderUI({
      if (inherits(data, "shiny.silent.error") && identical(data$message, "")) {
        if (!validate_shiny_silent_error) {
          return(teal_data())
        } else {
          validate(
            need(
              FALSE,
              paste(
                "Shiny error when executing the `data` module",
                "\nCheck your inputs or contact app developer if error persists.",
                collapse = "\n"
              )
            )
          )
        }
      }
    })
  })
}

#' @keywords internal
ui_validate_qenv_error <- function(id) {
  ns <- NS(id)
  uiOutput(ns("error"))
}

#' @keywords internal
srv_validate_qenv_error <- function(id, data) {
  checkmate::assert_string(id)
  moduleServer(id, function(input, output, session) {
    output$error <- renderUI({
      if (inherits(data, c("qenv.error"))) {
        validate(
          need(
            FALSE,
            paste(
              "Error when executing the `data` module:",
              strip_style(paste(data$message, collapse = "\n")),
              "\nCheck your inputs or contact app developer if error persists.",
              collapse = "\n"
            )
          )
        )
      }
    })
  })
}

#' @keywords internal
ui_check_class_teal_data <- function(id) {
  ns <- NS(id)
  uiOutput(ns("check"))
}

#' @keywords internal
srv_check_class_teal_data <- function(id, data) {
  checkmate::assert_string(id)
  moduleServer(id, function(input, output, session) {
    output$check <- renderUI({
      validate(
        need(
          checkmate::test_class(data, "teal_data"),
          "Did not recieve a valid `teal_data` object. Cannot proceed further."
        )
      )
    })
  })
}

#' @keywords internal
ui_is_empty_teal_data <- function(id) {
  ns <- NS(id)
  uiOutput(ns("is_empty"))
}

#' @keywords internal
srv_is_empty_teal_data <- function(id, data) {
  checkmate::assert_string(id)
  moduleServer(id, function(input, output, session) {
    output$is_empty <- renderUI({
      if (inherits(data, "teal_data")) {
        validate(
          need(
            !.is_empty_teal_data(data),
            "Empty `teal_data` object."
          )
        )
      }
    })
  })
}

#' @keywords internal
ui_check_shiny_warnings <- function(id) {
  ns <- NS(id)
  uiOutput(NS(id, "warnings"))
}

#' @keywords internal
srv_check_shiny_warnings <- function(id, data, modules) {
  checkmate::assert_string(id)
  moduleServer(id, function(input, output, session) {
    output$warnings <- renderUI({
      if (inherits(data, "teal_data")) {
        is_modules_ok <- check_modules_datanames(modules = modules, datanames = .teal_data_ls(data))
        if (!isTRUE(is_modules_ok)) {
          tags$div(
            class = "teal-output-warning",
            is_modules_ok$html(
              # Show modules prefix on message only in teal_data_module tab
              grepl(sprintf("data-teal_data_module-%s", id), session$ns(NULL), fixed = TRUE)
            )
          )
        }
      }
    })
  })
}
