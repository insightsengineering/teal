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
#' (except error 1). This is possible thanks to `.fallback_on_failure` which returns input-data
#' when output-data fails
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

    validated_data <- reactive(tryCatch(data(), error = function(e) e))

    final_data <- reactive({
      data <- validated_data()
      if (inherits(data, "shiny.silent.error")) {
        return(teal_data())
      }
      data
    })

    data_out <- tryCatch(
      {
        if (is_arg_used(data_module$server, "data")) {
          data_module$server(id = "data", data = final_data)
        } else {
          data_module$server(id = "data")
        }
      },
      error = function(e) e
    )

    data_out_rv <- reactive({
      data <- data_out()
    })

    data <- srv_validate_reactive_teal_data(
      id = "validate",
      data = data_out_rv,
      modules = modules,
      validate_shiny_silent_error = validate_shiny_silent_error
    )

    data_catch <- reactive(tryCatch(data(), error = function(e) e))
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
  tagList(
    uiOutput(NS(id, "shiny_errors")),
    uiOutput(NS(id, "shiny_warnings"))
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
    data_out_r <- reactive(tryCatch(data(), error = function(e) e))

    data_validated <- reactive({
      # custom module can return error
      data_out <- data_out_r()

      # there is an empty reactive cycle on init!
      if (inherits(data_out, "shiny.silent.error") && identical(data_out$message, "")) {
        if (!validate_shiny_silent_error) {
          return(teal_data())
        } else {
          validate(
            need(
              FALSE,
              paste(
                "Shiny error when executing the `data` module",
                "Check your inputs or contact app developer if error persists.",
                collapse = "\n"
              )
            )
          )
        }
      }

      # to handle errors and qenv.error(s)
      if (inherits(data_out, c("qenv.error", "error"))) {
        validate(
          need(
            FALSE,
            paste(
              "Error when executing the `data` module:",
              strip_style(paste(data_out$message, collapse = "\n")),
              "Check your inputs or contact app developer if error persists.",
              collapse = "\n"
            )
          )
        )
      }

      validate(
        need(
          checkmate::test_class(data_out, "teal_data"),
          paste(
            "Assertion on return value from the 'data' module failed:",
            checkmate::test_class(data_out, "teal_data"),
            "Check your inputs or contact app developer if error persists.",
            collapse = "\n"
          )
        )
      )

      data_out
    })

    output$shiny_errors <- renderUI({
      data_validated()
      NULL
    })

    output$shiny_warnings <- renderUI({
      if (inherits(data_out_r(), "teal_data")) {
        is_modules_ok <- check_modules_datanames(modules = modules, datanames = .teal_data_ls(data_validated()))
        if (!isTRUE(is_modules_ok)) {
          tags$div(
            is_modules_ok$html(
              # Show modules prefix on message only in teal_data_module tab
              grepl(sprintf("data-teal_data_module-%s", id), session$ns(NULL), fixed = TRUE)
            ),
            class = "teal-output-warning"
          )
        }
      }
    })

    data_validated
  })
}

#' Fallback on failure
#'
#' Function returns the previous reactive if the current reactive is invalid (throws error or returns NULL).
#' Application: In `teal` we try to prevent the error from being thrown and instead we replace failing
#' transform module data output with data input from the previous module (or from previous `teal` reactive
#' tree elements).
#'
#' @param this (`reactive`) Current reactive.
#' @param that (`reactive`) Previous reactive.
#' @param label (`character`) Label for identifying problematic `teal_data_module` transform in logging.
#' @return `reactive` `teal_data`
#' @keywords internal
.fallback_on_failure <- function(this, that, label) {
  assert_reactive(this)
  assert_reactive(that)
  checkmate::assert_string(label)

  reactive({
    res <- try(this(), silent = TRUE)
    if (inherits(res, "teal_data")) {
      logger::log_debug("{ label } evaluated successfully.")
      res
    } else {
      logger::log_debug("{ label } failed, falling back to previous data.")
      that()
    }
  })
}
