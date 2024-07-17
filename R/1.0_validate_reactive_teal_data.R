#' Validate reactive `teal_data`
#'
#' @section data validation:
#' `data` is invalid if:
#' - [teal_data_module()] is invalid if server doesn't return `reactive`.
#' - `reactive` throws a `shiny.error` - happens when module creating [teal_data()] fails.
#' - `reactive` returns `qenv.error` - happens when [teal_data()] evaluates a failing code.
#' - `reactive` object doesn't return [teal_data()].
#' - [teal_data()] object lacks any datanames specified in the `modules` argument.
#'
#' Any errors or warnings are displayed in the app pointing out to the reason of failure.
#' In all above, reactive cycle is halted and `teal` doesn't continue sending data further. On init,
#' halting reactive cycle stops an app load, while on subsequent reactive cycles, data just remains
#' unchanged and user is able to continue using the app.
#'
#' @inheritParams module_data
#' @return (`reactive` returning `teal_data`)
#' @rdname validate_reactive_teal_data
#' @name validate_reactive_teal_data
#' @keywords internal
NULL

#' @rdname validate_reactive_teal_data
#' @keywords internal
ui_validate_reactive_teal_data <- function(id) {
  uiOutput(NS(id, "response"))
}

#' @rdname validate_reactive_teal_data
#' @param validate_shiny_silent_error (`logical`) If `TRUE`, then `shiny.silent.error` is validated and
#' error message is displayed.
#' Default is `FALSE` to handle empty reactive cycle on init.
#' @keywords internal
srv_validate_reactive_teal_data <- function(id, data, modules = NULL, filter = teal_slices(), validate_shiny_silent_error = FALSE) {
  moduleServer(id, function(input, output, session) {
    if (!is.reactive(data)) {
      stop("The `teal_data_module` passed to `data` must return a reactive expression.", call. = FALSE)
    }
    data_validated <- reactive({
      # custom module can return error
      data_out <- tryCatch(data(), error = function(e) e)

      # there is an empty reactive cycle on init!
      if (inherits(data_out, "shiny.silent.error") && identical(data_out$message, "")) {
        if (!validate_shiny_silent_error) {
          return(NULL)
        } else {
          validate(
            need(
              FALSE,
              ifelse(nzchar(data_out$message), data_out$message, "Error with module. Check your inputs.")
            )
          )
        }
      }

      # to handle qenv.error
      if (inherits(data_out, "qenv.error")) {
        validate(
          need(
            FALSE,
            paste(
              "Error when executing `teal_data_module` passed to `data`:\n ",
              paste(data_out$message, collapse = "\n"),
              "\n Check your inputs or contact app developer if error persists."
            )
          )
        )
      }

      # to handle module non-qenv errors
      if (inherits(data_out, "error")) {
        validate(
          need(
            FALSE,
            paste(
              "Error when executing `teal_data_module` passed to `data`:\n ",
              paste(data_out$message, collpase = "\n"),
              "\n Check your inputs or contact app developer if error persists."
            )
          )
        )
      }

      validate(
        need(
          inherits(data_out, "teal_data"),
          paste(
            "Error: `teal_data_module` passed to `data` failed to return `teal_data` object, returned",
            toString(sQuote(class(data_out))),
            "instead.",
            "\n Check your inputs or contact app developer if error persists."
          )
        )
      )

      if (!length(teal.data::datanames(data_out))) {
        warning("`data` object has no datanames. Default datanames are set using `teal_data`'s environment.")
      }

      if (!is.null(modules)) {
        .validate_module_datanames(data_out, modules)
      }

      .validate_filter_datanames(data_out, filter)

      data_out
    })

    output$response <- renderUI({
      if (!is.null(data_validated())) {
        removeModal()
      }
      NULL
    })

    data_validated
  })
}


.validate_module_datanames <- function(data, modules) {
  is_modules_ok <- check_modules_datanames(modules, teal_data_datanames(data))
  if (!isTRUE(is_modules_ok)) {
    validate(need(isTRUE(is_modules_ok), sprintf("%s. Contact app developer.", is_modules_ok)))
  }
}

.validate_filter_datanames <- function(data, filter) {
  is_filter_ok <- check_filter_datanames(filter, teal_data_datanames(data))
  if (!isTRUE(is_filter_ok)) {
    showNotification(
      "Some filters were not applied because of incompatibility with data. Contact app developer.",
      type = "warning",
      duration = 10
    )
    warning(is_filter_ok)
  }
}
