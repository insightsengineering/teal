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
  # todo: format error message nicely. Add (⚠) icon.
  tagList(
    uiOutput(NS(id, "shiny_errors")),
    uiOutput(NS(id, "shiny_warnings"))
  )
}

#' @rdname validate_reactive_teal_data
#' @param validate_shiny_silent_error (`logical`) If `TRUE`, then `shiny.silent.error` is validated and
#' error message is displayed.
#' Default is `FALSE` to handle empty reactive cycle on init.
#' @keywords internal
srv_validate_reactive_teal_data <- function(id,
                                            data,
                                            modules = NULL,
                                            validate_shiny_silent_error = FALSE) {
  moduleServer(id, function(input, output, session) {
    if (!is.reactive(data)) {
      stop("The `teal_data_module` passed to `data` must return a reactive expression.", call. = FALSE)
    }

    data_out_rv <- reactive(tryCatch(data(), error = function(e) e))

    data_validated <- reactive({
      # custom module can return error
      data_out <- data_out_rv()

      # there is an empty reactive cycle on init!
      if (inherits(data_out, "shiny.silent.error") && identical(data_out$message, "")) {
        if (!validate_shiny_silent_error) {
          return(NULL)
        } else {
          validate(
            need(
              FALSE,
              paste(
                data_out$message,
                "Check your inputs or contact app developer if error persists.",
                sep = ifelse(identical(data_out$message, ""), "", "\n")
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
              "Error when executing `teal_data_module` passed to `data`:\n ",
              paste(data_out$message, collapse = "\n"),
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


      data_out
    })

    output$shiny_errors <- renderUI({
      data_validated()
      NULL
    })

    output$shiny_warnings <- renderUI({
      if (inherits(data_out_rv(), "teal_data")) {
        is_modules_ok <- check_modules_datanames(modules = modules, datanames = teal_data_datanames(data_validated()))
        if (!isTRUE(is_modules_ok)) {
          tags$div(is_modules_ok, class = "teal-output-warning")
        }
      }
    })

    data_validated
  })
}

ui_teal_module_validation_error <- function(id) {
  uiOutput(NS(id, "message"))
}

srv_teal_module_validation_error <- function(id, modules, data) {
  moduleServer(id, function(input, output, session) {
    is_modules_ok_rv <- reactive({
      req(data())
      check_modules_datanames(
        modules = modules,
        datanames = datanames(data())
      )
    })

    # check_modules_datanames()
    module_teal_data <- reactive({
      req(is_modules_ok_rv())

      if (isTRUE(is_modules_ok_rv())) {
        data()
      } else {
        validate(need(FALSE, is_modules_ok_rv())) # this prevents the module from being rendered
      }
    })


    output$message <- renderUI({
      if (!isTRUE(req(is_modules_ok_rv()))) {
        tags$div(
          class = "teal-validation-error",
          tags$p(is_modules_ok_rv()),
          tags$p(
            "Check your inputs, data and transformation modules.",
            "Contact app developer if error persists."
          )
        )
      }
    })
    module_teal_data
  })
}
