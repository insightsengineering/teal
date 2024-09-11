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
    div(
      id = ns("wrapper"),
      data_module$ui(id = ns("data"))
    ),
    div(
      id = ns("input_error"),
      class = "teal_validated",
      uiOutput(ns("error"))
    ),
    ui_validate_reactive_teal_data(ns("validate"))
  )
}

#' @rdname module_teal_data
srv_teal_data <- function(id,
                          data,
                          data_module,
                          modules = NULL,
                          validate_shiny_silent_error = TRUE,
                          failure_callback = function(data) {
                            invisible(NULL)
                          },
                          is_transformer_failed = reactiveValues()) {
  checkmate::assert_string(id)
  checkmate::assert_class(data_module, "teal_data_module")
  checkmate::assert_multi_class(modules, c("teal_modules", "teal_module"), null.ok = TRUE)

  moduleServer(id, function(input, output, session) {
    logger::log_debug("srv_teal_data initializing.")
    is_transformer_failed[[id]] <- FALSE
    data_out <- if (is_arg_used(data_module$server, "data")) {
      data_module$server(id = "data", data = data)
    } else {
      data_module$server(id = "data")
    }

    data_validated <- srv_validate_reactive_teal_data(
      id = "validate",
      data = data_out,
      modules = modules,
      validate_shiny_silent_error = validate_shiny_silent_error
    )

    out <- reactiveVal(NULL)
    observeEvent(data_validated(), {
      if (inherits(data_validated(), "teal_data")) {
        is_transformer_failed[[id]] <- FALSE
        if (!identical(data_validated(), out())) {
          out(data_validated())
        }
      } else {
        is_transformer_failed[[id]] <- TRUE
      }
      failure_callback(data_validated)
    })


    is_previous_failed <- reactive({
      idx_this <- which(names(is_transformer_failed) == id)
      is_transformer_failed_list <- reactiveValuesToList(is_transformer_failed)
      idx_failures <- which(unlist(is_transformer_failed_list))
      any(idx_failures < idx_this)
    })

    output$error <- renderUI({
      validation_ids <-
        paste0("validate-", c("shiny_warnings", "class_teal_data", "qenv_error", "silent_error"), "-message")
      if (is_previous_failed()) {
        shinyjs::disable("wrapper")
        sapply(validation_ids, shinyjs::hide)

        tags$div(
          class = "teal-output-warning",
          "One of previous transformers failed. Please fix and continue."
        )
      } else {
        shinyjs::enable("wrapper")
        sapply(validation_ids, shinyjs::show)
        NULL
      }
    })


    out
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
    # tryCatch can not be removed since in srv_teal_data this module works on non tryCatch-ed data
    data_rv <- reactive(tryCatch(data(), error = function(e) e))
    # there is an empty reactive cycle on init!
    srv_validate_silent_error("silent_error", data_rv, validate_shiny_silent_error)
    srv_validate_qenv_error("qenv_error", data_rv)
    if (!inherits(isolate(data_rv()), "shiny.silent.error")) {
      srv_check_class_teal_data("class_teal_data", data_rv)
    }
    srv_check_shiny_warnings("shiny_warnings", data_rv, modules)

    data_rv
  })
}

#' @keywords internal
ui_validate_silent_error <- function(id) {
  ns <- NS(id)
  uiOutput(ns("message"))
}

#' @keywords internal
srv_validate_silent_error <- function(id, data, validate_shiny_silent_error) {
  checkmate::assert_string(id)
  checkmate::assert_flag(validate_shiny_silent_error)
  moduleServer(id, function(input, output, session) {
    output$message <- renderUI({
      if (inherits(data(), "shiny.silent.error") && identical(data()$message, "")) {
        if (!validate_shiny_silent_error) {
          return(NULL)
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
  uiOutput(ns("message"))
}

#' @keywords internal
srv_validate_qenv_error <- function(id, data) {
  checkmate::assert_string(id)
  moduleServer(id, function(input, output, session) {
    output$message <- renderUI({
      if (inherits(data(), c("qenv.error"))) {
        validate(
          need(
            FALSE,
            paste(
              "Error when executing the `data` module:",
              strip_style(paste(data()$message, collapse = "\n")),
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
  uiOutput(ns("message"))
}

#' @keywords internal
srv_check_class_teal_data <- function(id, data) {
  checkmate::assert_string(id)
  moduleServer(id, function(input, output, session) {
    output$message <- renderUI({
      validate(
        need(
          inherits(data(), "teal_data"),
          "Did not recieve a valid `teal_data` object. Cannot proceed further."
        )
      )
    })
  })
}

#' @keywords internal
ui_check_shiny_warnings <- function(id) {
  ns <- NS(id)
  uiOutput(NS(id, "message"))
}

#' @keywords internal
srv_check_shiny_warnings <- function(id, data, modules) {
  checkmate::assert_string(id)
  moduleServer(id, function(input, output, session) {
    output$message <- renderUI({
      if (inherits(data(), "teal_data")) {
        is_modules_ok <- check_modules_datanames(modules = modules, datanames = .teal_data_ls(data()))
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
