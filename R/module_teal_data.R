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
#' @param data_module (`teal_data_module`)
#' @param modules (`teal_modules` or `teal_module`) For `datanames` validation purpose
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

#' @rdname module_teal_data
#' @aliases ui_teal_data
#' @note
#' `ui_teal_data_module` was renamed from `ui_teal_data`.
ui_teal_data_module <- function(id, data_module = function(id) NULL) {
  checkmate::assert_string(id)
  checkmate::assert_function(data_module, args = "id")
  ns <- NS(id)

  shiny::tagList(
    tags$div(id = ns("wrapper"), data_module(id = ns("data"))),
    ui_validate_reactive_teal_data(ns("validate"))
  )
}

#' @rdname module_teal_data
#' @aliases srv_teal_data
#' @note
#' `srv_teal_data_module` was renamed from `srv_teal_data`.
srv_teal_data_module <- function(id,
                                 data_module = function(id) NULL,
                                 modules = NULL,
                                 validate_shiny_silent_error = TRUE,
                                 is_transform_failed = reactiveValues()) {
  checkmate::assert_string(id)
  checkmate::assert_function(data_module, args = "id")
  checkmate::assert_multi_class(modules, c("teal_modules", "teal_module"), null.ok = TRUE)
  checkmate::assert_class(is_transform_failed, "reactivevalues")

  moduleServer(id, function(input, output, session) {
    logger::log_debug("srv_teal_data_module initializing.")
    is_transform_failed[[id]] <- FALSE
    module_out <- data_module(id = "data")
    try_module_out <- reactive(tryCatch(module_out(), error = function(e) e))
    observeEvent(try_module_out(), {
      if (!inherits(try_module_out(), "teal_data")) {
        is_transform_failed[[id]] <- TRUE
      } else {
        is_transform_failed[[id]] <- FALSE
      }
    })

    is_previous_failed <- reactive({
      idx_this <- which(names(is_transform_failed) == id)
      is_transform_failed_list <- reactiveValuesToList(is_transform_failed)
      idx_failures <- which(unlist(is_transform_failed_list))
      any(idx_failures < idx_this)
    })

    observeEvent(is_previous_failed(), {
      if (is_previous_failed()) {
        shinyjs::disable("wrapper")
      } else {
        shinyjs::enable("wrapper")
      }
    })

    srv_validate_reactive_teal_data(
      "validate",
      data = try_module_out,
      modules = modules,
      validate_shiny_silent_error = validate_shiny_silent_error,
      hide_validation_error = is_previous_failed
    )
  })
}

#' @rdname module_teal_data
ui_validate_reactive_teal_data <- function(id) {
  ns <- NS(id)
  tags$div(
    div(
      id = ns("validate_messages"),
      class = "teal_validated",
      ui_validate_error(ns("silent_error")),
      ui_check_class_teal_data(ns("class_teal_data")),
      ui_check_module_datanames(ns("shiny_warnings"))
    ),
    div(
      class = "teal_validated",
      uiOutput(ns("previous_failed"))
    )
  )
}

#' @rdname module_teal_data
srv_validate_reactive_teal_data <- function(id, # nolint: object_length
                                            data,
                                            modules = NULL,
                                            validate_shiny_silent_error = FALSE,
                                            hide_validation_error = reactive(FALSE)) {
  checkmate::assert_string(id)
  checkmate::assert_multi_class(modules, c("teal_modules", "teal_module"), null.ok = TRUE)
  checkmate::assert_flag(validate_shiny_silent_error)

  moduleServer(id, function(input, output, session) {
    # there is an empty reactive cycle on `init` and `data` has `shiny.silent.error` class
    srv_validate_error("silent_error", data, validate_shiny_silent_error)
    srv_check_class_teal_data("class_teal_data", data)
    srv_check_module_datanames("shiny_warnings", data, modules)
    output$previous_failed <- renderUI({
      if (hide_validation_error()) {
        shinyjs::hide("validate_messages")
        tags$div("One of previous transformators failed. Please check its inputs.", class = "teal-output-warning")
      } else {
        shinyjs::show("validate_messages")
        NULL
      }
    })

    .trigger_on_success(data)
  })
}

#' @keywords internal
ui_validate_error <- function(id) {
  ns <- NS(id)
  uiOutput(ns("message"))
}

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
          inherits(data(), c("teal_data", "error")),
          "Did not receive `teal_data` object. Cannot proceed further."
        )
      )
    })
  })
}

#' @keywords internal
ui_check_module_datanames <- function(id) {
  ns <- NS(id)
  uiOutput(NS(id, "message"))
}

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
