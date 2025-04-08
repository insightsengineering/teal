#' Module to transform `reactive` `teal_data`
#'
#' Module calls [teal_transform_module()] in sequence so that `reactive teal_data` output
#' from one module is handed over to the following module's input.
#'
#' @inheritParams module_teal_module
#' @param data_module (`teal_data_module`)
#' @param modules (`teal_modules` or `teal_module`) For `datanames` validation purpose
#' @param validate_shiny_silent_error (`logical`) If `TRUE`, then `shiny.silent.error` is validated and
#' @param is_transform_failed (`reactiveValues`) contains `logical` flags named after each transformator.
#' Help to determine if any previous transformator failed, so that following transformators can be disabled
#' and display a generic failure message.
#' @inheritParams teal_modules
#' @param class (character(1)) CSS class to be added in the `div` wrapper tag.

#' @return `reactive` `teal_data`
#'
#' @name module_transform_data
NULL

#' @export
#' @rdname module_transform_data
ui_transform_teal_data <- function(id, transformators, class = "well") {
  checkmate::assert_string(id)
  if (length(transformators) == 0L) {
    return(NULL)
  }
  if (inherits(transformators, "teal_transform_module")) {
    transformators <- list(transformators)
  }
  checkmate::assert_list(transformators, "teal_transform_module")
  names(transformators) <- sprintf("transform_%d", seq_len(length(transformators)))

  lapply(
    names(transformators),
    function(name) {
      child_id <- NS(id, name)
      ns <- NS(child_id)
      data_mod <- transformators[[name]]

      body_ui <- if (is.null(data_mod$ui)) NULL else data_mod$ui(id = ns("transform"))

      result <- bslib::accordion(
        id = ns("wrapper"),
        class = "validation-wrapper",
        bslib::accordion_panel(
          attr(data_mod, "label", exact = TRUE),
          icon = bsicons::bs_icon("palette-fill"),
          tags$div(
            id = ns(sprintf("wrapper_%s", name)),
            ui_module_validate(ns("validation")),
            body_ui
          )
        )
      )

      if (is.null(body_ui)) result <- shinyjs::hidden(result)
      result
    }
  )
}

#' @export
#' @rdname module_transform_data
srv_transform_teal_data <- function(id, data, transformators, modules = NULL, is_transform_failed = reactiveValues()) {
  checkmate::assert_string(id)
  assert_reactive(data)
  checkmate::assert_class(modules, "teal_module", null.ok = TRUE)
  if (length(transformators) == 0L) {
    return(data)
  }
  if (inherits(transformators, "teal_transform_module")) {
    transformators <- list(transformators)
  }
  checkmate::assert_list(transformators, "teal_transform_module", null.ok = TRUE)
  names(transformators) <- sprintf("transform_%d", seq_len(length(transformators)))

  moduleServer(id, function(input, output, session) {
    module_output <- Reduce(
      function(data_previous, name) {
        moduleServer(name, function(input, output, session) {
          logger::log_debug("srv_transform_teal_data@1 initializing module for { name }.")
          data_out <- reactiveVal()
          .call_once_when(inherits(data_previous(), "teal_data"), {
            logger::log_debug("srv_teal_transform_teal_data@2 triggering a transform module call for { name }.")
            data_unhandled <- transformators[[name]]$server("transform", data = data_previous)
            data_handled <- reactive(tryCatch(data_unhandled(), error = function(e) e))
            data_previous_handled <- reactive(tryCatch(data_previous(), error = function(e) e))

            observeEvent(data_handled(), {
              if (inherits(data_handled(), "teal_data")) {
                if (!identical(data_handled(), data_out())) {
                  data_out(data_handled())
                }
              }
            })

            is_transform_failed[[name]] <- FALSE
            observeEvent(data_handled(), {
              if (inherits(data_handled(), "teal_data") || rlang::is_condition(data_previous_handled())) {
                is_transform_failed[[name]] <- FALSE
              } else {
                is_transform_failed[[name]] <- TRUE
              }
            })

            is_previous_failed <- reactive({
              idx_this <- which(names(is_transform_failed) == name)
              is_transform_failed_list <- reactiveValuesToList(is_transform_failed)
              idx_failures <- which(unlist(is_transform_failed_list))
              any(idx_failures < idx_this)
            })

            srv_module_validate_transform(
              "validation",
              x = data_handled,
              validate_shiny_silent_error = FALSE,
              show_warn = is_previous_failed,
              message_warn = "One of the previous transformators failed. Please check its inputs."
            )

            # When there is no UI (`ui = NULL`) it should still show the errors
            observe({
              if (!inherits(data_handled(), "teal_data") && !is_previous_failed()) {
                shinyjs::show("wrapper")
              }
            })
            # Disable the UI elements in case of previous error
            observe({
              (if (is_previous_failed()) shinyjs::disable else shinyjs::enable)("wrapper")
            })
          })

          # Ignoring unwanted reactivity breaks during initialization
          reactive(req(data_out()))
        })
      },
      x = names(transformators),
      init = data
    )

    module_output
  })
}
