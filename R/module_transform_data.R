#' Module to transform `reactive` `teal_data`
#'
#' Module calls [teal_transform_module()] in sequence so that `reactive teal_data` output
#' from one module is handed over to the following module's input.
#'
#' @inheritParams module_teal_data
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
      transform_wrapper_id <- ns(sprintf("wrapper_%s", name))

      display_fun <- if (is.null(data_mod$ui)) shinyjs::hidden else function(x) x

      display_fun(
        bslib::accordion(
          bslib::accordion_panel(
            attr(data_mod, "label"),
            icon = bsicons::bs_icon("palette-fill"),
            tags$div(
              id = transform_wrapper_id,
              if (is.null(data_mod$ui)) {
                return(NULL)
              } else {
                data_mod$ui(id = ns("transform"))
              },
              div(
                id = ns("validate_messages"),
                class = "teal_validated",
                uiOutput(ns("error_wrapper"))
              )
            )
          )
        )
      )
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

            observeEvent(data_handled(), {
              if (inherits(data_handled(), "teal_data")) {
                if (!identical(data_handled(), data_out())) {
                  data_out(data_handled())
                }
              }
            })

            is_transform_failed[[name]] <- FALSE
            observeEvent(data_handled(), {
              if (inherits(data_handled(), "teal_data")) {
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

            srv_validate_error("silent_error", data_handled, validate_shiny_silent_error = FALSE)
            srv_check_class_teal_data("class_teal_data", data_handled)
            if (!is.null(modules)) {
              srv_check_module_datanames("datanames_warning", data_handled, modules)
            }

            # When there is no UI (`ui = NULL`) it should still show the errors
            observe({
              if (!inherits(data_handled(), "teal_data") && !is_previous_failed()) {
                shinyjs::show("wrapper")
              }
            })

            transform_wrapper_id <- sprintf("wrapper_%s", name)
            output$error_wrapper <- renderUI({
              if (is_previous_failed()) {
                shinyjs::disable(transform_wrapper_id)
                tags$div(
                  "One of previous transformators failed. Please check its inputs.",
                  class = "teal-output-warning"
                )
              } else {
                shinyjs::enable(transform_wrapper_id)
                shiny::tagList(
                  ui_validate_error(session$ns("silent_error")),
                  ui_check_class_teal_data(session$ns("class_teal_data")),
                  ui_check_module_datanames(session$ns("datanames_warning"))
                )
              }
            })
          })

          # Ignoring unwanted reactivity breaks during initialization
          reactive({
            req(data_out())
          })
        })
      },
      x = names(transformators),
      init = data
    )

    module_output
  })
}
