#' Module to transform `reactive` `teal_data`
#'
#' Module calls [teal_transform_module()] in sequence so that `reactive teal_data` output
#' from one module is handed over to the following module's input.
#'
#' @inheritParams module_teal_data
#' @inheritParams teal_modules
#' @return `reactive` `teal_data`
#'
#'
#' @name module_teal_transform_module
NULL

#' @export
#' @rdname module_teal_transform_module
ui_teal_transform_module <- function(id, transforms, class = "well") {
  checkmate::assert_string(id)
  if (length(transforms) == 0L) {
    return(NULL)
  }
  if (inherits(transforms, "teal_transform_module")) {
    transforms <- list(transforms)
  }
  checkmate::assert_list(transforms, "teal_transform_module")

  labels <- lapply(transforms, function(x) attr(x, "label"))
  ids <- get_unique_labels(labels)
  names(transforms) <- ids

  lapply(
    names(transforms),
    function(name) {
      child_id <- NS(id)(name)
      ns <- NS(child_id)
      data_mod <- transforms[[name]]
      transform_wrapper_id <- ns(sprintf("wrapper_%s", name))

      div( # todo: accordion?
        # class .teal_validated changes the color of the boarder on error in ui_validate_reactive_teal_data
        #   For details see tealValidate.js file.
        class = c(class, "teal_validated"),
        title = attr(data_mod, "label"),
        tags$span(
          class = "text-primary mb-4",
          icon("fas fa-square-pen"),
          attr(data_mod, "label")
        ),
        tags$i(
          class = "remove pull-right fa fa-angle-down",
          style = "cursor: pointer;",
          title = "fold/expand transform panel",
          onclick = sprintf("togglePanelItems(this, '%s', 'fa-angle-right', 'fa-angle-down');", transform_wrapper_id)
        ),
        tags$div(
          id = transform_wrapper_id,
          if (is.null(data_mod$ui)) {
            return(NULL)
          } else {
            data_mod$ui(id = ns(name))
          },
          div(
            id = ns("validate_messages"),
            class = "teal_validated",
            uiOutput(ns("error_wrapper"))
          )
        )
      )
    }
  )
}

#' @export
#' @rdname module_teal_transform_module
srv_teal_transform_module <- function(id, data, transforms, modules = NULL, is_transformer_failed = reactiveValues()) {
  checkmate::assert_string(id)
  assert_reactive(data)
  checkmate::assert_class(modules, "teal_module", null.ok = TRUE)
  if (length(transforms) == 0L) {
    return(data)
  }
  if (inherits(transforms, "teal_transform_module")) {
    transforms <- list(transforms)
  }
  checkmate::assert_list(transforms, "teal_transform_module", null.ok = TRUE)
  labels <- lapply(transforms, function(x) attr(x, "label"))
  ids <- get_unique_labels(labels)
  names(transforms) <- ids

  moduleServer(id, function(input, output, session) {
    Reduce(
      function(data_previous, name) {
        moduleServer(name, function(input, output, session) {
          logger::log_debug("srv_teal_transform_module initializing for { name }.")
          is_transformer_failed[[name]] <- FALSE
          data_out <- transforms[[name]]$server(name, data = data_previous)
          data_handled <- reactive(tryCatch(data_out(), error = function(e) e))
          observeEvent(data_handled(), {
            if (inherits(data_handled(), "teal_data")) {
              is_transformer_failed[[name]] <- FALSE
            } else {
              is_transformer_failed[[name]] <- TRUE
            }
          })

          is_previous_failed <- reactive({
            idx_this <- which(names(is_transformer_failed) == name)
            is_transformer_failed_list <- reactiveValuesToList(is_transformer_failed)
            idx_failures <- which(unlist(is_transformer_failed_list))
            any(idx_failures < idx_this)
          })

          srv_validate_error("silent_error", data_handled, validate_shiny_silent_error = FALSE)
          srv_check_class_teal_data("class_teal_data", data_handled)
          if (!is.null(modules)) {
            srv_check_module_datanames("datanames_warning", data_handled, modules)
          }

          transform_wrapper_id <- sprintf("wrapper_%s", name)
          output$error_wrapper <- renderUI({
            if (is_previous_failed()) {
              shinyjs::disable(transform_wrapper_id)
              tags$div("One of previous transformers failed. Please fix and continue.", class = "teal-output-warning")
            } else {
              shinyjs::enable(transform_wrapper_id)
              shiny::tagList(
                ui_validate_error(session$ns("silent_error")),
                ui_check_class_teal_data(session$ns("class_teal_data")),
                ui_check_module_datanames(session$ns("datanames_warning"))
              )
            }
          })

          .trigger_on_success(data_handled)
        })
      },
      x = names(transforms),
      init = data
    )
  })
}
