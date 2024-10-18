#' Module to transform `reactive` `teal_data`
#'
#' Module calls multiple [`module_teal_data`] in sequence so that `reactive teal_data` output
#' from one module is handed over to the following module's input.
#'
#' @inheritParams module_teal_data
#' @inheritParams teal_modules
#' @return `reactive` `teal_data`
#'
#'
#' @name module_transform_data
#' @keywords internal
NULL

#' @rdname module_transform_data
ui_transform_data <- function(id, transforms, class = "well") {
  checkmate::assert_string(id)
  checkmate::assert_list(transforms, "teal_transform_module", null.ok = TRUE)
  ns <- NS(id)
  labels <- lapply(transforms, function(x) attr(x, "label"))
  ids <- get_unique_labels(labels)
  names(transforms) <- ids

  lapply(
    names(transforms),
    function(name) {
      data_mod <- transforms[[name]]
      wrapper_id <- ns(sprintf("wrapper_%s", name))
      tags$div(
        class = "teal-trasform-component",
        bslib::accordion(
          class = "teal-transform-accordian", # todo: make transform accordian
          bslib::accordion_panel(
            attr(data_mod, "label"),
            icon = icon("fas fa-square-pen"),
            div(
              id = wrapper_id,
              ui_teal_data(id = ns(name), data_module = transforms[[name]]$ui)
            )
          )
        )
      )
    }
  )
}

#' @rdname module_transform_data
srv_transform_data <- function(id, data, transforms, modules, is_transformer_failed = reactiveValues()) {
  checkmate::assert_string(id)
  assert_reactive(data)
  checkmate::assert_list(transforms, "teal_transform_module", null.ok = TRUE)
  checkmate::assert_class(modules, "teal_module")
  if (length(transforms) == 0L) {
    return(data)
  }
  labels <- lapply(transforms, function(x) attr(x, "label"))
  ids <- get_unique_labels(labels)
  names(transforms) <- ids
  moduleServer(id, function(input, output, session) {
    logger::log_debug("srv_teal_data_modules initializing.")
    Reduce(
      function(previous_result, name) {
        srv_teal_data(
          id = name,
          data_module = function(id) transforms[[name]]$server(id, previous_result),
          modules = modules,
          is_transformer_failed = is_transformer_failed
        )
      },
      x = names(transforms),
      init = data
    )
  })
}
