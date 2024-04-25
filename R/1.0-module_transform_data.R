#' Transform reactive `teal_data` object
#'
#' Function applies data tranformation on `teal_data` object handed over by `teal` to `teal_module`
#'
#' @param module (`teal_module`) Module which data transformation will be applied to
#' @inheritParams teal_transform_module
#'
#' @seealso [teal_transform_module()]
#' @return `teal_module` with added transformation module
#' @export
transform_teal_data <- function(module, ui, server) {
  transformation_module <- teal_transform_module(ui, server)
  attr(module, "transform_modules") <- c(
    attr(module, "transform_modules"),
    list(transformation_module)
  )
  module
}

#' Create transform module
#'
#' Function creates a transform module that can be added to the `teal_module` object.
#' @param ui (`function(id)`) UI function for the module
#' @param server (`function(id, data)`) Server function for the module. Must accept `data` as an argument and
#' return a reactive `teal_data` object.
#'
#' @return `teal_data_module`
#' @export
teal_transform_module <- function(ui, server) {
  checkmate::assert_function(ui, args = "id", nargs = 1)
  checkmate::assert_function(server, args = c("id", "data"), nargs = 2)
  structure(
    list(ui = ui, server = server),
    class = "teal_data_module"
  )
}

#' Execute transform module
#'
#' Function executes the transform module and returns the transformed data.
#' Transformations' order is determined by the order of the modules in the `modules` argument.
#' Reactive data output of the previous module is used as input for the next module, so the final
#' data is the product of all consecutive transformations.
#' @name module_teal_module
#'
#' @param id (`character(1)`) Module id
#' @param data (`reactive`) `teal_data`
#' @param modules (`teal_module`)
#'
#' @return `reactive` `teal_data`
#' @keywords internal
ui_teal_transform_data <- function(id, modules) {
  checkmate::assert_string(id)
  checkmate::assert_class(modules, "teal_module")
  ns <- NS(id)
  lapply(
    seq_along(attr(modules, "transform_modules")),
    function(i) {
      bslib::accordion_panel(
        title = "Transformation",
        icon = bsicons::bs_icon("pencil-square"),
        attr(modules, "transform_modules")[[i]]$ui(
          id = ns(paste0("transform_", i))
        )
      )
    }
  )
}

#' @rdname module_teal_module
#' @keywords internal
srv_teal_transform_data <- function(id, data, modules) {
  checkmate::assert_string(id)
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(modules, "teal_module")
  moduleServer(id, function(input, output, session) {
    data_out <- list(data)
    transform_mods <- attr(modules, "transform_modules")
    for (i in seq_along(transform_mods)) {
      module_id <- paste0("transform_", i)
      data_out <- c(
        data_out,
        transform_mods[[i]]$server(
          id = module_id,
          data = data_out[[i]]
        )
      )
    }
    data_out[[length(data_out)]]
  })
}
