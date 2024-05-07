# TODO: this function doesn't have to exist, it can be replaced by teal_data_module. The difference is the
#  data argument which is not needed in the `teal_data_module`.

#' Create transform module
#'
#' Function creates a transform module that can be added to the `teal_module` object.
#' @param ui (`function(id)`) UI function for the module
#' @param server (`function(id, data)`) Server function for the module. Must accept `data` as an argument and
#' return a reactive `teal_data` object.
#'
#' @return `teal_data_module`
#' @export
teal_transform_module <- function(ui, server, label = "Transform data") {
  checkmate::assert_function(ui, args = "id", nargs = 1)
  checkmate::assert_function(server, args = c("id", "data"), nargs = 2)
  structure(
    list(ui = ui, server = server),
    label = label,
    class = "teal_data_module"
  )
}

#' Execute teal_data_module
#'
#' Function executes the `teal_data_module` and returns the modified data.
#' Modules' execution order is determined by the order provided to `...` argument.
#' Reactive data output of the previous module is used as input for the next module, so the final
#' data is the product of all consecutive transformations.
#' @name module_teal_module
#'
#' @param id (`character(1)`) Module id
#' @param data (`reactive`) `teal_data`
#' @param ... (`teal_data_module`)
#'
#' @return `reactive` `teal_data`
#' @export
ui_teal_data_module <- function(id, transformers) {
  checkmate::assert_string(id)
  checkmate::assert_list(transformers, "teal_data_module", null.ok = TRUE)

  ns <- NS(id)
  lapply(
    seq_along(transformers),
    function(i) {
      data_mod <- transformers[[i]]
      div( # todo: accordion?
        title = attr(data_mod, "label"),
        icon = icon("pencil-square"),
        data_mod$ui(id = ns(paste0("data_", i)))
      )
    }
  )
}

#' @rdname module_teal_module
#' @export
srv_teal_data_module <- function(id, data, transformers) {
  checkmate::assert_string(id)
  checkmate::assert_list(transformers, "teal_data_module", min.len = 0)

  moduleServer(id, function(input, output, session) {
    data_out <- list(data)
    lapply(
      seq_along(transformers),
      function(i) {
        module_id <- paste0("data_", i)
        data_out <<- c(
          data_out,
          transformers[[i]]$server(id = module_id, data = data_out[[i]])
        )
      }
    )
    data_out[[length(data_out)]]
  })
}
