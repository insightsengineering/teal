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
        data_mod$ui(id = ns(sprintf("data_%d", i)))
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
    Reduce(
      function(x, ix) transformers[[ix]]$server(id = sprintf("data_%d", ix), data = x),
      seq_along(transformers),
      init = data
    )
  })
}
