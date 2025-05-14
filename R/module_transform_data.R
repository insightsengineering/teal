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

  ns_parent <- NS(id)
  mapply(
    id = ns_parent(names(transformators)),
    data_mod = transformators,
    name = names(transformators),
    SIMPLIFY = FALSE,
    function(id, data_mod, name) {
      bslib::accordion(
        class = "validation-wrapper",
        bslib::accordion_panel(
          title = name,
          icon = bsicons::bs_icon("palette-fill"),
          .ui_call_teal_module(id = id, ui = data_mod$ui)
        )
      )
    }
  )
}

#' @export
#' @rdname module_transform_data
srv_transform_teal_data <- function(id, data, transformators, datanames_required = list()) {
  checkmate::assert_string(id)
  assert_reactive(data)
  checkmate::assert_list(datanames_required, types = c("character", "NULL"))
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
      x = names(transformators),
      init = data,
      function(data_previous, x) {
        .srv_call_teal_module(
          id = x,
          data = data_previous,
          server = transformators[[x]]$server,
          datanames_required = datanames_required
        )
      }
    )

    module_output
  })
}
