#' Create a `tdata` object
#'
#' @description `r lifecycle::badge("superseded")`
#'
#' Recent changes in `teal` cause modules to fail because modules expect a `tdata` object
#' to be passed to the `data` argument but instead they receive a `teal_data` object,
#' which is additionally wrapped in a reactive expression in the server functions.
#' In order to easily adapt such modules without a proper refactor,
#' use this function to downgrade the `data` argument.
#'
#' @name tdata
#' @param ... ignored
#' @return nothing
NULL

#' @rdname tdata
#' @export
new_tdata <- function(...) {
  .deprecate_tdata_msg()
}

#' @rdname tdata
#' @export
tdata2env <- function(...) {
  .deprecate_tdata_msg()
}

#' @rdname tdata
#' @export
get_code_tdata <- function(...) {
  .deprecate_tdata_msg()
}

#' @rdname tdata
#' @export
join_keys.tdata <- function(...) {
  .deprecate_tdata_msg()
}

#' @rdname tdata
#' @export
get_metadata <- function(...) {
  .deprecate_tdata_msg()
}

#' @rdname tdata
#' @export
as_tdata <- function(...) {
  .deprecate_tdata_msg()
}


.deprecate_tdata_msg <- function() {
  lifecycle::deprecate_stop(
    when = "0.16.0",
    what = "tdata()",
    details = paste(
      "tdata has been removed in favour of `teal_data`.\n",
      "Please follow migration instructions https://github.com/insightsengineering/teal/discussions/987."
    )
  )
}
