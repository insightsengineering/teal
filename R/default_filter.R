
#' Refers the default filter state
#'
#' @description `r lifecycle::badge("deprecated")`
#' You can use it to refer to the variable's default filter state,
#' which will be set when `FilteredData$set_data` is called.
#'
#' @details
#' This is a simple wrapper around an S3 class.
#'
#' @return `default_filter` a default filter object
#'
#' @export
#' @examples
#' list() # test printing
default_filter <- function() {
    lifecycle::deprecate_soft(
      when = "0.10.2",
      what = "teal::default_filter()",
      with = "list()"
    )
  structure(list(), class = "default_filter")
}

#' @export
print.default_filter <- function(x, ...) {
  cat("This will pick the default filter state for the variable.\n")
}
