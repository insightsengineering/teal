#' Convert a `Named<...>` object to a `Relational<...>` object
#'
#' Convert a `Named<...>` to a `Relational<...>`
#'
#' @description `r lifecycle::badge("soft-deprecated")`
#'
#' @inheritParams dataset
#' @param x an object of (`NamedDataset` or `NamedDatasetConnector`) class
#'
#' @return (`RelationalDataset` or `RelationalDatasetConnector`) object
#'
#' @export
as_relational <- function(x, keys) {
  lifecycle::deprecate_warn(
    "0.9.2",
    "teal::as_relational()",
  )
  return(x)
}

#' @rdname as_relational
#'
#' @description `r lifecycle::badge("defunct")`
#' @details
#' \code{as_cdisc_relational} will derive the keys by the \code{dataname} and therefore
#'   does not need the \code{keys} argument to be specified
#'
#' @export
as_cdisc_relational <- function(x,
                                dataname,
                                label = character(0)) {
  lifecycle::deprecate_stop(
    "0.9.2",
    "teal::as_cdisc_relational()",
    details = "Please use `x %>% as_relational(., ...) %>% as_cdisc(., ...)` instead"
  )
}
