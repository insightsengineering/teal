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

#' @inherit as_relational
#' @inheritParams dataset
#' @description `r lifecycle::badge("defunct")`
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
