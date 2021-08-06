#' Get dataset attributes
#'
#' @description `r lifecycle::badge("experimental")`
#' Get dataset attributes in form of named list.
#'
#' @param x an object of (`Dataset`) class
#'
#' @return named `list` of object attributes
#'
#' @export
get_attrs <- function(x) {
  UseMethod("get_attrs")
}


#' @rdname get_attrs
#' @export
#' @examples
#' # Dataset --------
#'
#' library(scda)
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#'
#' x1 <- dataset("ADSL", x = ADSL, label = "custom label")
#' get_attrs(x1)
#'
#' x2 <- dataset(
#'   "ADSL",
#'   x = ADSL,
#'   keys = get_cdisc_keys("ADSL"),
#'   label = "custom label"
#' )
#' get_attrs(x2)
#'
#' # CDISCDataset --------
#'
#' library(scda)
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#' x3 <- cdisc_dataset(
#'   "ADSL",
#'   x = ADSL,
#'   keys = get_cdisc_keys("ADSL"),
#'   label = "custom label"
#' )
#' get_attrs(x3)
get_attrs.Dataset <- function(x) {
  return(x$get_attrs())
}
