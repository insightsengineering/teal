#' Get dataset attributes
#'
#' @description `r lifecycle::badge("stable")`
#' Get dataset attributes in form of named list.
#'
#' @param x an object of (`TealDataset`) class
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
#' # TealDataset --------
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
#' # CDISCTealDataset --------
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
get_attrs.TealDataset <- function(x) {
  return(x$get_attrs())
}
