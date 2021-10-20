#' Get a \code{\link{Dataset}} objects.
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param x (\code{\link{RelationalData}})\cr
#'  object containing datasets.
#' @export
#' @return \code{list} or \code{Dataset} objects
get_datasets <- function(x) {
  UseMethod("get_datasets")
}

#' @rdname get_datasets
#' @export
#' @examples
#'
#' # RelationalData --------
#' library(scda)
#' adsl <- cdisc_dataset(dataname = "ADSL",
#'                       x = synthetic_cdisc_data("latest")$adsl,
#'                       code = "library(scda)\nADSL <- synthetic_cdisc_data(\"latest\")$adsl")
#'
#' adae <- cdisc_dataset(dataname = "ADAE",
#'                        x = synthetic_cdisc_data("latest")$adae,
#'                        code = "library(scda)\nADTTE <- synthetic_cdisc_data(\"latest\")$adae")
#'
#' rd <- cdisc_data(adsl, adae)
#' get_datasets(rd)
#'
#' # RelationalDataConnector --------
#' adsl_cf <- callable_function(function() synthetic_cdisc_data("latest")$adsl)
#' adsl <- cdisc_dataset_connector(dataname = "ADSL",
#'                                 pull_callable = adsl_cf,
#'                                 keys = get_cdisc_keys("ADSL"))
#' adlb_cf <- callable_function(function() synthetic_cdisc_data("latest")$adlb)
#' adlb <- cdisc_dataset_connector(dataname = "ADLB",
#'                                 pull_callable = adlb_cf,
#'                                 keys = get_cdisc_keys("ADLB"))
#'
#' rdc <- teal:::RelationalDataConnector$new(
#'   connection = teal:::DataConnection$new(),
#'   connectors = list(adsl, adlb)
#' )
#'
#'\dontrun{
#' load_datasets(rdc)
#' get_datasets(rdc)
#'}
#'
#' # RelationalData --------
#' drc <- cdisc_data(rdc, adae)
#' \dontrun{
#' get_datasets(drc)
#' }
get_datasets.DataAbstract <- function(x) { # nolint
  res <- x$get_datasets()
  if (is_empty(res)) {
    return(invisible(NULL))
  }
  res
}

#' @rdname get_datasets
#' @export
#' @examples
#'
#' # DatasetConnector --------
#' library(scda)
#' adsl_cf <- callable_function(
#'   function() {synthetic_cdisc_data("latest")$adsl}
#' )
#' rdc <- cdisc_dataset_connector("ADSL", adsl_cf, keys = get_cdisc_keys("ADSL"))
#'
#'\dontrun{
#' load_datasets(rdc)
#' get_datasets(rdc)
#'}
get_datasets.DatasetConnector <- function(x) { # nolint
  res <- x$get_dataset()
  if (is_empty(res)) {
    return(invisible(NULL))
  }
  res
}

#' @rdname get_datasets
#' @export
#' @examples
#'
#' # Dataset --------
#' library(scda)
#' adsl <- cdisc_dataset(dataname = "ADSL",
#'                       x = synthetic_cdisc_data("latest")$adsl,
#'                       code = "library(scda)\nADSL <- synthetic_cdisc_data(\"latest\")$adsl")
#'
#' get_datasets(adsl)
get_datasets.Dataset <- function(x) {
  x
}
