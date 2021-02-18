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
#' library(random.cdisc.data)
#' adsl <- cdisc_dataset(dataname = "ADSL",
#'                       x = radsl(cached = TRUE),
#'                       code = "library(random.cdisc.data)\nADSL <- radsl(cached = TRUE)")
#'
#' adae <- cdisc_dataset(dataname = "ADAE",
#'                        x = radae(cached = TRUE),
#'                        code = "library(random.cdisc.data)\nADTTE <- radae(cached = TRUE)")
#'
#' rd <- cdisc_data(adsl, adae)
#' get_datasets(rd)
#'
#' # RelationalDataConnector --------
#' rdc <- rcd_data(
#'   rcd_cdisc_dataset_connector("ADSL", radsl, cached = TRUE),
#'   rcd_cdisc_dataset_connector("ADTTE", radtte, cached = TRUE)
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
#' library(random.cdisc.data)
#' rdc <- rcd_cdisc_dataset_connector("ADSL", radsl, cached = TRUE)
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
#' library(random.cdisc.data)
#' adsl <- cdisc_dataset(dataname = "ADSL",
#'                       x = radsl(cached = TRUE),
#'                       code = "library(random.cdisc.data)\nADSL <- radsl(cached = TRUE)")
#'
#' get_datasets(adsl)
get_datasets.Dataset <- function(x) {
  x
}
