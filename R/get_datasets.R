#' Get a \code{\link{RelationalDataset}} objects.
#'
#' @param x (\code{\link{RelationalData}})\cr
#'  object containing datasets.
#' @export
#' @rdname get_datasets
#' @return \code{list} or \code{RelationalDataset} objects
get_datasets <- function(x) {
  UseMethod("get_datasets")
}

#' @rdname get_datasets
#' @export
#' @examples
#'
#' # RelationalDataList --------
#' library(random.cdisc.data)
#' adsl <- cdisc_dataset(dataname = "ADSL", # RelationalDataset
#'                       data = radsl(cached = TRUE),
#'                       code = "library(random.cdisc.data)\nADSL <- radsl(cached = TRUE)")
#'
#' adae <- cdisc_dataset(dataname = "ADAE", # RelationalDataset
#'                        data = radae(cached = TRUE),
#'                        code = "library(random.cdisc.data)\nADTTE <- radae(cached = TRUE)")
#'
#' rd <- teal_data(adsl, adae)
#' get_datasets(rd)
#'
#' # RelationalDataConnector --------
#' rdc <- rcd_cdisc_data(
#'   rcd_cdisc_dataset_connector("ADSL", radsl, cached = TRUE),
#'   rcd_cdisc_dataset_connector("ADTTE", radtte, cached = TRUE)
#' )
#'
#'\dontrun{
#' load_datasets(rdc)
#' get_datasets(rdc)
#'}
#'
#' # RelationalDataList --------
#' drc <- teal_data(rdc, adae)
#'
#' get_datasets(drc)
get_datasets.RelationalData <- function(x) {
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
#' # RelationalDatasetConnector --------
#' library(random.cdisc.data)
#' rdc <- rcd_cdisc_dataset_connector("ADSL", radsl, cached = TRUE)
#'
#'\dontrun{
#' load_datasets(rdc)
#' get_datasets(rdc)
#'}
get_datasets.RawDatasetConnector <- function(x) { # nolint
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
#' # RelationalDataset --------
#' library(random.cdisc.data)
#' adsl <- cdisc_dataset(dataname = "ADSL", # RelationalDataset
#'                       data = radsl(cached = TRUE),
#'                       code = "library(random.cdisc.data)\nADSL <- radsl(cached = TRUE)")
#'
#' get_datasets(adsl)
get_datasets.RawDataset <- function(x) {
  x
}
