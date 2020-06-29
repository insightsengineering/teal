#' Get a cdisc data object.
#'
#' @param x (\code{object}) of class\link{RawDatasetConnector} or \link{NamedDataset}. If of
#'   class \code{character} will be treated as file to read.
#' @export
#' @return \code{cdisc_data} object
get_cdisc_data <- function(x) {
  UseMethod("get_cdisc_data")
}

#' @rdname get_cdisc_data
#' @export
#' @examples
#'
#' # RelationalData ----------------
#' library(random.cdisc.data)
#' adsl <- cdisc_dataset(dataname = "ADSL", # RelationalDataset
#'                       data = radsl(cached = TRUE),
#'                       code = "library(random.cdisc.data)\nADSL <- radsl(cached = TRUE)")
#'
#' adtte <- cdisc_dataset(dataname = "ADTTE", # RelationalDataset
#'                        data = radtte(cached = TRUE),
#'                        code = "library(random.cdisc.data)\nADTTE <- radtte(cached = TRUE)")
#'
#' rd <- teal:::RelationalData$new(adsl, adtte)
#' get_cdisc_data(rd)
get_cdisc_data.RelationalData <- function(x) {
  datasets <- get_datasets(x)
  if (!is.null(datasets)) {
    x$get_cdisc_data()
  } else {
    return(invisible(NULL))
  }
}
#' @rdname get_dataset
#' @export
#' @examples
#' library(random.cdisc.data)
#' rdc <- rcd_cdisc_data(
#'   rcd_cdisc_dataset_connector("ADSL", radsl, cached = TRUE),
#'   rcd_cdisc_dataset_connector("ADTTE", radtte, cached = TRUE)
#' )
#'
#' \dontrun{
#' get_cdisc_data(rdc)
#' load_datasets(rdc)
#' get_cdisc_data(rdc)
#' }
#'
get_cdisc_data.RelationalDataConnector <- function(x) { # nolint
  datasets <- get_datasets(x)
  if (!is.null(datasets)) {
    do.call("cdisc_data", datasets)
  } else {
    warning("`load_datasets()` should be used previously")
    return(invisible(NULL))
  }
}
#' @rdname get_dataset
#' @export
#' @examples
#' library(random.cdisc.data)
#' adsl <- cdisc_dataset(dataname = "ADSL", # RelationalDataset
#'                       data = radsl(cached = TRUE),
#'                       code = "library(random.cdisc.data)\nADSL <- radsl(cached = TRUE)")
#'
#' adtte <- cdisc_dataset(dataname = "ADTTE", # RelationalDataset
#'                        data = radtte(cached = TRUE),
#'                        code = "library(random.cdisc.data)\nADTTE <- radtte(cached = TRUE)")
#'
#'
#' rdc <- rcd_cdisc_data(
#'   rcd_cdisc_dataset_connector("ADRS", radrs, cached = TRUE),
#'   rcd_cdisc_dataset_connector("ADLB", radlb, cached = TRUE)
#' )
#'
#' drc <- teal_data(adsl, adtte, rdc)
#'
#' # get adsl and adtte (present already)
#' get_cdisc_data(drc)
#'
#' \dontrun{
#' # load rdc
#' load_datasets(drc)
#' get_cdisc_data(drc)
#' }
get_cdisc_data.DelayedDataConnector <- function(x) { # nolint
  datasets <- get_datasets(x)
  if (!is.null(datasets)) {
    do.call("cdisc_data", datasets)
  } else {
    warning("`load_datasets()` should be used previously")
    return(invisible(NULL))
  }
}
