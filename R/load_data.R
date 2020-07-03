#' Load data from connection
#'
#' Load data from connection. Function used on \link{RawDatasetConnector} and
#' \link{RelationalDatasetConnector} to obtain data from connection.
#' @param x (\code{RawDatasetConnector} or \code{RelationalDatasetConnection})
#'
#' @param args (\code{NULL} or named \code{list})\cr
#'   additional dynamic arguments passed to function which loads the data.
#'
#' @param try (\code{logical}) whether perform function evaluation inside \code{try} clause
#'
#' @return \code{x} with loaded \code{dataset} object
#' @export
#' @rdname load_dataset
load_dataset <- function(x, args, try) {
  UseMethod("load_dataset")
}

#' @rdname load_dataset
#' @examples
#'
#' @export
load_dataset.RawDatasetConnector <- function(x, args = NULL, try = FALSE) { # nolint
  x$pull(args = args, try = try)
  return(invisible(x))
}

#' @rdname load_dataset
#' @examples
#'
#' # RelationalDatasetConnector ---------
#' library(random.cdisc.data)
#' adsl <- rcd_cdisc_dataset_connector(dataname = "ADSL", fun = radsl, cached = TRUE)
#' load_dataset(adsl)
#' get_dataset(adsl)
#'
#' adrs <- rcd_cdisc_dataset_connector(dataname = "ADRS", fun = radrs, ADSL = adsl)
#' load_dataset(adrs)
#' @export
load_dataset.RelationalDatasetConnector <- function(x, args = NULL, try = FALSE) { # nolint
  x$pull(args = args, try = try)
  return(invisible(x))
}

#' Load datasets
#' @param x (\code{object}) of class\link{RawDatasetConnector} or \link{NamedDataset}. If of
#'   class \code{character} will be treated as file to read.
#' @export
#' @rdname load_datasets
#' @return object of the same class as \code{x}
load_datasets <- function(x) {
  UseMethod("load_datasets")
}

#' @rdname load_datasets
#' @export
#' @examples
#'
#' # RelationalData ------
#' library(random.cdisc.data)
#' adsl <- cdisc_dataset(dataname = "ADSL", # RelationalDataset
#'                       data = radsl(cached = TRUE),
#'                       code = "library(random.cdisc.data)\nADSL <- radsl(cached = TRUE)")
#'
#' adtte <- cdisc_dataset(dataname = "ADTTE", # RelationalDataset
#'                        data = radtte(cached = TRUE),
#'                        code = "library(random.cdisc.data)\nADTTE <- radtte(cached = TRUE)")
#'
#' rd <- teal_data(adsl, adtte)
#'
#' \dontrun{
#' # return a warning
#' load_datasets(rd)
#' }
load_datasets.RelationalData <- function(x) {
  warning("This is a RelationalData object so data is already loaded")
}

#' @rdname load_datasets
#' @export
#' @examples
#'
#' # RelationalDataConnector --------
#' library(random.cdisc.data)
#' adsl <- rcd_cdisc_dataset_connector(dataname = "ADSL", fun = radsl, cached = TRUE)
#' adrs <- rcd_cdisc_dataset_connector(dataname = "ADRS", fun = radrs, ADSL = adsl)
#'
#' rdc <- rcd_cdisc_data(adsl, adrs)
#'
#' \dontrun{
#' load_datasets(rdc)
#' }
load_datasets.RelationalDataConnector <- function(x) { # nolint
  if (interactive()) {
    x$launch()
  } else {
    return(invisible(x))
  }
}

#' @rdname load_datasets
#' @export
#' @examples
#'
#' # DelayedRelationalData --------
#' library(random.cdisc.data)
#' adsl <- rcd_cdisc_dataset_connector(dataname = "ADSL", fun = radsl, cached = TRUE)
#' adlb <- rcd_cdisc_dataset_connector(dataname = "ADLB", fun = radlb, cached = TRUE)
#' adrs <- rcd_cdisc_dataset_connector("ADRS", radrs, ADSL = adsl)
#'
#' tc <- teal_data(
#'   rcd_cdisc_data(adsl, adlb),
#'   rcd_cdisc_data(adrs)
#' )
#'
#' \dontrun{
#' load_datasets(tc)
#' }
load_datasets.DelayedRelationalData <- function(x) { # nolint
  if (interactive()) {
    x$get_server()
  } else {
    return(invisible(x))
  }
}
