#' Load data from connection
#'
#' Load data from connection. Function used on \link{RawDatasetConnector} and
#' \link{RawDataset} to obtain data from connection.
#'
#' @param x (\code{RawDatasetConnector} or \code{RawDataset})
#' @param args (\code{NULL} or named \code{list})\cr
#'   additional dynamic arguments passed to function which loads the data.
#' @param try (\code{logical}) whether perform function evaluation inside \code{try} clause
#' @param ... not used, only for support of S3
#'
#' @return \code{x} with loaded \code{dataset} object
#' @export
load_dataset <- function(x, ...) {
  UseMethod("load_dataset")
}

#' @rdname load_dataset
#' @examples
#'
#' # RawDataset --------
#' library(random.cdisc.data)
#' ADSL <- radsl(cached = TRUE)
#' ADSL_dataset <- raw_dataset(x = ADSL)
#'
#' load_dataset(ADSL_dataset)
#' @export
load_dataset.RawDataset <- function(x, ...) { # nolint
  return(invisible(x))
}

#' @rdname load_dataset
#' @examples
#'
#' # RawDatasetConnector --------
#' library(random.cdisc.data)
#' adsl <- rcd_cdisc_dataset_connector("ADSL", fun = radsl, cached = TRUE)
#' load_dataset(adsl)
#' get_dataset(adsl)
#'
#' adrs <- rcd_cdisc_dataset_connector("ADRS", fun = radrs, ADSL = adsl)
#' load_dataset(adrs)
#' @export
load_dataset.RawDatasetConnector <- function(x, args = NULL, try = FALSE, ...) { # nolint
  x$pull(args = args, try = try)
  return(invisible(x))
}

#' Load datasets
#' @param x (\code{object} of class \code{\link{RelationalData}}, \code{\link{RawDataset}} or
#'  \code{\link{RawDatasetConnector}})
#' @param args (\code{NULL} or named \code{list})\cr
#'   additional dynamic arguments passed to function which loads the data. Applicable only on
#'   \code{\link{RawDatasetConnector}})
#' @param try (\code{logical})\cr
#'   whether perform function evaluation inside \code{try} clause. Applicable only on
#'   \code{\link{RawDatasetConnector}})
#' @param ... (not used)\cr
#'  only for support of S3
#'
#' @export
#' @return If executed in the interactive session shiny app is opened to load the data. If executed in
#'  shiny application - it returns shiny server module.
load_datasets <- function(x) {
  UseMethod("load_datasets")
}

#' @rdname load_datasets
#' @examples
#'
#' # RawDataset ------
#' library(random.cdisc.data)
#' ADSL <- radsl(cached = TRUE)
#' ADSL_dataset <- raw_dataset(x = ADSL)
#'
#' load_datasets(ADSL_dataset)
#' @export
load_datasets.RawDataset <- function(x, ...) { # nolint
  return(invisible(x))
}

#' @rdname load_datasets
#' @examples
#'
#' # RawDatasetConnector ------
#' library(random.cdisc.data)
#' adsl <- rcd_cdisc_dataset_connector("ADSL", fun = radsl, cached = TRUE)
#' load_datasets(adsl)
#' get_dataset(adsl)
#'
#' adrs <- rcd_cdisc_dataset_connector("ADRS", fun = radrs, ADSL = adsl)
#' load_datasets(adrs)
#' @export
load_datasets.RawDatasetConnector <- function(x, args = NULL, try = FALSE, ...) { # nolint
  x$pull(args = args, try = try)
  return(invisible(x))
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
#' rd <- teal:::RelationalData$new(adsl, adtte)
#'
#' \dontrun{
#' # return a warning
#' load_datasets(rd)
#' }
load_datasets.RelationalData <- function(x, ...) {
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
#' rdc <- rcd_data(adsl, adrs)
#'
#' \dontrun{
#' load_datasets(rdc)
#' }
load_datasets.RelationalDataConnector <- function(x, ...) { # nolint
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
#' # RelationalDataList --------
#' library(random.cdisc.data)
#' adsl <- rcd_cdisc_dataset_connector(dataname = "ADSL", fun = radsl, cached = TRUE)
#' adlb <- rcd_cdisc_dataset_connector(dataname = "ADLB", fun = radlb, cached = TRUE)
#' adrs <- rcd_cdisc_dataset_connector("ADRS", radrs, ADSL = adsl)
#'
#' tc <- cdisc_data(
#'   rcd_data(adsl, adlb),
#'   rcd_data(adrs)
#' )
#'
#' \dontrun{
#' load_datasets(tc)
#' }
load_datasets.RelationalDataList <- function(x, ...) { # nolint
  if (interactive()) {
    x$launch()
  } else {
    return(invisible(x))
  }
}
