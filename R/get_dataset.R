#' Get dataset from \code{DatasetConnector}
#'
#' Get dataset from \code{DatasetConnector}
#' @param x \code{RawDatasetConnector} or \code{RelationalDatasetConnector})
#'
#' @return (\code{RawDataset} or \code{RelationalDataset})
#' @rdname get_dataset
#' @export
get_dataset <- function(x, dataname) {
  UseMethod("get_dataset")
}

#' @rdname get_dataset
#' @export
#' @examples
#'
#' # RawDatasetConnector --------
#' library(random.cdisc.data)
#' dc <- rcd_cdisc_dataset_connector(dataname = "ADSL", fun = radsl)
#'
#' load_dataset(dc)
#' get_dataset(dc)
get_dataset.RawDatasetConnector <- function(x, dataname = NULL) { # nolint
  if (!is.null(dataname)) {
    warning("'dataname' argument ignored - RawDatasetConnector can contain only one dataset.")
  }
  x$get_dataset()
}

#' @param dataname \code{character} a name of dataset to be retrieved
#' @rdname get_dataset
#' @export
#' @examples
#'
#' # RelationalData --------
#' library(random.cdisc.data)
#' adsl <- cdisc_dataset(dataname = "ADSL", # RelationalDataset
#'                       data = radsl(cached = TRUE),
#'                       code = "library(random.cdisc.data)\nADSL <- radsl(cached = TRUE)")
#'
#' adae <- cdisc_dataset(dataname = "ADAE", # RelationalDataset
#'                        data = radae(cached = TRUE),
#'                        code = "library(random.cdisc.data)\nADTTE <- radae(cached = TRUE)")
#'
#' rd <- teal:::RelationalData$new(adsl, adae)
#' get_dataset(rd, "ADSL")
#'
#' # RelationalDataConnector --------
#' rdc <- rcd_data(
#'   rcd_cdisc_dataset_connector("ADSL", radsl, cached = TRUE),
#'   rcd_cdisc_dataset_connector("ADTTE", radtte, cached = TRUE)
#' )
#'
#' get_dataset(rd, dataname = "ADSL")
#'\dontrun{
#' load_datasets(rdc)
#' get_datasets(rdc)
#' get_dataset(rdc, dataname = "ADSL")
#'}
#'
#' # RelationalDataList --------
#' drc <- cdisc_data(rdc, adae)
#'
#' get_dataset(drc, "ADSL")
get_dataset.RelationalData <- function(x, dataname = NULL) { # nolint
  if (is.null(dataname)) {
    stop("To get singe dataset from 'RelationalData' one must specify the name of the dataset.
         To get all datasets please use get_datasets()")
  }
  x$get_dataset(dataname = dataname)
}
