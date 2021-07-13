#' Get dataset from `DatasetConnector`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Get dataset from \code{DatasetConnector}
#' @param x (`DatasetConnector` or `DatasetConnector` or `DataAbstract`)
#' @param dataname \code{character} a name of dataset to be retrieved
#'
#' @return (`Dataset`)
#' @export
get_dataset <- function(x, dataname) {
  UseMethod("get_dataset")
}

#' @rdname get_dataset
#' @export
#' @examples
#'
#' # DatasetConnector --------
#' library(random.cdisc.data)
#' ADSL <- radsl(cached = TRUE)
#'
#' dc <- rcd_dataset_connector(dataname = "ADAE", fun = radae,
#'                             ADSL = ADSL, max_n_aes = 2L)
#'
#' load_dataset(dc)
#' get_dataset(dc)
get_dataset.DatasetConnector <- function(x, dataname = NULL) { # nolint
  if (!is.null(dataname)) {
    warning("'dataname' argument ignored - DatasetConnector can contain only one dataset.")
  }
  return(x$get_dataset())
}

#' @rdname get_dataset
#' @export
#' @examples
#'
#' # Dataset --------
#' library(random.cdisc.data)
#' ADSL <- radsl(cached = TRUE)
#' x <- dataset("ADSL", ADSL)
#'
#' get_dataset(x)
get_dataset.Dataset <- function(x, dataname = NULL) { # nolint
  if (!is.null(dataname)) {
    warning("'dataname' argument ignored - Dataset can contain only one dataset.")
  }
  return(x$get_dataset())
}

#' @rdname get_dataset
#' @export
#' @examples
#'
#' # RelationalData  (not containing connectors) --------
#' library(random.cdisc.data)
#' adsl <- cdisc_dataset(dataname = "ADSL",
#'                       x = radsl(cached = TRUE),
#'                       code = "library(random.cdisc.data)\nADSL <- radsl(cached = TRUE)")
#'
#' adae <- cdisc_dataset(dataname = "ADAE",
#'                        x = radae(cached = TRUE),
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
#'\dontrun{
#' get_dataset(rdc, dataname = "ADSL")
#' load_datasets(rdc)
#' get_datasets(rdc)
#' get_dataset(rdc, dataname = "ADSL")
#'}
#'
#' # RelationalData (containing connectors) --------
#' rd <- cdisc_data(rdc, adae)
#'
#'\dontrun{
#' get_dataset(rd, "ADSL")
#' }
get_dataset.DataAbstract <- function(x, dataname = NULL) {
  if (is.null(dataname)) {
    stop(paste("To get single dataset from data class one must specify the name of the dataset.",
               "To get all datasets please use get_datasets()"))
  }
  return(x$get_dataset(dataname = dataname))
}
