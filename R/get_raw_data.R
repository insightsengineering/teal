#' Retrieve raw data
#'
#' @param x (`Dataset`, `DatasetConnector`, `DataAbstract`)\cr
#'   object
#' @param dataname (\code{character})\cr
#'  Name of dataset to return raw data for.
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @return \code{data.frame} with the raw data inserted into the R6 objects. In case of
#' \code{DataAbstract}, \code{list} of \code{data.frame} can be returned
#' if user doesn't specify \code{dataname} - (\code{get_raw_data} from all datasets).
#'
#' @export
get_raw_data <- function(x, dataname = NULL) {
  stopifnot(is.null(dataname) || is_character_single(dataname))
  UseMethod("get_raw_data")
}

#' @export
#' @rdname get_raw_data
#' @examples
#'
#' # Dataset ---------
#' library(scda)
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#'
#' x <- dataset(dataname = "ADSL", x = ADSL)
#' get_raw_data(x)
get_raw_data.Dataset <- function(x, dataname = NULL) {
  if (!is.null(dataname)) {
    warning("'dataname' argument ignored - Dataset can contain only one dataset.")
  }
  x$get_raw_data()
}

#' @export
#' @rdname get_raw_data
#' @examples
#'
#' # DatasetConnector ---------
#' library(scda)
#' pull_fun_adsl <- callable_function(
#'   function() {synthetic_cdisc_data("latest")$adsl}
#' )
#' dc <- dataset_connector("ADSL", pull_fun_adsl)
#' load_dataset(dc)
#' get_raw_data(dc)
get_raw_data.DatasetConnector <- function(x, dataname = NULL) { # nolint
  if (!is.null(dataname)) {
    warning("'dataname' argument ignored - DatasetConnector can contain only one dataset.")
  }
  x$get_raw_data()
}

#' @rdname get_raw_data
#' @export
#' @examples
#'
#' # RelationalData ----------------
#' library(scda)
#' adsl <- cdisc_dataset(dataname = "ADSL",
#'                       x = synthetic_cdisc_data("latest")$adsl,
#'                       code = "library(scda)\nADSL <- synthetic_cdisc_data(\"latest\")$adsl")
#'
#' adtte <- cdisc_dataset(dataname = "ADTTE",
#'                        x = synthetic_cdisc_data("latest")$adtte,
#'                        code = "library(scda)\nADTTE <- synthetic_cdisc_data(\"latest\")$adtte")
#'
#' rd <- teal:::RelationalData$new(adsl, adtte)
#' get_raw_data(rd)
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
#' rdc <- relational_data_connector(
#'   connection = data_connection,
#'   connectors = list(adsl, adlb)
#' )
#'
#'\dontrun{
#' load_datasets(rdc)
#' get_raw_data(rdc)
#'}
#'
#' # RelationalData (with connectors) --------
#' drc <- cdisc_data(rdc)
#' \dontrun{
#' get_raw_data(drc)
#' }
get_raw_data.DataAbstract <- function(x, dataname = NULL) { # nolint
  if (!is.null(dataname)) {
    datasets_names <- x$get_datanames()
    if (dataname %in% datasets_names) {
      if (is_pulled(x$get_items(dataname))) {
        get_raw_data(
          get_dataset(x, dataname = dataname)
        )
      } else {
        stop(
          sprintf("'%s' has not been pulled yet\n - please use `load_dataset()` first.", dataname), call. = FALSE
        )
      }
    } else {
      stop("The dataname supplied does not exist.")
    }
  } else {
    lapply(
      get_datasets(x),
      get_raw_data
    )
  }
}
