#' Get dataset from `TealDatasetConnector`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Get dataset from `TealDatasetConnector`
#' @param x (`TealDatasetConnector` or `TealDatasetConnector` or `TealDataAbstract`)
#' @param dataname (`character`) a name of dataset to be retrieved
#' @details See `help(TealDataConnector)` and `help(TealData)` for more complex examples.
#' @return (`TealDataset`)
#' @export
get_dataset <- function(x, dataname) {
  UseMethod("get_dataset")
}

#' @rdname get_dataset
#' @export
#' @examples
#'
#' # TealDatasetConnector --------
#' library(scda)
#' pull_fun_adae <- callable_function(
#'   function() {
#'     synthetic_cdisc_data("latest")$adae
#'   }
#' )
#'
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#'
#' dc <- dataset_connector(
#'   dataname = "ADAE", pull_callable = pull_fun_adae,
#'   keys = get_cdisc_keys("ADSL")
#' )
#' load_dataset(dc)
#' get_dataset(dc)
get_dataset.TealDatasetConnector <- function(x, dataname = NULL) { # nolint
  if (!is.null(dataname)) {
    warning("'dataname' argument ignored - TealDatasetConnector can contain only one dataset.")
  }
  return(x$get_dataset())
}

#' @rdname get_dataset
#' @export
#' @examples
#'
#' # TealDataset --------
#' library(scda)
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#' x <- dataset("ADSL", ADSL)
#'
#' get_dataset(x)
get_dataset.TealDataset <- function(x, dataname = NULL) { # nolint
  if (!is.null(dataname)) {
    warning("'dataname' argument ignored - TealDataset can contain only one dataset.")
  }
  return(x$get_dataset())
}

#' @rdname get_dataset
#' @export
#' @examples
#'
#' # TealData  (not containing connectors) --------
#' library(scda)
#' adsl <- cdisc_dataset(
#'   dataname = "ADSL",
#'   x = synthetic_cdisc_data("latest")$adsl,
#'   code = "library(scda)\nADSL <- synthetic_cdisc_data(\"latest\")$adsl"
#' )
#'
#' adae <- cdisc_dataset(
#'   dataname = "ADAE",
#'   x = synthetic_cdisc_data("latest")$adsl,
#'   code = "library(scda)\nADTTE <- synthetic_cdisc_data(\"latest\")$adsl"
#' )
#'
#' rd <- teal:::TealData$new(adsl, adae)
#' get_dataset(rd, "ADSL")
get_dataset.TealDataAbstract <- function(x, dataname = NULL) {
  if (is.null(dataname)) {
    stop(paste(
      "To get single dataset from data class one must specify the name of the dataset.",
      "To get all datasets please use get_datasets()"
    ))
  }
  return(x$get_dataset(dataname = dataname))
}
