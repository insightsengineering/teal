#' Retrieve raw data
#'
#' @md
#' @param x (\code{RawDataset, RawDatasetConnector, RelationalData, cdisc_data})\cr object
#' @param dataname (\code{character} value)\cr
#'  Name of dataset to return raw data for.
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @return \code{data.frame} with the raw data inserted into the R6 objects. In case of
#' \code{RelationalData} or \code{cdisc_data} \code{list} of \code{data.frame} can be returned
#' if user doesn't specify \code{dataname} (\code{get_raw_data} from all datasets).
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
#' # RawDataset ---------
#' library(random.cdisc.data)
#' ADSL <- radsl(cached = TRUE)
#' ADSL_raw <- raw_dataset(x = ADSL)
#' get_raw_data(ADSL_raw)
#'
#' ADSL_named <- named_dataset(dataname = "ADSL", x = ADSL)
#' get_raw_data(ADSL_named)
#'
#' ADSL_relational <- as_relational(ADSL_raw,
#'   dataname = "ADSL",
#'   keys = keys(primary = c("USUBJID", "STUDYID"), foreign = NULL, parent = NULL)
#' )
#' get_raw_data(ADSL_relational)
get_raw_data.RawDataset <- function(x, dataname = NULL) {
  if (!is.null(dataname)) {
    warning("'dataname' argument ignored - RawDataset can contain only one dataset.")
  }
  x$get_raw_data()
}

#' @export
#' @rdname get_raw_data
#' @examples
#'
#' # RawDatasetConnector ---------
#' library(random.cdisc.data)
#' dc <- rcd_cdisc_dataset_connector(dataname = "ADSL", fun = radsl, cached = TRUE)
#' load_dataset(dc)
#' get_raw_data(dc)
get_raw_data.RawDatasetConnector <- function(x, dataname = NULL) { # nolint
  if (!is.null(dataname)) {
    warning("'dataname' argument ignored - RawDatasetConnector can contain only one dataset.")
  }
  x$get_raw_data()
}

#' @rdname get_raw_data
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
#' get_raw_data(rd)
#'
#' # RelationalDataConnector --------
#' rdc <- rcd_data(
#'   rcd_cdisc_dataset_connector("ADSL", radsl, cached = TRUE),
#'   rcd_cdisc_dataset_connector("ADTTE", radtte, cached = TRUE)
#' )
#'
#'\dontrun{
#' load_datasets(rdc)
#' get_raw_data(rdc)
#'}
#'
#' # RelationalData (with connectors) --------
#' rdc2 <- rcd_data(
#'   rcd_cdisc_dataset_connector("ADRS", radrs, cached = TRUE),
#'   rcd_cdisc_dataset_connector("ADLB", radlb, cached = TRUE)
#' )
#'
#' drc <- cdisc_data(adsl, adtte, rdc2)
#' \dontrun{
#' get_raw_data(drc)
#' }
get_raw_data.RelationalDataCollection <- function(x, dataname = NULL) { # nolint
  if (!is.null(dataname)) {
    datasets_names <- x$get_datanames()
    if (dataname %in% datasets_names) {
      if (is_pulled(x$get_items(dataname))) {
        get_raw_data(
          get_dataset(x, dataname = dataname)
        )
      } else {
        stop(
          sprintf("'%s' has not been pulled yet\n - please use `load_dataset()` first.",
                  dataname),
          call. = FALSE
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
