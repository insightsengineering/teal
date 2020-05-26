#' Retrieve raw data
#'
#' @param x (\link{RawDataset} or \link{RawDatasetConnector}) object
#'
#' @return \code{data.frame} with the raw data inserted into the
#'   R6 objects
#'
#' @export
#' @examples
#'
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
#' @name get_raw_data
get_raw_data <- function(x) {
  UseMethod("get_raw_data")
}

#' @export
#' @rdname get_raw_data
get_raw_data.RawDataset <- function(x) { #nolint
  x$get_raw_data()
}

#' @export
#' @rdname get_raw_data
get_raw_data.RawDatasetConnector <- function(x) { #nolint
  x$get_raw_data()
}
