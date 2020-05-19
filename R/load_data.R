#' Load data from connection
#'
#' Load data from connection. Function used on \link{RawDatasetConnector} and
#' \link{RelationalDatasetConnector} to obtain data from connection.
#' @param x (\code{RawDatasetConnector} or \code{RelationalDatasetConnection})
#' @return \code{x} with loaded \code{dataset} object
#' @export
#' @rdname load_dataset
load_dataset <- function(x) {
  UseMethod("load_dataset")
}

#' @rdname load_dataset
#' @examples
#' library(random.cdisc.data)
#' # for RawDatasetConnector
#' fun <- callable_function(radsl)
#' fun$set_args(list(N = 5, seed = 1, cached = TRUE))
#' x <- raw_dataset_connector(fun)
#' load_dataset(x)
#'
#' # for RelationalDatasetConnector
#' \dontrun{
#' x2 <- as_relational(x, dataname = "ADSL")
#' load_dataset(x2)
#' }
#' @export
load_dataset.RawDatasetConnector <- function(x) { # nolint
  x$pull()
  return(invisible(x))
}
