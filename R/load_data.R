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
load_dataset.RawDatasetConnector <- function(x, args = NULL, try = FALSE) { # nolint
  x$pull(args = args, try = try)
  return(invisible(x))
}
