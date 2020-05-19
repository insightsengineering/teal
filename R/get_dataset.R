#' Get dataset from \code{DatasetConnector}
#'
#' Get dataset from \code{DatasetConnector}
#' @param x \code{RawDatasetConnector} or \code{RelationalDatasetConnector})
#'
#' @return (\code{RawDataset} or \code{RelationalDataset})
#' @rdname get_dataset
#' @export
get_dataset <- function(x) {
  UseMethod("get_dataset")
}

#' @rdname get_dataset
#' @export
#' @examples
#' library(random.cdisc.data)
#' fun <- callable_function(radsl)
#' fun$set_args(list(N = 5, seed = 1, cached = TRUE))
#' x <- raw_dataset_connector(fun)
#'
#' load_dataset(x)
#' ds <- get_dataset(x)
get_dataset.RawDatasetConnector <- function(x) { # nolint
  x$get_dataset()
}
