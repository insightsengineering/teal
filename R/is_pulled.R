#' Is pulled
#'
#' @description S3 method to determine if dataset is pulled (loaded).
#' @param x (\code{object}) of class \link{RawDatasetConnector}, \link{RelationalDataset},
#'   \link{RelationalDatasetConnector}, or \link{RelationalDataConnector}.
#'
#' @return \code{TRUE} if connector has been already pulled, else \code{FALSE}.
#' @export
is_pulled <- function(x) {
  UseMethod("is_pulled")
}

#' @rdname is_pulled
#' @export
#'
#' @examples
#' # RawDatasetConnector --------
#' ds <- raw_dataset_connector(pull_fun = callable_function(data.frame))
#' set_args(ds, list(x = 1:5, y = letters[1:5]))
#'
#' is_pulled(ds)
#'
#' load_dataset(ds)
#' is_pulled(ds)
is_pulled.RawDatasetConnector <- function(x) {
  return(x$is_pulled())
}

#' @rdname is_pulled
#' @export
#'
#' @examples
#' rel_data <- teal:::RelationalDataset$new(
#'   x = data.frame(x = c(1, 2), y = c("a", "b"), stringsAsFactors = FALSE),
#'   keys = keys(primary = "y", foreign = NULL, parent = NULL),
#'   dataname = "XY",
#'   code = "XY <- data.frame(x = c(1, 2), y = c('aa', 'bb'),
#'                            stringsAsFactors = FALSE)"
#' )
#'
#' is_pulled(rel_data)
is_pulled.RawDataset <- function(x) {
  return(x$is_pulled())
}

#' @rdname is_pulled
#' @export
#'
#' @examples
#'
#' # RelationalDataConnector --------
#' library(random.cdisc.data)
#' adsl <- rcd_cdisc_dataset_connector(dataname = "ADSL", fun = radsl, cached = TRUE)
#' adrs <- rcd_cdisc_dataset_connector(dataname = "ADRS", fun = radrs, ADSL = adsl)
#'
#' rdc <- rcd_cdisc_data(adsl, adrs)
#'
#' is_pulled(rdc)
#' \dontrun{
#' load_datasets(rdc)
#' is_pulled(rdc)
#' }
is_pulled.RelationalDataConnector <- function(x) { # nolint
  return(x$is_pulled())
}

#' @rdname is_pulled
#' @export
is_pulled.RelationalData <- function(x) { # nolint
  return(x$is_pulled())
}
