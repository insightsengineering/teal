#' Is pulled
#'
#' @description S3 method to determine if dataset is pulled (loaded).
#' @param x (\code{object}) of class \link{RawDatasetConnector}, \link{RelationalDataset},
#'   \link{RelationalDatasetConnector}, or \link{RelationalDataConnector}.
#'
#' @return (\code{logical}) \code{TRUE} if connector has been already pulled, else \code{FALSE}.
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
#' set_args(ds, list(x = 1:5, y = letters[1:5], stringsAsFactors = FALSE))
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
#' rel_data <- relational_dataset(
#'   dataname = "XY",
#'   x = data.frame(x = c(1, 2), y = c("a", "b"), stringsAsFactors = FALSE),
#'   keys = keys(primary = "y", foreign = NULL, parent = NULL),
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
#' # RelationalData --------
#' library(random.cdisc.data)
#' x1 <- relational_dataset(
#'   x = radsl(cached = TRUE),
#'   dataname = "ADSL",
#'   keys = get_cdisc_keys("ADSL"),
#'   code = "ADSL <- radsl(cached = TRUE)",
#'   label = "ADTTE dataset"
#' )
#'
#' x2 <- relational_dataset(
#'   x = radtte(cached = TRUE),
#'   dataname = "ADTTE",
#'   keys = get_cdisc_keys("ADTTE"),
#'   code = "ADTTE <- radtte(cached = TRUE)",
#'   label = "ADTTE dataset"
#' )
#'
#' rd <- teal_data(x1, x2)
#' is_pulled(rd)
#'
#' # RelationalDataConnector --------
#' library(random.cdisc.data)
#' adsl <- rcd_cdisc_dataset_connector(dataname = "ADSL", fun = radsl, cached = TRUE)
#' adrs <- rcd_cdisc_dataset_connector(dataname = "ADRS", fun = radrs, ADSL = adsl)
#'
#' rdc <- rcd_data(adsl, adrs)
#'
#' is_pulled(rdc)
#' \dontrun{
#' load_datasets(rdc)
#' is_pulled(rdc)
#' }
is_pulled.RelationalData <- function(x) { # nolint
  return(x$is_pulled())
}
