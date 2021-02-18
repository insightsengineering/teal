#' Is pulled
#'
#' @description `r lifecycle::badge("experimental")`
#'   S3 method to determine if dataset is pulled (loaded).
#'
#' @param x (\code{object}) of class \code{\link{DatasetConnector}}, \code{\link{Dataset}} or \code{\link{DataAbstract}}
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
#' # DatasetConnector --------
#' library(random.cdisc.data)
#' x <- rcd_dataset_connector("ADSL", radsl)
#'
#' is_pulled(x)
#'
#' load_dataset(x)
#' is_pulled(x)
is_pulled.DatasetConnector <- function(x) {
  return(x$is_pulled())
}

#' @rdname is_pulled
#' @export
#'
#' @examples
#' # Dataset --------
#' x <- dataset(
#'   dataname = "XY",
#'   x = data.frame(x = c(1, 2), y = c("a", "b"), stringsAsFactors = FALSE),
#'   keys = "y",
#'   code = "XY <- data.frame(x = c(1, 2), y = c('aa', 'bb'),
#'                            stringsAsFactors = FALSE)"
#' )
#'
#' is_pulled(x)
is_pulled.Dataset <- function(x) {
  return(x$is_pulled())
}

#' @rdname is_pulled
#' @export
#'
#' @examples
#' # RelationalData --------
#' library(random.cdisc.data)
#' x1 <- dataset(
#'   x = radsl(cached = TRUE),
#'   dataname = "ADSL",
#'   keys = get_cdisc_keys("ADSL"),
#'   code = "ADSL <- radsl(cached = TRUE)",
#'   label = "ADTTE dataset"
#' )
#'
#' x2 <- dataset(
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
is_pulled.DataAbstract <- function(x) { # nolint
  return(x$is_pulled())
}
