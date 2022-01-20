#' Is pulled
#'
#' @description `r lifecycle::badge("stable")`
#'   S3 method to determine if dataset is pulled (loaded).
#'
#' @param x ([`TealDatasetConnector`], [`TealDataset`] or [`TealDataAbstract`])
#'
#' @return (`logical`) `TRUE` if connector has been already pulled, else `FALSE`.
#' @export
is_pulled <- function(x) {
  UseMethod("is_pulled")
}

#' @rdname is_pulled
#' @export
#'
#' @examples
#' # TealDatasetConnector --------
#' library(scda)
#' pull_fun_adsl <- callable_function(
#'   function() {
#'     synthetic_cdisc_data("latest")$adsl
#'   }
#' )
#' x <- dataset_connector("ADSL", pull_fun_adsl)
#'
#' is_pulled(x)
#'
#' load_dataset(x)
#' is_pulled(x)
is_pulled.TealDatasetConnector <- function(x) {
  return(x$is_pulled())
}

#' @rdname is_pulled
#' @export
#'
#' @examples
#' # TealDataset --------
#' x <- dataset(
#'   dataname = "XY",
#'   x = data.frame(x = c(1, 2), y = c("a", "b"), stringsAsFactors = FALSE),
#'   keys = "y",
#'   code = "XY <- data.frame(x = c(1, 2), y = c('aa', 'bb'),
#'                            stringsAsFactors = FALSE)"
#' )
#'
#' is_pulled(x)
is_pulled.TealDataset <- function(x) {
  return(x$is_pulled())
}

#' @rdname is_pulled
#' @export
#'
#' @examples
#' # TealData --------
#' library(scda)
#' x1 <- dataset(
#'   x = synthetic_cdisc_data("latest")$adsl,
#'   dataname = "ADSL",
#'   keys = get_cdisc_keys("ADSL"),
#'   code = "ADSL <- synthetic_cdisc_data(\"latest\")$adsl",
#'   label = "ADTTE dataset"
#' )
#'
#' x2 <- dataset(
#'   x = synthetic_cdisc_data("latest")$adtte,
#'   dataname = "ADTTE",
#'   keys = get_cdisc_keys("ADTTE"),
#'   code = "ADTTE <- synthetic_cdisc_data(\"latest\")$adtte",
#'   label = "ADTTE dataset"
#' )
#'
#' rd <- teal_data(x1, x2)
#' is_pulled(rd)
#'
#' # TealDataConnector --------
#' library(scda)
#' adsl_cf <- callable_function(
#'   function() {
#'     synthetic_cdisc_data("latest")$adsl
#'   }
#' )
#' adsl <- cdisc_dataset_connector(
#'   dataname = "ADSL",
#'   pull_callable = adsl_cf,
#'   keys = get_cdisc_keys("ADSL")
#' )
#'
#' new_cf <- callable_function(function(x) {
#'   x$NEW <- 1:nrow(x)
#'   x
#' })
#' new_cf$set_args(list(x = as.name("x")))
#' new <- cdisc_dataset_connector(
#'   dataname = "NEW",
#'   pull_callable = new_cf,
#'   keys = get_cdisc_keys("ADSL"),
#'   vars = list(x = adsl)
#' )
#'
#' rdc <- cdisc_data(adsl, new)
#'
#' is_pulled(rdc)
#' \dontrun{
#' load_datasets(rdc)
#' is_pulled(rdc)
#' }
is_pulled.TealDataAbstract <- function(x) { # nolint
  return(x$is_pulled())
}
