#' S3 method for getting a label of
#' (\code{DatasetConnector} or \code{Dataset}) R6 object
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param x (\code{DatasetConnector} or \code{Dataset}) R6 object
#'
#' @return label (\code{character}) Label to describe the dataset
#' @export
get_dataset_label <- function(x) {
  UseMethod("get_dataset_label")
}

#' @rdname get_dataset_label
#' @export
#' @examples
#' library(random.cdisc.data)
#' fun <- callable_function(data.frame)
#' fun$set_args(list(n = 5, seed = 1, cached = TRUE))
#'
#' x <- dataset_connector(
#'  pull_callable = fun,
#'  dataname = "ADSL",
#'  label = "My custom label"
#' )
#' get_dataset_label(x)
get_dataset_label.DatasetConnector <- function(x) { # nolint
  return(x$get_dataset_label())
}

#' @rdname get_dataset_label
#' @export
#' @examples
#' library(random.cdisc.data)
#' ADSL <- radsl(cached = TRUE)
#' ADSL_dataset <- dataset(dataname = "ADSL", x = ADSL, label = "My custom label")
#' get_dataset_label(ADSL_dataset)
get_dataset_label.Dataset <- function(x) {
  return(x$get_dataset_label())
}
