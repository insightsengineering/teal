#' S3 method for getting a label of
#' (\code{RelationalDatasetConnector} or \code{NamedDataset}) R6 object
#'
#' @md
#' @description `r lifecycle::badge("experimental")`
#'
#' @param x (\code{RelationalDatasetConnector} or \code{NamedDataset}) R6 object
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
#' x <- named_dataset_connector(
#'  pull_callable = fun,
#'  dataname = "ADSL"
#' )
#' get_dataset_label(x)
#'
#' x1 <- relational_dataset_connector(
#'  dataname = "ADSL",
#'  pull_callable = fun,
#'  keys = get_cdisc_keys("ADSL")
#' )
#' get_dataset_label(x1)
get_dataset_label.NamedDatasetConnector <- function(x) { # nolint

  return(x$get_dataset_label())

}

#' @rdname get_dataset_label
#' @export
#' @examples
#' library(random.cdisc.data)
#' ADSL <- radsl(cached = TRUE)
#' ADSL_dataset <- named_dataset(dataname = "ADSL", x = ADSL)
#' get_dataset_label(ADSL_dataset)
get_dataset_label.NamedDataset <- function(x) {

  return(x$get_dataset_label())

}
