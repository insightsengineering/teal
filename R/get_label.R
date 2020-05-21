#' S3 method for getting a label of
#' (\code{RelationalDatasetConnector }or \code{NamedDataset}) R6 object
#'
#' @param x (\code{RelationalDatasetConnector} or \code{NamedDataset}) R6 object
#'
#' @return label (\code{character}) Label to describe the dataset
#' @rdname get_label
#' @export
get_label <- function(x) {

  UseMethod("get_label")

}

#' @rdname get_label
#' @export
#' @examples
#' library(random.cdisc.data)
#' fun <- callable_function(data.frame)
#' fun$set_args(list(n = 5, seed = 1, cached = TRUE))
#'
#' x <- relational_dataset_connector(
#'  pull_fun = fun,
#'  dataname = "ADSL",
#'  keys = get_cdisc_keys("ADSL")
#' )
#' get_label(x)
get_label.RelationalDatasetConnector <- function(x) { # nolint

    return(x$get_label())

}

#' @rdname get_label
#' @export
#' @examples
#' library(random.cdisc.data)
#' ADSL <- radsl(cached = TRUE)
#' ADSL_dataset <- named_dataset(dataname = "ADSL", x = ADSL)
#' get_label(ADSL_dataset)
get_label.NamedDataset <- function(x) {

    return(x$get_label())

}
