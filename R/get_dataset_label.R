#' S3 method for getting a label of
#' (`TealDatasetConnector` or `TealDataset`) R6 object
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @param x (`TealDatasetConnector` or `TealDataset`) R6 object
#'
#' @return label (`character`) Label to describe the dataset
#' @export
get_dataset_label <- function(x) {
  UseMethod("get_dataset_label")
}

#' @rdname get_dataset_label
#' @export
#' @examples
#' fun <- callable_function(data.frame)
#' fun$set_args(list(c1 = seq_len(10)))
#'
#' x <- dataset_connector(
#'   pull_callable = fun,
#'   dataname = "ADSL",
#'   label = "My custom label"
#' )
#' get_dataset_label(x)
get_dataset_label.TealDatasetConnector <- function(x) { # nolint
  return(x$get_dataset_label())
}

#' @rdname get_dataset_label
#' @export
#' @examples
#' library(scda)
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#' ADSL_dataset <- dataset(dataname = "ADSL", x = ADSL, label = "My custom label")
#' get_dataset_label(ADSL_dataset)
get_dataset_label.TealDataset <- function(x) {
  return(x$get_dataset_label())
}
