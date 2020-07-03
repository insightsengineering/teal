#' S3 method for getting a dataname of
#' (\code{RelationalDatasetConnector } or \code{NamedDataset}) R6 object
#'
#' @param x (\code{RelationalDatasetConnector} or \code{NamedDataset}) R6 object
#'
#' @return dataname (\code{character}) A given name for the dataset
#'   it may not contain spaces
#' @rdname get_dataname
#' @export
get_dataname <- function(x) {
  UseMethod("get_dataname")
}

#' @rdname get_dataname
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
#' get_dataname(x)
get_dataname.RelationalDatasetConnector <- function(x) { # nolint
    return(x$get_dataname())
}

#' @rdname get_dataname
#' @export
#' @examples
#' library(random.cdisc.data)
#' ADSL <- radsl(cached = TRUE)
#' ADSL_dataset <- named_dataset(dataname = "ADSL", x = ADSL)
#' get_dataname(ADSL_dataset)
get_dataname.NamedDataset <- function(x) {
    return(x$get_dataname())
}

#' @rdname get_dataname
#' @export
#' @examples
#' library(random.cdisc.data)
#' ADSL <- radsl(cached = TRUE)
#' ADSL_dataset <- named_dataset(dataname = "ADSL", x = ADSL)
#' get_dataname(ADSL_dataset)
get_dataname.NamedDataset <- function(x) {
    return(x$get_dataname())
}
