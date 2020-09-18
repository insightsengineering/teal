#' S3 method for getting a dataname(s) of
#' (\code{RelationalDataConnector}, (\code{RelationalDatasetConnector} or
#' \code{NamedDataset}) R6 object
#'
#' @md
#' @description `r lifecycle::badge("experimental")`
#'
#' @param x (\code{RelationalDataConnector}, (\code{RelationalDatasetConnector} or
#' \code{NamedDataset}) R6 object
#'
#' @return dataname (\code{character}) A given name for the dataset(s)
#'   it may not contain spaces
#' @export
get_dataname <- function(x) {
  UseMethod("get_dataname")
}

#' @rdname get_dataname
#' @export
get_dataname.RelationalDataCollection <- function(x) { # nolint
  return(x$get_datanames())
}

#' @rdname get_dataname
#' @export
get_dataname.NamedDatasetConnector <- function(x) { # nolint
  return(x$get_dataname())
}


#' @rdname get_dataname
#' @export
get_dataname.NamedDataset <- function(x) { # nolint
  return(x$get_dataname())
}
