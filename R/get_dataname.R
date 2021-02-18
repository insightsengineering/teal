#' S3 method for getting a dataname(s) of
#' (\code{DataAbstract}, (\code{DatasetConnector} or
#' \code{Dataset}) R6 object
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param x (\code{DataAbstract}, (\code{DatasetConnector} or
#' \code{Dataset}) R6 object
#'
#' @return dataname (\code{character}) A given name for the dataset(s)
#'   it may not contain spaces
#' @export
get_dataname <- function(x) {
  UseMethod("get_dataname")
}

#' @rdname get_dataname
#' @export
get_dataname.DataAbstract <- function(x) { # nolint
  return(x$get_datanames())
}

#' @rdname get_dataname
#' @export
get_dataname.DatasetConnector <- function(x) { # nolint
  return(x$get_dataname())
}


#' @rdname get_dataname
#' @export
get_dataname.Dataset <- function(x) { # nolint
  return(x$get_dataname())
}
