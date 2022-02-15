#' S3 method for getting a dataname(s) of
#' (`TealDataAbstract`, (`TealDatasetConnector` or
#' `TealDataset`) R6 object
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @param x (`TealDataAbstract`, `TealDatasetConnector` or
#' `TealDataset`) object
#'
#' @return dataname (`character`) A given name for the dataset(s)
#'   it may not contain spaces
#' @export
get_dataname <- function(x) {
  UseMethod("get_dataname")
}

#' @rdname get_dataname
#' @export
get_dataname.TealDataAbstract <- function(x) { # nolint
  return(x$get_datanames())
}

#' @rdname get_dataname
#' @export
get_dataname.TealDatasetConnector <- function(x) { # nolint
  return(x$get_dataname())
}


#' @rdname get_dataname
#' @export
get_dataname.TealDataset <- function(x) { # nolint
  return(x$get_dataname())
}
