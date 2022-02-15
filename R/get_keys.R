#' Get dataset primary keys
#'
#' @description `r lifecycle::badge("stable")`
#' Get dataset primary keys
#'
#' @param x an object of `TealDataset` or `TealDatasetConnector` class
#' @param dataname (`character`) name of dataset to return keys for
#' @param ... not used, only for support of S3
#'
#' @return (`character`) vector of column names
#'
#' @export
get_keys <- function(x, ...) {
  UseMethod("get_keys")
}

#' @rdname get_keys
#' @export
#' @examples
#' # TealDataset --------
#'
#' library(scda)
#' get_keys(
#'   dataset(
#'     "ADSL",
#'     synthetic_cdisc_data("latest")$adsl,
#'     keys = get_cdisc_keys("ADSL"),
#'     code = "ADSL <- synthetic_cdisc_data(\"latest\")$adsl"
#'   )
#' )
get_keys.TealDataset <- function(x, ...) {
  check_ellipsis(...)
  x$get_keys()
}

#' @rdname get_keys
#' @export
#' @examples
#' # TealDatasetConnector --------
#'
#' library(scda)
#' pull_fun_adsl <- callable_function(
#'   function() {
#'     synthetic_cdisc_data("latest")$adsl
#'   }
#' )
#' get_keys(
#'   dataset_connector(
#'     "ADSL",
#'     pull_fun_adsl,
#'     keys = get_cdisc_keys("ADSL"),
#'   )
#' )
get_keys.TealDatasetConnector <- function(x, ...) {
  check_ellipsis(...)
  x$get_keys()
}

#' @rdname get_keys
#' @export
#' @examples
#' # TealData --------
#'
#' get_keys(
#'   teal_data(
#'     dataset("x", data.frame(x1 = 1:10, y1 = 11:20), keys = "x1"),
#'     dataset("y", data.frame(x2 = 1:10, y2 = 11:20), keys = "x2")
#'   ),
#'   "x"
#' )
get_keys.TealDataAbstract <- function(x, dataname, ...) {
  check_ellipsis(...)
  get_keys(x$get_items(dataname))
}



#' Set dataset primary keys
#'
#' @description `r lifecycle::badge("stable")`
#' Set dataset primary keys
#'
#' @param x an object of `TealDataset` or `TealDatasetConnector` class
#' @param keys optional, (`character`) vector with primary keys
#' @param dataname (`character`) name of dataset for which set the keys
#' @param ... not used, only for support of S3
#'
#' @return (`character`) vector of column names
#'
#' @export
set_keys <- function(x, ...) {
  UseMethod("set_keys")
}

#' @rdname set_keys
#' @export
#' @examples
#' # TealDataset --------
#'
#' library(scda)
#' set_keys(
#'   dataset(
#'     "ADSL",
#'     synthetic_cdisc_data("latest")$adsl,
#'     code = "ADSL <- synthetic_cdisc_data(\"latest\")$adsl"
#'   ),
#'   keys = get_cdisc_keys("ADSL")
#' )
set_keys.TealDataset <- function(x, keys, ...) {
  check_ellipsis(...)
  x$set_keys(keys)
}

#' @rdname set_keys
#' @export
#' @examples
#' # TealDatasetConnector --------
#'
#' library(scda)
#' pull_fun_adsl <- callable_function(
#'   function() {
#'     synthetic_cdisc_data("latest")$adsl
#'   }
#' )
#' set_keys(
#'   dataset_connector(
#'     "ADSL",
#'     pull_fun_adsl
#'   ),
#'   keys = get_cdisc_keys("ADSL")
#' )
set_keys.TealDatasetConnector <- function(x, keys, ...) {
  check_ellipsis(...)
  x$set_keys(keys)
}

#' @rdname set_keys
#' @export
#' @examples
#' # TealData --------
#'
#' set_keys(
#'   teal_data(
#'     dataset("x", data.frame(x1 = 1:10, y1 = 11:20), keys = "x1"),
#'     dataset("y", data.frame(x2 = 1:10, y2 = 11:20), keys = "x2")
#'   ),
#'   "x",
#'   c("x1", "y1")
#' )
set_keys.TealDataAbstract <- function(x, dataname, keys, ...) {
  check_ellipsis(...)
  set_keys(x$get_items(dataname), keys = keys)
  return(invisible(x))
}
