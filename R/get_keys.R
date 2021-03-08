#' Get dataset primary keys
#'
#' @description `r lifecycle::badge("experimental")`
#' Get dataset primary keys
#'
#' @param x an object of `Dataset` or `DatasetConnector` class
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
#' # Dataset --------
#'
#' library(random.cdisc.data)
#' get_keys(
#'   dataset(
#'     "ADSL",
#'     radsl(cached = TRUE),
#'     keys = get_cdisc_keys("ADSL"),
#'     code = "ADSL <- radsl(cached = TRUE)"
#'   )
#' )
get_keys.Dataset <- function(x, ...) {
  check_ellipsis(...)
  x$get_keys()
}

#' @rdname get_keys
#' @export
#' @examples
#' # DatasetConnector --------
#'
#' library(random.cdisc.data)
#' get_keys(
#'   rcd_dataset_connector(
#'     "ADSL",
#'     radsl,
#'     keys = get_cdisc_keys("ADSL"),
#'   )
#' )
get_keys.DatasetConnector <- function(x, ...) {
  check_ellipsis(...)
  x$get_keys()
}

#' @rdname get_keys
#' @export
#' @examples
#' # RelationalData --------
#'
#' get_keys(
#'   cdisc_data(
#'     dataset("x", data.frame(x1 = 1:10, y1 = 11:20), keys = "x1"),
#'     dataset("y", data.frame(x2 = 1:10, y2 = 11:20), keys = "x2")
#'   ),
#'   "x"
#' )
get_keys.DataAbstract <- function(x, dataname, ...) {
  check_ellipsis(...)
  get_keys(x$get_items(dataname))
}



#' Set dataset primary keys
#'
#' @description `r lifecycle::badge("experimental")`
#' Set dataset primary keys
#'
#' @param x an object of `Dataset` or `DatasetConnector` class
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
#' # Dataset --------
#'
#' library(random.cdisc.data)
#' set_keys(
#'   dataset(
#'     "ADSL",
#'     radsl(cached = TRUE),
#'     code = "ADSL <- radsl(cached = TRUE)"
#'   ),
#'   keys = get_cdisc_keys("ADSL")
#' )
set_keys.Dataset <- function(x, keys, ...) {
  check_ellipsis(...)
  x$set_keys(keys)
}

#' @rdname set_keys
#' @export
#' @examples
#' # DatasetConnector --------
#'
#' library(random.cdisc.data)
#' set_keys(
#'   rcd_dataset_connector(
#'     "ADSL",
#'     radsl
#'   ),
#'   keys = get_cdisc_keys("ADSL")
#' )
set_keys.DatasetConnector <- function(x, keys, ...) {
  check_ellipsis(...)
  x$set_keys(keys)
}

#' @rdname set_keys
#' @export
#' @examples
#' # RelationalData --------
#'
#' set_keys(
#'   cdisc_data(
#'     dataset("x", data.frame(x1 = 1:10, y1 = 11:20), keys = "x1"),
#'     dataset("y", data.frame(x2 = 1:10, y2 = 11:20), keys = "x2")
#'   ),
#'   "x",
#'   c("x1", "y1")
#' )
set_keys.DataAbstract <- function(x, dataname, keys, ...) {
  check_ellipsis(...)
  set_keys(x$get_items(dataname), keys = keys)
  return(invisible(x))
}
