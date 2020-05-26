#' Set arguments of a \code{CallableFunction}
#'
#' Set arguments of a \code{CallableFunction}
#' @param x \code{CallableFunction} or \code{RawDatasetConnector})
#' @param args (\code{NULL} or named \code{list}) dynamic arguments to function
#'
#' @return nothing
#' @rdname set_args
#' @export
set_args <- function(x, args) {
  UseMethod("set_args")
}

#' @rdname set_args
#' @export
#' @examples
#' library(random.cdisc.data)
#' fun <- callable_function(radsl)
#' set_args(fun, list(N = 5, seed = 1, cached = TRUE))
set_args.CallableFunction <- function(x, args) {
  x$set_args(args)
}

#' @rdname set_args
#' @export
#' @examples
#' ds <- raw_dataset_connector(pull_fun = callable_function(data.frame))
#' set_args(ds, list(x = 1:5, y = letters[1:5]))
set_args.RawDatasetConnector <- function(x, args) {
  x$set_args(args)
}
