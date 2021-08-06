#' Set arguments of a \code{CallableFunction}
#'
#' @description `r lifecycle::badge("experimental")`
#' Set arguments of a \code{CallableFunction}
#'
#' @param x \code{CallableFunction} or \code{DatasetConnector})
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
#' library(scda)
#' f <- function(df) {synthetic_cdisc_data('latest')[[df]]}
#' fun <- callable_function(f)
#' set_args(fun, list(df = "adsl"))
set_args.CallableFunction <- function(x, args) {
  x$set_args(args)
  return(invisible(x))
}

#' @rdname set_args
#' @export
#' @examples
#' library(scda)
#' f <- function(df) {synthetic_cdisc_data('latest')[[df]]}
#' code <- callable_code("f()")
#' set_args(code, list(df = "adsl"))
set_args.CallableCode <- function(x, args) {
  warning(
    "'CallableCode' is unchangable. Ignoring arguments set by 'set_args'",
    call. = FALSE
  )
  return(invisible(x))
}

#' @rdname set_args
#' @export
#' @examples
#' ds <- dataset_connector("x", pull_callable = callable_function(data.frame))
#' set_args(ds, list(x = 1:5, y = letters[1:5]))
set_args.DatasetConnector <- function(x, args) {
  x$set_args(args)
  return(invisible(x))
}
