#' Set filter state in `FilterState` object
#'
#' @description `r lifecycle::badge("experimental")`
#' Sets values of the selection in `FilterState` object
#' @param x (`list`,`vector`, `default_filter`)\cr
#'  values of the variable used in filter. Depending on the `FilterState` type
#'  list should contain:
#'  \itemize{
#'  \item{`selected`}{ defines initial selection}
#'  \item{`keep_na` (`logical`)}{ defines whether to keep or remove `NA` values}
#'  \item{`keep_inf` (`logical`)}{ defines whether to keep or remove `Inf` values}
#'  }
#'  If `vector` is provided then `keep_na` and `keep_inf` can be specified
#'  adding `NA` and `Inf` to the selection vector.
#'
#' @param filter_state (`FilterState`)\cr
#'
#' @examples
#' filter_state <- teal:::RangeFilterState$new(
#'   c(1:10, NA, Inf),
#'   varname = "x"
#' )
#'
#' teal:::set_filter_state(
#'   list(selected = c(1, 2), keep_na = FALSE, keep_inf = TRUE),
#'   filter_state
#' )
#' teal:::set_filter_state(c(1, 2, Inf), filter_state)
#' teal:::set_filter_state(default_filter(), filter_state)
set_filter_state <- function(x, filter_state) {
  UseMethod("set_filter_state")
}

#' @rdname set_filter_state
#' @export
set_filter_state.default <- function(x, filter_state) { #nousage
  state <- list()
  if (any(is.na(x))) {
    state$keep_na <- TRUE
  }

  if (any(is.infinite(x))) {
    state$keep_inf <- TRUE
  }

  if (length(x[!(is.infinite(x) | is.na(x))]) > 0) {
    state$selected <- x[!(is.infinite(x) | is.na(x))]
  }

  filter_state$set_state(state)
}

#' @rdname set_filter_state
#' @export
set_filter_state.default_filter <- function(x, filter_state) { #nolint #nousage
  invisible(NULL)
}

#' @rdname set_filter_state
#' @export
set_filter_state.list <- function(x, filter_state) { #nousage
  filter_state$set_state(state = x)
}
