# Custom extensions to the function argument checks.

#' Check that argument is reactive.
#'
#' @inherit checkmate::check_class params return
#'
#' @seealso check_reactiveExpr check_reactiveVal
#'
#' @keywords internal
check_reactive <- function(x, null.ok = FALSE) { # nolint: object_name_linter.
  if (!isTRUE(checkmate::test_class(x, classes = "reactive", null.ok = null.ok))) {
    cl <- class(x)
    return(sprintf(
      "Must be a reactive (i.e. inherit from 'reactive' class) but has class%s '%s'",
      if (length(cl) > 1L) "es" else "",
      paste0(cl, collapse = "','")
    ))
  }
  return(TRUE)
}
#' @rdname check_reactive
test_reactive <- function(x, null.ok = FALSE) { # nolint: object_name_linter.
  isTRUE(check_reactive(x, null.ok = null.ok))
}
#' @rdname check_reactive
assert_reactive <- checkmate::makeAssertionFunction(check_reactive)

#' Check that argument is a reactive expression.
#'
#' @inherit checkmate::check_class params return
#'
#' @seealso check_reactive check_reactiveVal
#'
#' @keywords internal
check_reactiveExpr <- function(x, null.ok = FALSE) { # nolint: object_name_linter.
  if (!isTRUE(checkmate::test_class(x, classes = "reactiveExpr", null.ok = null.ok))) {
    cl <- class(x)
    return(sprintf(
      "Must be a reactive expression (i.e. inherit from 'reactiveExpr' class) but has class%s '%s'",
      if (length(cl) > 1L) "es" else "",
      paste0(cl, collapse = "','")
    ))
  }
  return(TRUE)
}
#' @rdname check_reactiveExpr
test_reactiveExpr <- function(x, null.ok = FALSE) { # nolint: object_name_linter.
  isTRUE(check_reactiveExpr(x, null.ok = null.ok))
}
#' @rdname check_reactiveExpr
assert_reactiveExpr <- checkmate::makeAssertionFunction(check_reactiveExpr) # nolint: object_name_linter.

#' Check that argument is a reactive value.
#'
#' @inherit checkmate::check_class params return
#'
#' @seealso check_reactive check_reactiveExpr
#'
#' @keywords internal
check_reactiveVal <- function(x, null.ok = FALSE) { # nolint: object_name_linter.
  if (!isTRUE(checkmate::test_class(x, classes = "reactiveVal", null.ok = null.ok))) {
    cl <- class(x)
    return(sprintf(
      "Must be a reactive value (i.e. inherit from 'reactiveVal' class) but has class%s '%s'",
      if (length(cl) > 1L) "es" else "",
      paste0(cl, collapse = "','")
    ))
  }
  return(TRUE)
}
#' @rdname check_reactiveVal
test_reactiveVal <- function(x, null.ok = FALSE) { # nolint: object_name_linter.
  isTRUE(check_reactiveVal(x, null.ok = null.ok))
}
#' @rdname check_reactiveVal
assert_reactiveVal <- checkmate::makeAssertionFunction(check_reactiveVal) # nolint: object_name_linter.


#' Capture error and decorate error message.
#'
#' @param x object to evaluate
#' @param prefix (`character(1)`) prefix to error message
#' @param suffix (`character(1)`) suffix to error message
#'
#' @return `x` if no error, otherwise throws error with decorated message
#'
#' @keywords internal
decorate_err_msg <- function(x, prefix = character(0), suffix = character(0)) {
  tryCatch(
    x,
    error = function(e) {
      stop(
        paste0(
          "\n",
          prefix,
          "\n",
          e$message,
          "\n",
          suffix
        ),
        call. = FALSE
      )
    }
  )
  x
}
