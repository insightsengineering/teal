#' Check that argument is reactive.
#'
#' @inherit checkmate::check_class params return
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

#' Capture error and decorate error message.
#'
#' @param x object to evaluate
#' @param pre (`character(1)`) A string to prepend to error message
#' @param post (`character(1)`) A string to append to error message
#'
#' @return `x` if no error, otherwise throws error with decorated message
#'
#' @keywords internal
decorate_err_msg <- function(x, pre = character(0), post = character(0)) {
  tryCatch(
    x,
    error = function(e) {
      stop(
        "\n",
        pre,
        "\n",
        e$message,
        "\n",
        post,
        call. = FALSE
      )
    }
  )
  x
}
