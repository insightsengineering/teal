# Custom extensions to the function argument checks.

#' Check that argument is reactive.
#'
#' @inherit checkmate::check_class params return
#'
#' @seealso check_reactiveExpr check_reactiveVal
#'
#' @keywords internal
#'
#' @examples
#' test_reactive(1)
#' test_reactive(reactive(NULL))
#'
#' check_reactive(1)
#' check_reactive(reactive(NULL))
#'
#' \dontrun{
#'   assert_reactive(1)
#'   assert_reactive(reactive(NULL))
#' }
check_reactive <- function(x, null.ok = FALSE) {
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
test_reactive <- function(x, null.ok = FALSE) {
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
#'
#' @examples
#' test_reactiveExpr(1)
#' test_reactiveExpr(reactive(NULL))
#'
#' check_reactiveExpr(1)
#' check_reactiveExpr(reactive(NULL))
#'
#' \dontrun{
#'   assert_reactiveExpr(1)
#'   assert_reactiveExpr(reactive(NULL))
#' }
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
#'
#' @examples
#' test_reactiveVal(1)
#' test_reactiveVal(reactiveVal(NULL))
#'
#' check_reactiveVal(1)
#' check_reactiveVal(reactiveVal(NULL))
#'
#' \dontrun{
#'   assert_reactiveVal(1)
#'   assert_reactiveVal(reactiveVal(NULL))
#' }
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
