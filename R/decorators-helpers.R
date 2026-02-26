#' Check decorators list
#'
#' Check if a decorators list is valid and matches the expected output names.
#'
#' @param x (named `list`) of [teal_transform_module()] objects, or nested lists thereof.
#' @param names (`character`) optional vector of valid output names. When provided, all names
#'   in `x` must be one of these names, and names must be unique.
#'
#' @return `TRUE` if valid, otherwise a `character(1)` string describing the problem.
#' @seealso [module_transform_data()]
#' @export
#' @examples
#' decorator <- teal_transform_module(server = function(id, data) data)
#' check_decorators(decorator)
#' check_decorators(list(all = decorator))
#' check_decorators(list(all = decorator, output = decorator))
#' check_decorators(list(all = decorator, output = list(decorator, decorator)))
check_decorators <- function(x, names = NULL) { # nolint: object_name.

  check_message <- checkmate::check_list(x, names = "named")

  if (!is.null(names) && isTRUE(check_message)) {
    if (length(names(x)) != length(unique(names(x)))) {
      check_message <- sprintf(
        "The `decorators` must contain unique names from these names: %s",
        paste(sQuote(names), collapse = ", ")
      )
    } else if (!all(unique(names(x)) %in% c("all", names))) {
      check_message <- sprintf(
        paste0(
          "The `decorators` must be a named list with:\n",
          " * 'all' for decorating all objects and/or\n",
          " * A name from these: %s"
        ),
        paste(sQuote(names), collapse = ", ")
      )
    }
  }

  if (!isTRUE(check_message)) {
    return(check_message)
  }

  valid_elements <- vapply(
    x,
    checkmate::test_class,
    classes = "teal_transform_module",
    FUN.VALUE = logical(1L)
  )

  # Nested list
  if (any(!valid_elements)) {
    valid_nested <- vapply(
      x[!valid_elements], function(subdecorators) {
        checks <- vapply(subdecorators,
          checkmate::test_class,
          classes = "teal_transform_module",
          logical(1L)
        )
        all(checks)
      },
      FUN.VALUE = logical(1L)
    )
    valid_elements[!valid_elements] <- valid_nested
  }

  if (all(valid_elements)) {
    return(TRUE)
  }

  paste(
    "The named list can contain a list of 'teal_transform_module' objects created",
    "using `teal_transform_module()` or be a `teal_transform_module` object."
  )
}

#' @rdname check_decorators
#' @inheritParams checkmate::makeAssertionFunction
#' @param add If an `AssertCollection` is provided, the error message is stored in it.
#' If `NULL`, an exception is raised if res is not `TRUE`.
#' @export
assert_decorators <- checkmate::makeAssertionFunction(check_decorators)

#' Subset decorators based on the scope
#'
#' @param scope (`character(1)`) a decorator name to include.
#' @param decorators (named `list`) a named list of decorators to subset.
#'
#' @return A `list` of `teal_transform_module` objects matching the given `scope` and `all`.
#' Returns an empty list if `scope` and `all` is not found in `decorators`.
#' @keywords internal
select_decorators <- function(decorators, scope) {
  checkmate::assert_string(scope, null.ok = FALSE)
  check_decorators(decorators, scope)
  decorators <- decorators[names(decorators) %in% c("all", scope)]
  if (!length(decorators)) {
    return(list())
  }
  decorators
}
