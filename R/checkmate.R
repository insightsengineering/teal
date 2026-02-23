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
  TRUE
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


#' Internal function to check if decorators is a valid object
#' @noRd
check_transformators <- function(x, names = NULL, arg = "decorators") { # nolint: object_name.

  check_message <- checkmate::check_list(x, names = "named")

  if (!is.null(names) && isTRUE(check_message)) {
    if (length(names(x)) != length(unique(names(x)))) {
      check_message <- sprintf(
        "The `%s` must contain unique names from these names: %s", arg,
        paste(sQuote(names), collapse = ", ")
      )
    } else if (!all(unique(names(x)) %in% c("default", names))) {
      check_message <- sprintf(
        paste0(
          "The `%s` must be a named list with:\n",
          " * 'default' for decorating all objects and/or\n",
          " * A name from these: %s"
        ),
        arg, paste(sQuote(names), collapse = ", ")
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

  paste0(
    "The named list can contain a list of 'teal_transform_module' objects created ",
    "using `teal_transform_module()` or be a `teal_transform_module` object."
  )
}
#' Internal assertion on decorators
#' @noRd
assert_transformators <- checkmate::makeAssertionFunction(check_transformators)
