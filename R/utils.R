# are elements unique
is.unique <- function(x) { # nolint
  length(x) == length(unique(x))
}

# also returns a list if only a single element
#' Split by separator
#'
#' @param x (\code{character}) Character (single)
#' @param sep (\code{character}) Separator
#' @export
split_by_sep <- function(x, sep) {
  stopifnot(is.atomic(x))
  strsplit(x, sep, fixed = TRUE)
}

#' List element in other list
#'
#' Checks if \code{x} element matches any of \code{y} element. If one of the arguments is a list then list elements
#' are treated as whole - in this case list elements can be a vector, so it looks
#' for equal element in second vector to be matched.
#'
#' @param x \code{list} to be matched.
#' @param y \code{list} to be matched against.
#' @return \code{logical} vector length of \code{x} denoting if element was found in second list.
#' @export
`%is_in%`  <- function(x, y) {
  if (!is.list(x) & is.list(y)) {
    x <- list(x)
  } else if (!is.list(y) & is.list(x)) {
    y <- list(y)
  }
  vapply(x, function(x1) {
    any(vapply(y, function(y1) isTRUE(all.equal(y1, x1)), logical(1), USE.NAMES = FALSE))
  }, logical(1), USE.NAMES = FALSE)
}

check_module_names <- function(modules) {
  label <- unlist(switch(
      class(modules),
      teal_module = modules$label,
      teal_modules = lapply(modules$modules, function(x) {
        x$label
      }),
      stop("no default implementation for check_module_names")
  ))
  if (any(duplicated(label))) {
    stop("Please choose a unique labels for each teal module.")
  }
}

#' Extract labels from choices basing on attributes and names
#'
#' @param choices (\code{list} or \code{vector}) select choices
#' @param values optional, choices subset for which labels should be extracted, \code{NULL} for all choices
#'
#' @return (\code{character}) vector with labels
extract_choices_labels <- function(choices, values = NULL) {
  res <- if (is(choices, "choices_labeled")) {
    attr(choices, "raw_labels")
  } else if (!is.null(names(choices)) && !setequal(names(choices), unlist(unname(choices)))) {
    names(choices)
  } else {
    NULL
  }

  if (!is.null(values) && !is.null(res)) {
    stopifnot(all(values %in% choices))
    res <- res[vapply(values, function(val) which(val == choices), numeric(1))]
  }

  return(res)
}
