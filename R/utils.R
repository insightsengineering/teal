# are elements unique
is.unique <- function(x) length(x) == length(unique(x))

# returns true if fcn evaluates to true on all elements
#' Split by sepearator
#'
#' @param lst (\code{list}) List of values
#' @param fcn (\code{function}) function to apply
#'   that returns TRUE or FALSE
#' @export
all_true <- function(lst, fcn) {
  all(vapply(lst, fcn, TRUE))
}

# also returns a list if only a single element
#' Split by sepearator
#'
#' @param x (\code{character}) Character (single)
#' @param sep (\code{character}) Separator
#' @export
split_by_sep <- function(x, sep) {
  stopifnot(is.atomic(x))
  strsplit(x, sep, fixed = TRUE)
}
