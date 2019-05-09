# are elements unique
is.unique <- function(x) length(x) == length(unique(x))

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
