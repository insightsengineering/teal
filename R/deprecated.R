

#' @export
tab_item <- function(...) {
  warning("tab_item is deprecated, please use module()")
  module(...)
}

#' @export
tabs_item <- function(...) {
  warning("tabs_item is deprecated, please use modules()")
  modules(...)
}

#' @export
tabs <- function(...) {
  warning("tabs is deprecated, please use modules()")
  modules(...)
}
