

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

#' Deprecated: Variable Browser Teal Module
#'
#' Please use the \code{\link{tm_variable_browser}} function instead.
#'
#' @param ... arguments passed on to \code{\link{tm_variable_browser}}
#'
#' @export
variable_browser_item <- function(...) {
  warning("variable_browser_item is deprecated, pleas use tm_variable_browser instead")
  tm_variable_browser(...)
}


#' Deprecated: Data Table Viewer Teal Module
#'
#' Please use the \code{\link{tm_data_table}} function instead.
#'
#' @param ... arguments passed on to \code{\link{tm_data_table}}
#'
#' @export
data_table_item <- function(...) {
  warning("data_table_item is deprecated, pleas use tm_data_table instead")
  tm_data_table(...)
}
