#' Create a `teal` module for previewing a report
#'
#' This function wraps [teal.reporter::reporter_previewer_ui()] and
#' [teal.reporter::reporter_previewer_srv()] into a `teal_module` to be
#' used in `teal` applications.
#'
#' If you are creating a `teal` application using [teal::init()] then this
#' module will be added to your application automatically if any of your `teal modules`
#' support report generation
#'
#' @inheritParams module
#' @return `teal_module` containing the `teal.reporter` previewer functionality
#' @export
reporter_previewer_module <- function(label = "Report previewer") {
  checkmate::assert_string(label)
  srv <- function(id, datasets, reporter, ...) {
    teal.reporter::reporter_previewer_srv(id, reporter, ...)
  }

  ui <- function(id, datasets, ...) {
    teal.reporter::reporter_previewer_ui(id, ...)
  }

  module(
    label = label,
    server = srv, ui = ui,
    server_args = list(), ui_args = list(), filters = NULL
  )
}
