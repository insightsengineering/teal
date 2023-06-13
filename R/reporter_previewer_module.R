#' Create a `teal` module for previewing a report
#'
#' @description `r lifecycle::badge("experimental")`
#' This function wraps [teal.reporter::reporter_previewer_ui()] and
#' [teal.reporter::reporter_previewer_srv()] into a `teal_module` to be
#' used in `teal` applications.
#'
#' If you are creating a `teal` application using [teal::init()] then this
#' module will be added to your application automatically if any of your `teal modules`
#' support report generation
#'
#' @inheritParams module
#' @param srv_args (`named list`)\cr
#'  Arguments passed to [teal.reporter::reporter_previewer_srv()].
#' @param ui_args (`named list`)\cr
#'  Arguments passed to [teal.reporter::reporter_previewer_ui()].
#' @return `teal_module` containing the `teal.reporter` previewer functionality
#' @export
reporter_previewer_module <- function(label = "Report previewer", srv_args = list(), ui_args = list()) {
  checkmate::assert_string(label)
  checkmate::assert_list(srv_args, names = "named")
  checkmate::assert_list(ui_args, names = "named")
  checkmate::assert_true(all(names(srv_args) %in% names(formals(teal.reporter::reporter_previewer_srv))))
  checkmate::assert_true(all(names(ui_args) %in% names(formals(teal.reporter::reporter_previewer_ui))))

  srv <- function(id, reporter, ...) {
    teal.reporter::reporter_previewer_srv(id, reporter, ...)
  }

  ui <- function(id, ...) {
    teal.reporter::reporter_previewer_ui(id, ...)
  }

  module <- module(
    label = label,
    server = srv, ui = ui,
    server_args = srv_args, ui_args = ui_args, filters = NULL
  )
  class(module) <- c("teal_module_previewer", class(module))
  module
}
