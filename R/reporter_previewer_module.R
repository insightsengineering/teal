#' Create a `teal` module for previewing a report
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This function wraps [teal.reporter::reporter_previewer_ui()] and
#' [teal.reporter::reporter_previewer_srv()] into a `teal_module` to be
#' used in `teal` applications.
#'
#' If you are creating a `teal` application using [init()] then this
#' module will be added to your application automatically if any of your `teal_modules`
#' support report generation.
#'
#' @inheritParams teal_modules
#' @param server_args (named `list`)
#'  Arguments passed to [teal.reporter::reporter_previewer_srv()].
#'
#' @return
#' `teal_module` (extended with `teal_module_previewer` class) containing the `teal.reporter` previewer functionality.
#'
#' @export
#'
reporter_previewer_module <- function(label = "Report previewer", server_args = list()) {
  checkmate::assert_string(label)
  checkmate::assert_list(server_args, names = "named")
  checkmate::assert_true(all(names(server_args) %in% names(formals(teal.reporter::reporter_previewer_srv))))

  message("Initializing reporter_previewer_module")

  srv <- function(id, reporter, ...) {
    teal.reporter::reporter_previewer_srv(id, reporter, ...)
  }

  ui <- function(id, ...) {
    teal.reporter::reporter_previewer_ui(id, ...)
  }

  module <- module(
    label = "temporary label",
    server = srv, ui = ui,
    server_args = server_args, ui_args = list(), datanames = NULL
  )
  # Module is created with a placeholder label and the label is changed later.
  # This is to prevent another module being labeled "Report previewer".
  class(module) <- c(class(module), "teal_module_previewer")
  module$label <- label
  attr(module, "teal_bookmarkable") <- TRUE
  module
}
