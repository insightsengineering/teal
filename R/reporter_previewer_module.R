#' Create a `teal` module for previewing a report
#'
#' @description `r lifecycle::badge("deprecated")`
#'
#' This function controls the appearance of the dropdown menu for the reporter.
#' It is now deprecated in favour of the options:
#' - `teal.reporter.nav_buttons = c("preview", "load", "download", "reset")` to control which
#'  buttons will be displayed in the dropdown.
#' - `teal.reporter.rmd_outputs`: passed to [teal.reporter::download_report_button_srv()]
#' - `teal.reporter.rmd_yaml_args`: passed to [teal.reporter::download_report_button_srv()]
#' - `teal.reporter.knitr_global`: passed to [teal.reporter::download_report_button_srv()]
#'
#' @inheritParams teal_modules
#' @param server_args (named `list`) Arguments will overwrite the default `teal.reporter` options
#' described in the description.
#'
#' @return
#' `teal_module` (extended with `teal_module_previewer` class) containing the `teal.reporter` previewer functionality.
#'
#' @export
#'
#'
reporter_previewer_module <- function(label = "Report previewer", server_args = list()) {
  checkmate::assert_string(label)
  checkmate::assert_list(server_args, names = "named")
  checkmate::assert_true(all(names(server_args) %in% names(formals(teal.reporter::reporter_previewer_srv))))

  lifecycle::deprecate_soft(
    when = "",
    what = "reporter_previewer_module()",
    details = "Please use `options()`"
  )

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

#' Temporary function to server before hard deprecate report_previewer_module
.set_reporter_options <- function(args) {
  if (length(args$previewer_buttons)) {
    options(teal.reporter.nav_buttons = args$previewer_buttons)
  }

  if (length(args$global_knitr)) {
    options(teal.reporter.global_knitr = args$global_knitr)
  }

  if (length(args$rmd_output)) {
    options(teal.reporter.rmd_output = args$rmd_output)
  }

  if (length(args$rmd_yaml_args)) {
    options(teal.reporter.rmd_yaml_args = args$rmd_yaml_args)
  }
}
