#' Create a `teal` module for previewing a report
#'
#' @description `r lifecycle::badge("deprecated")`
#'
#' This function controls the appearance of the drop-down menu for the reporter.
#' It is now deprecated in favor of the options:
#' - `teal.reporter.nav_buttons = c("preview", "download", "load", "reset")` to control which
#'  buttons will be displayed in the drop-down.
#' - `teal.reporter.rmd_output`: passed to [teal.reporter::download_report_button_srv()]
#' - `teal.reporter.rmd_yaml_args`: passed to [teal.reporter::download_report_button_srv()]
#' - `teal.reporter.global_knitr`: passed to [teal.reporter::download_report_button_srv()]
#'
#' @inheritParams teal_modules
#' @param server_args (named `list`) Arguments will overwrite the default `teal.reporter` options
#' described in the description.
#'
#' @return
#' `teal_module` (extended with `teal_module_previewer` class) containing the `teal.reporter` previewer functionality.
#'
#' @export
reporter_previewer_module <- function(label = "Report previewer", server_args = list()) {
  checkmate::assert_string(label)
  checkmate::assert_list(server_args, names = "named")
  checkmate::assert_true(all(names(server_args) %in% names(formals(teal.reporter::reporter_previewer_srv))))

  lifecycle::deprecate_soft(
    when = "1.0.0",
    what = "reporter_previewer_module()",
    details = paste(
      "Please use `options()` to customize the reporter options:\n",
      "`teal.reporter.nav_buttons` to control which buttons will be displayed in the 'Report' drop-down.\n",
      "`teal.reporter.rmd_output` to customize the R Markdown outputs types for the report.\n",
      "`teal.reporter.rmd_yaml_args` to customize the widget inputs in the download report modal.\n",
      "`teal.reporter.global_knitr` to customize the global knitr options for the report."
    )
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
  # Module is created with a placeholder label and path and both are changed later.
  # This is to prevent another module being labeled "Report previewer".
  class(module) <- c(class(module), "teal_module_previewer")
  module$label <- label
  module$path <- label
  attr(module, "teal_bookmarkable") <- TRUE
  module
}

#' Temporary function to handle server_args of the report_previewer_module before its hard
#' deprecation.
#' @param args (`list`)
#'
#' @keywords internal
.get_reporter_options <- function(args) {
  opts <- list()
  if (length(args$previewer_buttons)) {
    opts <- c(opts, list(teal.reporter.nav_buttons = args$previewer_buttons))
  }

  if (length(args$global_knitr)) {
    opts <- c(opts, list(teal.reporter.global_knitr = args$global_knitr))
  }

  if (length(args$rmd_output)) {
    opts <- c(opts, list(teal.reporter.rmd_output = args$rmd_output))
  }

  if (length(args$rmd_yaml_args)) {
    opts <- c(opts, list(teal.reporter.rmd_yaml_args = args$rmd_yaml_args))
  }

  opts
}
