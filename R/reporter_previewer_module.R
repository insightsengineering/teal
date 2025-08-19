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

# # TODO: averissimo (check if necessary)
# #' Reporter previewer tab
# #'
# #' Creates navigation for reporter previewer in main tab of the teal UI
# #' @noRd
# insert_reporter_previewer_tab <- function(session, modules, modules_output, reporter, app_id) {
#   if (is.null(reporter)) {
#     return(FALSE)
#   }
#   reporter$set_id(app_id)
#   reporter_module <- extract_module(modules, "teal_module_previewer")[[1]]
#   modules <- drop_module(modules, "teal_module_previewer")

#   previewer_out <- do.call(
#     reporter_module$server,
#     args = c(list(id = "report_previewer", reporter = reporter), reporter_module$server_args)
#   )
#   previewer_ui <- do.call(
#     reporter_module$ui,
#     args = c(list(id = session$ns("report_previewer")), reporter_module$ui_args)
#   )

#   # Report Previewer tab needs to be shown only if any module has a reporter functionality
#   returns_teal_report <- function(x) {
#     if (is.reactive(x)) {
#       returns_teal_report(tryCatch(x(), error = function(e) e))
#     } else if (inherits(x, "teal_report")) {
#       TRUE
#     } else if (is.list(x)) {
#       any(unlist(sapply(x, returns_teal_report)))
#     } else {
#       FALSE
#     }
#   }
#   any_use_reporter <- reactiveVal(FALSE)
#   observeEvent(returns_teal_report(modules_output), {
#     if ((is_arg_used(modules, "reporter") || returns_teal_report(modules_output)) && !isTRUE(any_use_reporter())) {
#       any_use_reporter(TRUE)
#     }
#   })

#   observeEvent(any_use_reporter(), {
#     if (any_use_reporter()) {
#       bslib::nav_insert(
#         id = "teal_modules-active_tab",
#         nav = bslib::nav_panel(title = reporter_module$label, previewer_ui),
#         session = session
#       )
#     }
#   })

#   previewer_out # returned for testing
# }

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
