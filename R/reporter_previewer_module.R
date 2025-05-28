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

#' Reporter previewer tab
#'
#' Creates navigation for reporter previewer in main tab of the teal UI
#' @noRd
insert_reporter_previewer_tab <- function(session, modules, modules_output, reporter) {
  if (is.null(reporter)) {
    return(FALSE)
  }
  reporter$set_id(attr(filter, "app_id"))
  reporter_module <- extract_module(modules, "teal_module_previewer")[[1]]
  modules <- drop_module(modules, "teal_module_previewer")

  previewer_out <- do.call(
    reporter_module$server,
    args = c(list(id = "report_previewer", reporter = reporter), reporter_module$server_args)
  )
  previewer_ui <- do.call(
    reporter_module$ui,
    args = c(list(id = session$ns("report_previewer")), reporter_module$ui_args)
  )

  # Report Previewer tab needs to be shown only if any module has a reporter functionality
  returns_teal_report <- function(x) {
    if (is.reactive(x)) {
      returns_teal_report(tryCatch(x(), error = function(e) e))
    } else if (inherits(x, "teal_report")) {
      TRUE
    } else if (is.list(x)) {
      any(unlist(sapply(x, returns_teal_report)))
    } else {
      FALSE
    }
  }
  any_use_reporter <- reactiveVal(FALSE)
  observeEvent(returns_teal_report(modules_output), {
    if ((is_arg_used(modules, "reporter") || returns_teal_report(modules_output)) && !isTRUE(any_use_reporter())) {
      any_use_reporter(TRUE)
    }
  })

  observeEvent(any_use_reporter(), {
    if (any_use_reporter()) {
      bslib::nav_insert(
        id = "teal_modules-active_tab",
        nav = bslib::nav_panel(title = reporter_module$label, previewer_ui),
        session = session
      )
    }
  })

  previewer_out # returned for testing
}
