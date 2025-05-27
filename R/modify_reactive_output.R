#' Modify Reactive Outputs of a `teal` Module
#'
#' @description
#' This function takes a `teal_module` object and allows modification of its
#' server's reactive outputs. It wraps the original module's server,
#' applying specified functions to one or more of the outputs returned by the
#' original server.
#'
#' The original server logic is called within a new server function.
#' The transformations are applied to the outputs of the original server
#' before they are returned by the new server.
#'
#' @param teal_module (`teal_module`)
#' @param ... Named arguments where each name corresponds to an output element
#'   (e.g., a reactive expression or a static value) returned by the
#'   `teal_module`'s server function. The value for each named argument must be a
#'   function that takes the original output's value as its single argument and
#'   returns the modified value. If the original output is a reactive, its
#'   resolved value is passed to the modifying function, and the modifying
#'   function's result is wrapped in a new reactive.
#'
#' @return A new `teal_module` object with the modified server logic and a UI
#'   that internally namespaces the original UI. The new module will have the
#'   same label, datanames, server_args, ui_args, and transformators as the
#'   original `teal_module`.
#'
#' @export
modify_reactive_output <- function(teal_module, fun = function(data) data) {
  checkmate::assert_class(teal_module, "teal_module")
  checkmate::assert_function(fun)

  original_server <- teal_module$server
  original_ui <- teal_module$ui
  new_ui <- function(id, ...) {
    ns <- NS(id)
    original_ui(ns("modified"), ...)
  }

  original_server <- teal_module$server
  new_server <- function(...) { }
  formals(new_server) <- formals(original_server)

  body(new_server) <- quote({
    arg_names <- names(formals(original_server))

    # Capture current call and evaluate each argument
    call_expr <- match.call()
    call_args <- as.list(call_expr)[-1] # remove function name
    call_args <- call_args[names(call_args) %in% arg_names]

    # Evaluate all arguments in the current environment
    eval_args <- lapply(call_args, eval, envir = parent.frame())

    moduleServer(id, function(input, output, session) {
      eval_args$id <- "modified"
      out <- do.call(original_server, eval_args)
      reactive(fun(out()))
    })
  })

  module(
    label = teal_module$label,
    server = new_server,
    ui = new_ui,
    datanames = teal_module$datanames,
    server_args = teal_module$server_args,
    ui_args = teal_module$ui_args,
    transformators = teal_module$transformators
  )
}

disable_report <- function(teal_module) {
  modify_reactive_output(teal_module, fun = function(data) {
    report(data) <- report_document()
    data
  })
}
