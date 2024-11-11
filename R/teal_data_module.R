#' Data module for `teal` applications
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Create a `teal_data_module` object and evaluate code on it with history tracking.
#'
#' @details
#' `teal_data_module` creates a `shiny` module to interactively supply or modify data in a `teal` application.
#' The module allows for running any code (creation _and_ some modification) after the app starts or reloads.
#' The body of the server function will be run in the app rather than in the global environment.
#' This means it will be run every time the app starts, so use sparingly.
#'
#' Pass this module instead of a `teal_data` object in a call to [init()].
#' Note that the server function must always return a `teal_data` object wrapped in a reactive expression.
#'
#' See vignette `vignette("data-as-shiny-module", package = "teal")` for more details.
#'
#' @param ui (`function(id)`)
#'  `shiny` module UI function; must only take `id` argument
#' @param server (`function(id)`)
#'  `shiny` module server function; must only take `id` argument;
#'  must return reactive expression containing `teal_data` object
#' @param label (`character(1)`) Label of the module.
#' @param once (`logical(1)`)
#'  If `TRUE`, the data module will be shown only once and will disappear after successful data loading.
#'  App user will no longer be able to interact with this module anymore.
#'  If `FALSE`, the data module can be reused multiple times.
#'  App user will be able to interact and change the data output from the module multiple times.
#'
#' @return
#' `teal_data_module` returns a list of class `teal_data_module` containing two elements, `ui` and
#' `server` provided via arguments.
#'
#' @examples
#' tdm <- teal_data_module(
#'   ui = function(id) {
#'     ns <- NS(id)
#'     actionButton(ns("submit"), label = "Load data")
#'   },
#'   server = function(id) {
#'     moduleServer(id, function(input, output, session) {
#'       eventReactive(input$submit, {
#'         data <- within(
#'           teal_data(),
#'           {
#'             dataset1 <- iris
#'             dataset2 <- mtcars
#'           }
#'         )
#'
#'         data
#'       })
#'     })
#'   }
#' )
#'
#' @name teal_data_module
#' @seealso [`teal.data::teal_data-class`], [teal.code::qenv()]
#'
#' @export
teal_data_module <- function(ui, server, label = "data module", once = TRUE) {
  checkmate::assert_function(ui, args = "id", nargs = 1)
  checkmate::assert_function(server, args = "id", nargs = 1)
  checkmate::assert_string(label)
  checkmate::assert_flag(once)
  structure(
    list(
      ui = ui,
      server = function(id) {
        data_out <- server(id)
        decorate_err_msg(
          assert_reactive(data_out),
          pre = sprintf("From: 'teal_data_module()':\nA 'teal_data_module' with \"%s\" label:", label),
          post = "Please make sure that this module returns a 'reactive` object containing 'teal_data' class of object." # nolint: line_length_linter.
        )
      }
    ),
    label = label,
    class = "teal_data_module",
    once = once
  )
}
