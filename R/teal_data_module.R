#' Data module for `teal` applications
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Create a `teal_data_module` object and evaluate code on it with history tracking.
#'
#' @details
#' `teal_data_module` creates a `shiny` module to supply or modify data in a `teal` application.
#' The module allows for running data pre-processing code (creation _and_ some modification) after the app starts.
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
#' @param once (`logical(1)`) Applicable only when used in the `data` argument of `init`.
#'  If `TRUE`, the data module can only be consumed once. After the successful consumption,
#'  the data module tab will disappear and the user will not be able to interact with the UI of the module.
#'  The common usecase for using `once` as `TRUE` is when `teal_data_module` is used for authentication.
#'
#' @return
#' `teal_data_module` returns an object of class `teal_data_module`.
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
#'         datanames(data) <- c("dataset1", "dataset2")
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
  checkmate::assert(
    checkmate::check_function(server, args = "id", nargs = 1),
    # todo: allow for teal_data_module$server to have 'data' argument or break this in teal_transformer_module
    checkmate::check_function(server, args = c("id", "data"), nargs = 2)
  )
  structure(
    list(ui = ui, server = server),
    label = label,
    class = "teal_data_module",
    once = once
  )
}
