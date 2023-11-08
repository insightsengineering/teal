#' Data module
#'
#' Data input for `teal::init` in form of a module
#'
#' @param ui (`function(id)`)\cr
#'  `shiny` `ui` module with `id` argument
#' @param server (`function(id)`)\cr
#'  `shiny` server function with `id` as argument. Module should return reactive `teal_data`.
#' @examples
#' data <- data_module(
#'   ui = function(id) {
#'     ns <- NS(id)
#'     actionButton(ns("submit"), label = "Load data")
#'   },
#'   server = function(id) {
#'     moduleServer(id, function(input, output, session) {
#'       eventReactive(input$submit, {
#'         data <- within(
#'           teal.data::teal_data(),
#'           {
#'             dataset1 <- iris
#'             dataset2 <- mtcars
#'           }
#'         )
#'         teal.data::datanames(data) <- c("iris", "mtcars")
#'
#'         data
#'       })
#'     })
#'   }
#' )
#' @export
data_module <- function(ui, server) {
  checkmate::assert_function(ui, args = "id", nargs = 1)
  checkmate::assert_function(server, args = "id", nargs = 1)
  structure(
    list(ui = ui, server = server),
    class = "data_module"
  )
}
