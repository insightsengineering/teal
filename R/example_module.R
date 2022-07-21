#' An example `teal` module
#'
#' @description `r lifecycle::badge("experimental")`
#' @param label `character`, the label of the module
#' @return A `teal` module which can be included in the `modules` argument to [teal::init()].
#' @examples
#' app <- init(
#'   data = teal_data(
#'     dataset("IRIS", iris),
#'     dataset("MTCARS", mtcars)
#'   ),
#'   modules = modules(example_module())
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#' @export
example_module <- function(label = "example teal module") {
  checkmate::assert_string(label)
  module(
    label,
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        output$text <- renderPrint(data[[input$dataname]]())
      })
    },
    ui = function(id, data) {
      ns <- NS(id)
      teal.widgets::standard_layout(
        output = verbatimTextOutput(ns("text")),
        encoding = selectInput(ns("dataname"), "Choose a dataset", choices = names(data))
      )
    },
    filters = "all"
  )
}
