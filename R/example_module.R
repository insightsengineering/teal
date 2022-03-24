#' An example `teal` module
#'
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
    server = function(id, datasets) {
      moduleServer(id, function(input, output, session) {
        output$text <- renderPrint(datasets$get_data(input$dataname, filtered = TRUE))
      })
    },
    ui = function(id, datasets) {
      ns <- NS(id)
      teal.widgets::standard_layout(
        output = verbatimTextOutput(ns("text")),
        encoding = selectInput(ns("dataname"), "Choose a dataset", choices = datasets$datanames())
      )
    },
    filters = "all"
  )
}
