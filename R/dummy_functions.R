#' An example `teal` module
#'
#' `r lifecycle::badge("experimental")`
#'
#' @inheritParams teal_modules
#' @return A `teal` module which can be included in the `modules` argument to [init()].
#' @examples
#' app <- init(
#'   data = teal_data(IRIS = iris, MTCARS = mtcars),
#'   modules = example_module()
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#' @export
example_module <- function(label = "example teal module", datanames = "all") {
  checkmate::assert_string(label)
  module(
    label,
    server = function(id, data) {
      checkmate::assert_class(data(), "teal_data")
      moduleServer(id, function(input, output, session) {
        updateSelectInput(session, "dataname", choices = isolate(teal.data::datanames(data())))
        output$text <- renderPrint({
          req(input$dataname)
          data()[[input$dataname]]
        })
        teal.widgets::verbatim_popup_srv(
          id = "rcode",
          verbatim_content = reactive(teal.code::get_code(data())),
          title = "Example Code"
        )
      })
    },
    ui = function(id) {
      ns <- NS(id)
      teal.widgets::standard_layout(
        output = verbatimTextOutput(ns("text")),
        encoding = div(
          selectInput(ns("dataname"), "Choose a dataset", choices = NULL),
          teal.widgets::verbatim_popup_ui(ns("rcode"), "Show R code")
        )
      )
    },
    datanames = datanames
  )
}
