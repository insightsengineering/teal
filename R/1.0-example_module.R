example_module_2 <- function(label = "example teal module", datanames = "all") {
  checkmate::assert_string(label)
  ans <- module(
    label,
    server = function(id, data) {
      checkmate::assert_class(data(), "teal_data")
      moduleServer(id, function(input, output, session) {
        updateSelectInput(
          inputId = "dataname",
          choices = isolate(teal.data::datanames(data())),
          selected = restoreInput(session$ns("dataname"), NULL)
        )
        output$text <- renderPrint({
          req(input$dataname)
          data()[[input$dataname]]
        })
      })
    },
    ui = function(id) {
      ns <- NS(id)
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          selectInput(ns("dataname"), "Choose a dataset", choices = NULL)
        ),
        body = verbatimTextOutput(ns("text"))
      )
    },
    datanames = datanames
  )
  attr(ans, "teal_bookmarkable") <- TRUE
  ans
}
