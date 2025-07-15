simple_teal_data <- function() {
  data <- within(teal_data(), {
    iris <- iris
    mtcars <- mtcars
  })
  data
}

report_module <- function(label = "example teal module") {
  module(
    label = label,
    server = function(id, data, reporter) {
      moduleServer(id, function(input, output, session) {
        teal.reporter::simple_reporter_srv(
          id = "reporter",
          reporter = reporter,
          card_fun = function(card) card
        )
        updateSelectInput(session, "dataname", choices = isolate(names(data())))
        output$dataset <- renderPrint({
          req(input$dataname)
          data()[[input$dataname]]
        })
      })
    },
    ui = function(id) {
      ns <- NS(id)
      sidebarLayout(
        sidebarPanel(
          teal.reporter::simple_reporter_ui(ns("reporter")),
          selectInput(ns("dataname"), "Choose a dataset", choices = NULL)
        ),
        mainPanel(verbatimTextOutput(ns("dataset")))
      )
    }
  )
}

example_teal_data_module <- function(with_submit = FALSE, once = TRUE) {
  teal_data_module(
    ui = function(id) {
      tagList(
        numericInput(NS(id, "iris_rows"), "iris rows", min = 0, max = 150, step = 1, value = 10),
        if (with_submit) actionButton(NS(id, "submit"), "Submit")
      )
    },
    server = function(id, ...) {
      moduleServer(id, function(input, output, session) {
        if (with_submit) {
          eventReactive(input$submit, {
            teal_data(iris = head(iris, input$iris_rows), mtcars = mtcars)
          })
        } else {
          reactive(teal_data(iris = head(iris, input$iris_rows), mtcars = mtcars))
        }
      })
    },
    once = once
  )
}