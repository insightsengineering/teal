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
example_module <- function(label = "example teal module", datanames = "all", transformers = list()) {
  checkmate::assert_string(label)
  ans <- module(
    label,
    server = function(id, data, transformers) {
      checkmate::assert_class(data(), "teal_data")
      moduleServer(id, function(input, output, session) {
        new_data <- srv_teal_data_module("data_transform", data, transformers)
        reactive_datanames <- reactive(teal.data::datanames(new_data()))
        observeEvent(reactive_datanames(), {
          updateSelectInput(
            session = session,
            inputId = "dataname",
            choices = reactive_datanames(),
            selected = restoreInput(session$ns("dataname"), NULL)
          )
        })
        output$text <- renderPrint({
          req(input$dataname)
          new_data()[[input$dataname]]
        })
        teal.widgets::verbatim_popup_srv(
          id = "rcode",
          verbatim_content = reactive(teal.code::get_code(new_data())),
          title = "Example Code"
        )
      })
    },
    ui = function(id, transformers) {
      ns <- NS(id)
      teal.widgets::standard_layout(
        output = verbatimTextOutput(ns("text")),
        encoding = tags$div(
          ui_teal_data_module(ns("data_transform"), transformers),
          selectInput(ns("dataname"), "Choose a dataset", choices = NULL),
          teal.widgets::verbatim_popup_ui(ns("rcode"), "Show R code")
        )
      )
    },
    ui_args = list(transformers = transformers),
    server_args = list(transformers = transformers),
    datanames = datanames
  )
  attr(ans, "teal_bookmarkable") <- TRUE
  ans
}
