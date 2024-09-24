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
    server = function(id, data) {
      checkmate::assert_class(isolate(data()), "teal_data")
      moduleServer(id, function(input, output, session) {
        datanames_rv <- reactive(ls(teal.code::get_env((req(data())))))
        observeEvent(datanames_rv(), {
          selected <- input$dataname
          if (identical(selected, "")) {
            selected <- restoreInput(session$ns("dataname"), NULL)
          } else if (isFALSE(selected %in% datanames_rv())) {
            selected <- datanames_rv()[1]
          }
          updateSelectInput(
            session = session,
            inputId = "dataname",
            choices = datanames_rv(),
            selected = selected
          )
        })

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
        encoding = tags$div(
          selectInput(ns("dataname"), "Choose a dataset", choices = NULL),
          teal.widgets::verbatim_popup_ui(ns("rcode"), "Show R code")
        )
      )
    },
    datanames = datanames,
    transformers = transformers
  )
  attr(ans, "teal_bookmarkable") <- TRUE
  ans
}
