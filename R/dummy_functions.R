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
example_module <- function(label = "example teal module", datanames = "all", transforms = list(),
                           decorators = teal_transform_module()) {
  checkmate::assert_string(label)
  ans <- module(
    label,
    server = function(id, data, decorators) {
      checkmate::assert_class(isolate(data()), "teal_data")
      moduleServer(id, function(input, output, session) {
        datanames_rv <- reactive(names(req(data())))
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

        table_data <- reactive({
          within(data(),
            {
              table <- dataname
            },
            dataname = as.name(input$dataname)
          )
        })

        table_data_decorated <- srv_transform_data("decorate", data = table_data, transforms = decorators)

        output$text <- renderPrint({
          req(table_data_decorated)
          table_data_decorated()[['table']]
        })

        teal.widgets::verbatim_popup_srv(
          id = "rcode",
          verbatim_content = reactive(teal.code::get_code(req(table_data_decorated()))),
          title = "Example Code"
        )
      })
    },
    ui = function(id, decorators) {
      ns <- NS(id)
      div(
        teal.widgets::standard_layout(
          output = verbatimTextOutput(ns("text")),
          encoding = tags$div(
            selectInput(ns("dataname"), "Choose a dataset", choices = NULL),
            teal.widgets::verbatim_popup_ui(ns("rcode"), "Show R code")
          )
        ),
        ui_transform_data(ns("decorate"), transforms = decorators)
      )
    },
    ui_args = list(decorators = decorators),
    server_args = list(decorators = decorators),
    datanames = datanames,
    transforms = transforms
  )
  attr(ans, "teal_bookmarkable") <- TRUE
  ans
}
