#' An example `teal` module
#'
#' `r lifecycle::badge("experimental")`
#'
#' This module creates an object called `object` that can be modified with decorators.
#' The `object` is determined by what's selected in `Choose a dataset` input in UI.
#' The object can be anything that can be handled by `renderPrint()`.
#' See the `vignette("decorate-modules-output", package = "teal")` or [`teal_transform_module`]
#' to read more about decorators.
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
example_module <- function(label = "example teal module",
                           datanames = "all",
                           transformators = list(),
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
          req(input$dataname)
          within(data(),
            {
              object <- dataname
            },
            dataname = as.name(input$dataname)
          )
        })

        table_data_decorated <- srv_teal_transform_data("decorate", data = table_data, transformators = decorators)

        output$text <- renderPrint({
          req(table_data_decorated)
          table_data_decorated()[["object"]]
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
        ui_teal_transform_data(ns("decorate"), transformators = decorators)
      )
    },
    ui_args = list(decorators = decorators),
    server_args = list(decorators = decorators),
    datanames = datanames,
    transformators = transformators
  )
  attr(ans, "teal_bookmarkable") <- TRUE
  ans
}
