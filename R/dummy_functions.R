#' An example `teal` module
#'
#' This module creates an object called `object` that can be modified with decorators.
#' The `object` is determined by what's selected in `Choose a dataset` input in UI.
#' The object can be anything that can be handled by `renderPrint()`.
#' See the `vignette("transform-module-output", package = "teal")` or [`teal_transform_module`]
#' to read more about decorators.
#'
#' @inheritParams teal_modules
#' @param decorators `r lifecycle::badge("experimental")` (`list` of `teal_transform_module`) optional,
#' decorator for `object` included in the module.
#'
#' @return A `teal` module which can be included in the `modules` argument to [init()].
#'
#' @examplesShinylive
#' library(teal)
#' interactive <- function() TRUE
#' {{ next_example }}
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
                           decorators = list()) {
  checkmate::assert_string(label)
  checkmate::assert_list(decorators, "teal_transform_module")

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

        table_data_decorated_no_print <- srv_transform_teal_data(
          "decorate",
          data = table_data,
          transformators = decorators
        )
        table_data_decorated <- reactive(within(req(table_data_decorated_no_print()), expr = object))

        output$text <- renderPrint({
          req(table_data()) # Ensure original errors from module are displayed
          table_data_decorated()[["object"]]
        })

        teal.widgets::verbatim_popup_srv(
          id = "rcode",
          verbatim_content = reactive(teal.code::get_code(req(table_data_decorated()))),
          title = "Example Code"
        )

        table_data_decorated
      })
    },
    ui = function(id, decorators) {
      ns <- NS(id)
      teal.widgets::standard_layout(
        output = verbatimTextOutput(ns("text")),
        encoding = tags$div(
          selectInput(ns("dataname"), "Choose a dataset", choices = NULL),
          ui_transform_teal_data(ns("decorate"), transformators = decorators),
          teal.widgets::verbatim_popup_ui(ns("rcode"), "Show R code")
        )
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

globalVariables("dataname")
