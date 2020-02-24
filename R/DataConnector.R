#' A \code{DataConnector} is an R6 class
#'
#' that carries a shiny module that can return a \code{cdisc_data} object as
#' a \code{reactiveVal}.
#' @name DataConnector
#'
DataConnector <- R6::R6Class( #nolint
    "DataConnector",
    public = list(
        #' @description
        #' Create a new \code{DataConnector} object
        #'
        #' @param ui (\code{function}) ui function of a shiny module
        #' @param server (\code{function}) A shiny module server function
        #'   that should return reactive value of type \code{cdisc_data}
        #' @return new \code{DataConnector} object
        initialize = function(ui, server) {
          stopifnot(is(ui, "function"))
          stopifnot(is(server, "function"))
          stopifnot(names(formals(ui)) == "id")
          stopifnot(all(c("input", "output", "session") %in% names(formals(server))))

          private$ui <- ui
          private$server <- server

        },
        #' @description
        #' return the \code{server} function of the DataConnector
        get_server = function() {
          private$server
        },
        #' @description
        #' return the \code{ui} function of the DataConnector
        #' @param id \code{character} shiny element id
        get_ui = function(id) {
          private$ui(id)
        },
        #' @description
        #' Run simple application that uses its \code{ui} and \code{server} fields
        #'
        #' Useful for debugging
        #'
        #' @param data reactiveVal to allow data handling inside server function
        #'
        #' @return An object that represents the app
        launch = function(data = reactiveVal(NULL)) {
          shinyApp(
              ui = fluidPage(private$ui(id = "main_app")),
              server = function(input, output, session) {
                callModule(private$server, id = "main_app", data)
              }
          )
        }
    ),
    private = list(
        server = NULL,
        ui = NULL
    )
)

#' Function to create an object of class DataConnector
#'
#' @param ui (\code{function}) A shiny module UI function with the argument \code{ui}
#' @param server (\code{function}) A shiny module SERVER function with the
#'   arguments \code{input}, \code{output}, \code{session} and one additional
#'   argument to handover data to the final teal app
#'
#' @return An object of class \code{DataConnector}
data_connector <- function(ui, server) {
  DataConnector$new(ui = ui, server = server)
}

#' Dummy random CDISC data connector
#'
#' @export
#'
#' @examples
#' app <- init(
#'   data = rcd_connector(),
#'   modules = root_modules(
#'     module(
#'       "ADSL AGE histogram",
#'       server = function(input, output, session, datasets) {
#'         output$hist <- renderPlot(
#'           hist(datasets$get_data("ADSL", filtered = TRUE, reactive = TRUE)$AGE)
#'         )
#'       },
#'       ui = function(id, ...) {ns <- NS(id); plotOutput(ns('hist'))},
#'       filters = "ADSL"
#'     )
#'   ),
#'   filter = NULL,#list(init = list(ADSL = c("AGE"))),
#'   header = tags$h1("Sample App"),
#'   footer = tags$p("Copyright 2017")
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
rcd_connector <- function() {

  if (!requireNamespace("random.cdisc.data", quietly = TRUE)) {
    # otherwise need to move random.cdisc.data to Imports
    stop("need random.cdisc.data package installed to use the rcd_connector function")
  }

  data_connector(
    ui = function(id) {
      ns <- NS(id)
      tagList(
        numericInput(ns("seed"), "Choose seed", min = 1, max = 1000, value = 1),
        actionButton(ns("submit"), "submit")
      )
    },
    server = function(input, output, session) {
      rv <- reactiveVal(NULL)
      observeEvent(input$submit, {
        rv(cdisc_data(cdisc_dataset("ADSL", random.cdisc.data::radsl(seed = input$seed))))
      })
      return(rv)
    }
  )
}
