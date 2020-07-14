#' \code{RelationalDataConnector} connector for \code{random.cdisc.data}
#'
#' Build data connector for \code{random.cdisc.data} functions or datasets
#'
#' @export
#'
#' @param ... (\code{RelationalDatasetConnector}) dataset connectors created using \link{rcd_dataset_connector}
#'   In case \code{cached = FALSE}, please watch the order and call \code{ADSL} generation first.
#' @param check optional, (\code{logical}) whether perform reproducibility check
#'
#' @details
#'
#' This data connector can load data from \code{random.cdisc.data}. datasets can be loaded
#' from a seed or cached. In case datasets should be loaded from cached the
#' \link{rcd_cdisc_dataset_connector} needs to be used with the cached argument set
#' to \code{TRUE} for all datasets.
#'
#' In case non-cached datasets should be used, please watch the order of datasets. Most
#' of the datasets from \code{random.cdisc.data} need \code{ADSL} to be produced first. So
#' please create the \code{ADSL} dataset first.
#' @param check optional, (\code{logical}) whether perform reproducibility check
#'
#' @return An object of class \code{RelationalDataConnector}
#'
#' @examples
#' library(random.cdisc.data)
#' x <- rcd_cdisc_data(
#'   rcd_cdisc_dataset_connector("ADSL", radsl, cached = TRUE),
#'   rcd_cdisc_dataset_connector("ADLB", radlb, cached = TRUE)
#' )
#'
#' # add the data to a proper teal app
#' app <- init(
#'   data = x,
#'   modules = root_modules(
#'     module(
#'       "ADSL AGE histogram",
#'       server = function(input, output, session, datasets) {
#'         output$hist <- renderPlot(
#'           hist(datasets$get_data("ADSL", filtered = TRUE)$AGE)
#'         )
#'       },
#'       ui = function(id, ...) {ns <- NS(id); plotOutput(ns('hist'))},
#'       filters = "ADSL"
#'     )
#'   ),
#'   header = tags$h1("Sample App")
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
rcd_cdisc_data <- function(..., check = TRUE) {
  connectors <- list(...)
  stopifnot(is_class_list("RelationalDatasetConnector")(connectors))
  connection <- rcd_connection()

  x <- RelationalDataConnector$new(connection = connection, connectors = connectors)
  x$set_check(check)

  x$set_ui(
    function(id) {
      ns <- NS(id)
      tagList(
        connection$get_open_ui(ns("open_connection")),
        numericInput(ns("seed"), "Choose seed", min = 1, max = 1000, value = 1),
        do.call(
          what = "tagList",
          args = lapply(
            connectors,
            function(connector) {
              div(
                connector$get_ui(
                  id = ns(connector$get_dataname())
                ),
                br()
              )
            }
          )
        )
      )
    }
  )

  x$set_server(
    function(input, output, session, connection, connectors) {
      # opens connection
      if (!is.null(connection$get_open_server())) {
        callModule(connection$get_open_server(),
                   id = "open_connection",
                   connection = connection)
      }

      lapply(connectors, function(connector) {
        # set_args before to return them in the code (fixed args)
        set_args(connector, args = list(seed = input$seed))

        # pull each dataset
        callModule(connector$get_server(),
                   id = connector$get_dataname())

      })
    }
  )

  return(x)
}


#' Data connector for \code{RICE}
#'
#' Build data connector for \code{RICE} datasets
#'
#' @importFrom askpass askpass
#'
#' @export
#'
#' @param ... (\code{RelationalDatasetConnector} objects)\cr
#'  dataset connectors created using \link{rice_dataset_connector}
#' @param additional_ui (\code{shiny.tag})\cr
#'  additional user interface to be visible over login panel
#'
#' @return An object of class \code{RelatonalDataConnector}
#'
#' @examples
#'
#' x <- rice_cdisc_data(
#'   rice_cdisc_dataset_connector("ADSL", "/path/to/ADSL"),
#'   rice_cdisc_dataset_connector("ADLB", "/path/to/ADLB")
#' )
#' app <- init(
#'   data = x,
#'   modules = root_modules(
#'     module(
#'       "ADSL AGE histogram",
#'       server = function(input, output, session, datasets) {
#'         output$hist <- renderPlot({
#'           hist(datasets$get_data("ADSL", filtered = TRUE)$AGE)
#'         })
#'       },
#'       ui = function(id, ...) {ns <- NS(id); plotOutput(ns('hist'))},
#'       filters = "ADSL"
#'     )
#'   ),
#'   header = tags$h1("Sample App")
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
rice_cdisc_data <- function(..., additional_ui = NULL) {
  connectors <- list(...)
  stopifnot(is_class_list("RelationalDatasetConnector")(connectors))

  connection <- rice_connection()

  x <- RelationalDataConnector$new(connection = connection, connectors = connectors)
  x$set_check(`attributes<-`(FALSE, list(quiet = TRUE)))

  x$set_ui(
    function(id) {
      ns <- NS(id)
      div(
        div(
          h1("TEAL - Access data on entimICE using RICE"),
          br(),
          h5("Data access requested for:"),
          fluidRow(
            column(
              11,
              offset = 1,
              lapply(seq_along(connectors), function(i) {
                tags$li(paste0(connectors[[i]]$get_dataname(),
                               ": ",
                               connectors[[i]]$args$node))
              })
            )
          )
        ),
        br(),
        additional_ui,
        br(),
        connection$get_open_ui(ns("open_connection"))
      )
    }
  )

  x$set_server(
    function(input, output, session, connectors, connection) {
      # opens connection
      if (!is.null(connection$get_open_server())) {
        callModule(connection$get_open_server(),
                   id = "open_connection",
                   connection = connection)
      }

      # rice::rice_read doesn't need arguments from data-level
      if (connection$is_opened()) {
        # call connectors$pull
        lapply(connectors, function(connector) {
          callModule(connector$get_server(),
                     id = connector$get_dataname())
        })

        if (!is.null(connection$get_close_server())) {
          callModule(connection$get_close_server(),
                     id = "close_connection",
                     connection = connection)
        }
      }
    }
  )
  return(x)
}
