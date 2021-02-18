#' \code{RelationalDataConnector} connector for \code{random.cdisc.data}
#'
#' @description `r lifecycle::badge("experimental")`
#' Build data connector for \code{random.cdisc.data} functions or datasets
#'
#' @export
#'
#' @param ... (\code{DatasetConnector}) dataset connectors created using \code{\link{rcd_dataset_connector}}
#'   In case \code{cached = FALSE}, please watch the order and call \code{ADSL} generation first.
#' @param connection (\code{DataConnection}) object returned from \code{rcd_connection}.
#' @param check optional, (\code{logical}) whether perform reproducibility check
#'
#' @details
#'
#' This data connector can load data from \code{random.cdisc.data}. datasets can be loaded
#' from a seed or cached. In case datasets should be loaded from cache, the
#' \code{\link{rcd_dataset_connector}} needs to be used with the cached argument set
#' to \code{TRUE} for all datasets.
#'
#' In case non-cached datasets should be used, please watch the order of datasets. Most
#' of the datasets from \code{random.cdisc.data} need \code{ADSL} to be produced first. So
#' please create the \code{ADSL} dataset first.
#'
#' @return An object of class \code{RelationalDataConnector}
#'
#' @examples
#' library(random.cdisc.data)
#' x <- rcd_data(
#'   rcd_cdisc_dataset_connector("ADSL", radsl, cached = TRUE),
#'   rcd_cdisc_dataset_connector("ADLB", radlb, cached = TRUE)
#' )
#'
#' # add the data to a proper teal app
#' app <- init(
#'   data = cdisc_data(x),
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
rcd_data <- function(..., connection = rcd_connection(), check = TRUE) {
  connectors <- list(...)
  stopifnot(is_class_list("DatasetConnector")(connectors))
  stopifnot(inherits(connection, "DataConnection"))
  stopifnot(is_logical_single(check))

  x <- if (any(vapply(connectors, is, logical(1), "CDISCDatasetConnector"))) {
    CDISCDataConnector$new(connection = connection, connectors = connectors)
  } else {
    RelationalDataConnector$new(connection = connection, connectors = connectors)
  }

  x$set_check(check)

  x$set_ui(
    function(id) {
      ns <- NS(id)
      tagList(
        connection$get_open_ui(ns("open_connection")),
        numericInput(ns("seed"), p("Choose", code("seed")), min = 1, max = 1000, value = 1),
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
          connection = connection
        )
      }
      if (connection$is_opened()) {
        for (connector in connectors) {
          # set_args before to return them in the code (fixed args)
          set_args(connector, args = list(seed = input$seed))

          # pull each dataset
          callModule(connector$get_server(), id = connector$get_dataname())
          if (connector$is_failed()) {
            break
          }
        }

        if (!is.null(connection$get_close_server())) {
          callModule(connection$get_close_server(),
            id = "close_connection",
            connection = connection
          )
        }
      }
    }
  )

  return(x)
}


#' \code{RelationalDataConnector} connector for \code{RICE}
#'
#' @description `r lifecycle::badge("experimental")`
#' Build data connector for \code{RICE} datasets
#'
#' @export
#'
#' @param ... (\code{DatasetConnector} objects)\cr
#'  dataset connectors created using \code{\link{rice_dataset_connector}}
#' @param connection (\code{DataConnection}) object returned from \code{rice_connection}.
#' @param additional_ui (\code{shiny.tag})\cr
#'  additional user interface to be visible over login panel
#'
#' @return An object of class \code{RelatonalDataConnector}
#'
#' @examples
#'
#' x <- rice_data(
#'   rice_cdisc_dataset_connector("ADSL", "/path/to/ADSL"),
#'   rice_cdisc_dataset_connector("ADLB", "/path/to/ADLB")
#' )
#' app <- init(
#'   data = cdisc_data(x),
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
rice_data <- function(..., connection = rice_connection(), additional_ui = NULL) {
  connectors <- list(...)
  stopifnot(is_class_list("DatasetConnector")(connectors))
  stopifnot(inherits(connection, "DataConnection"))
  stopifnot(is.null(additional_ui) || is_html_like(additional_ui))

  x <- if (any(vapply(connectors, is, logical(1), "CDISCDatasetConnector"))) {
    CDISCDataConnector$new(connection = connection, connectors = connectors)
  } else {
    RelationalDataConnector$new(connection = connection, connectors = connectors)
  }

  x$set_check(FALSE)

  x$set_ui(
    function(id) {
      ns <- NS(id)
      div(
        div(
          h1("TEAL - Access data on entimICE using rice"),
          br(),
          h5("Data access requested for:"),
          fluidRow(
            column(
              11,
              offset = 1,
              lapply(seq_along(connectors), function(i) {
                tags$li(code(connectors[[i]]$get_dataname()),
                        ": ",
                        code(connectors[[i]]$get_pull_args()$node))
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
        for (connector in connectors) {
          callModule(connector$get_server(), id = connector$get_dataname())
          if (connector$is_failed()) {
            break
          }
        }

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



#' \code{RelationalDataConnector} connector for \code{TERADATA}
#'
#' @description `r lifecycle::badge("experimental")`
#' Build data connector for \code{TERADATA} functions or datasets
#'
#' @export
#'
#' @param ... (\code{DatasetConnector}) dataset connectors created using \code{\link{teradata_dataset_connector}}
#' @param connection (\code{DataConnection}) object returned from \code{teradata_connection}.
#'
#' @return An object of class \code{RelationalDataConnector}
#'
#' @examples
#' \dontrun{
#' x <- teradata_data(
#'   teradata_cdisc_dataset_connector("ADSL", "ADSL_table"),
#'   teradata_cdisc_dataset_connector("ADAE", "ADAE_table"),
#'   connection = teradata_connection(open_args = list(datalab = "my_custom_datalab"))
#' )
#'
#' # add the data to a proper teal app
#' app <- init(
#'   data = teal_data(x),
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
#' shinyApp(app$ui, app$server)
#' }
teradata_data <- function(..., connection = teradata_connection()) {
  connectors <- list(...)
  stopifnot(is_class_list("DatasetConnector")(connectors))

  x <- if (any(vapply(connectors, is, logical(1), "CDISCDatasetConnector"))) {
    CDISCDataConnector$new(connection = connection, connectors = connectors)
  } else {
    RelationalDataConnector$new(connection = connection, connectors = connectors)
  }

  x$set_check(FALSE)

  x$set_ui(
    function(id) {
      ns <- NS(id)
      div(
        div(
          h1("TEAL - Access data on Teradata"),
          br(),
          h5("Data access requested for:"),
          fluidRow(
            column(
              11,
              offset = 1,
              lapply(seq_along(connectors), function(i) {
                tags$li(code(connectors[[i]]$get_dataname()),
                        ": ",
                        code(connectors[[i]]$get_pull_args()$name))
              })
            )
          )
        ),
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

      if (connection$is_opened()) {
        conn <- connection$get_conn()
        if (!is.null(conn)) {
          for (connector in connectors) {
            connector$get_pull_callable()$assign_to_env("conn", conn)
          }
        }

        # call connectors$pull
        for (connector in connectors) {
          callModule(connector$get_server(), id = connector$get_dataname())
          if (connector$is_failed()) {
            break
          }
        }

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
