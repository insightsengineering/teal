#' \code{RelationalDataConnector} connector for \code{DataSetDB}
#'
#' @description `r lifecycle::badge("experimental")`
#' Build data connector for \code{DataSetDB} datasets
#'
#' @export
#'
#' @param ... (\code{DatasetConnector} objects)\cr
#'  dataset connectors created using \code{\link{datasetdb_dataset_connector}}
#' @param connection (\code{DataConnection}) object returned from \code{datasetdb_connection}.
#'
#' @return An object of class \code{RelationalDataConnector}
#'
#' @examples
#'
#' if (all(c("dsassembly", "gdbauth") %in% installed.packages())) {
#'   x <- datasetdb_data(
#'     datasetdb_dataset_connector("MAE", id = "DS000000267")
#'   )
#'   app <- init(
#'     data = teal_data(x),
#'     modules = root_modules(
#'       module(
#'         "dummy module",
#'         server = function(input, output, session, datasets) {
#'           output$str <- renderText({
#'             paste0(capture.output(str(
#'               datasets$get_data("MAE", filtered = TRUE)
#'             )), collapse = "\n")
#'           })
#'         },
#'         ui = function(id, ...) {ns <- NS(id); verbatimTextOutput(ns('str'))},
#'         filters = "MAE"
#'       )
#'     ),
#'     header = tags$h1("Sample App")
#'   )
#' }
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
datasetdb_data <- function(..., connection = datasetdb_connection()) {
  connectors <- list(...)
  stopifnot(is_class_list("DatasetConnector")(connectors))
  stopifnot(inherits(connection, "DataConnection"))

  x <- if (any(vapply(connectors, is, logical(1), "CDISCDatasetConnector"))) {
    CDISCDataConnector$new(connection = connection, connectors = connectors)
  } else {
    RelationalDataConnector$new(connection = connection, connectors = connectors)
  }

  x$set_check(FALSE)

  x$set_ui(
    function(id, connection, connectors) {
      ns <- NS(id)
      div(
        div(
          h1("TEAL - Access data on DataSetDB using dsassembly"),
          br(),
          h5("Data access requested for:"),
          fluidRow(
            column(
              11,
              offset = 1,
              lapply(seq_along(connectors), function(i) {
                x <- connectors[[i]]
                tags$li(
                  code(x$get_dataname()),
                  ": ",
                  "ID: ", code(x$get_pull_args()$id),
                  if_not_null(x$get_pull_args()$version, paste(", Version:", x$get_pull_args()$version)),
                  if_not_null(x$get_pull_args()$experiments,
                              paste(", Experiments:", paste0(x$get_pull_args()$experiments, collapse = ", ")))
                )
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
      if (!is.null(connection$get_open_server())) {
        callModule(connection$get_open_server(),
                   id = "open_connection",
                   connection = connection)
      }

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
