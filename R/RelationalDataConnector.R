# RelationalDataConnector ------
#' @title Manage multiple and \code{RelationalDatasetConnector} of the same type.
#' @description
#' Class manages \code{RelationalDatasetConnector} to specify additional dynamic arguments and to
#' open/close connection.
#'
#' @importFrom R6 R6Class
#' @importFrom shinyjs enable
#'
#' @examples
#' library(random.cdisc.data)
#' adsl <- rcd_cdisc_dataset_connector("ADSL", radsl)
#' adlb <- rcd_cdisc_dataset_connector("ADLB", radlb, ADSL = adsl)
#' con <- teal:::rcd_connection()
#'
#' ui <- function(id) {
#'   ns <- NS(id)
#'   tagList(
#'     numericInput(ns("seed"), "Choose seed", min = 1, max = 1000, value = 1),
#'     sliderInput(ns("N"), "Choose number of observations", min = 1, max = 400, value = 10)
#'   )
#' }
#'
#' x <- teal:::RelationalDataConnector$new(connection = con, connectors = list(adsl, adlb))
#'
#' x$set_ui(ui)
#' x$set_server(function(input, output, session, connectors, connection) {
#'   lapply(connectors, function(connector) {
#'     if (get_dataname(connector) == "ADSL") {
#'       set_args(connector, args = list(seed = input$seed, N = input$N))
#'     } else {
#'       set_args(connector, args = list(seed = input$seed))
#'     }
#'     connector$pull(try = TRUE)
#'   })
#' })
#' \dontrun{
#' x$launch()
#' x$get_datasets()
#' }
#' @importFrom R6 R6Class
#' @importFrom shinyjs enable
#' @importFrom methods is
RelationalDataConnector <- R6::R6Class( #nolint
  classname = "RelationalDataConnector",
  inherit = RelationalDataCollection,
  ## __Public Methods ====
  public = list(
    #' @description
    #' Create a new \code{RelationalDataConnector} object
    #'
    #' @param connection (\code{DataConnection}) connection to data source
    #' @param connectors (\code{list} of \code{RelationalDatasetConnector} elements) list with dataset connectors
    #'
    #' @return new \code{RelationalDataConnector} object
    initialize = function(connection, connectors) {
      stopifnot(is_class_list("RelationalDatasetConnector")(connectors))

      if (!missing(connection)) {
        stopifnot(is(connection, "DataConnection"))
        private$connection <- connection
      }

      connectors_names <- vapply(connectors, get_dataname, character(1))
      connectors <- setNames(connectors, connectors_names)

      private$check_names(connectors)

      private$datasets <- connectors

      private$pull_code <- CodeClass$new()
      private$mutate_code <- CodeClass$new()

      invisible(self)
    },
    #' @description
    #' Get internal \code{CodeClass} object
    #'
    #' @return \code{CodeClass}
    get_code_class = function() {
      all_code <- CodeClass$new()

      open_connection_code <- if_not_null(private$connection, private$connection$get_open_call(deparse = TRUE))
      if_not_null(open_connection_code, all_code$set_code(open_connection_code, dataname = "*open"))

      datasets_code_class <- private$get_datasets_code_class()
      all_code$append(datasets_code_class)

      close_connection_code <- if_not_null(
        private$connection,
        private$connection$get_close_call(deparse = TRUE, silent = TRUE))
      if_not_null(close_connection_code, all_code$set_code(close_connection_code, dataname = "*close"))

      mutate_code_class <- private$get_mutate_code_class()
      all_code$append(mutate_code_class)

      return(all_code)
    },
    #' @description
    #' Get connection to data source
    #'
    #' @return connector's connection
    get_connection = function() {
      return(private$connection)
    },
    #' @description
    #'
    #' @return the \code{server} function of the \code{RelationalDataConnector}
    get_server = function() {
      return(private$server)
    },
    #' @description
    #' Get Shiny module with inputs for all \code{RelationalDatasetConnector} objects
    #'
    #' @param id \code{character} shiny element id
    #'
    #' @return the \code{ui} function of the \code{RelationalDataConnector}
    get_ui = function(id) {
      if (is.null(private$ui)) {
        stop("No UI set yet. Please use set_ui method first.")
      }
      return(private$ui(id))
    },
    #' @description
    #' Check if pull or connection has not failed.
    #'
    #' @return \code{TRUE} if pull or connection failed, else \code{FALSE}
    is_failed = function() {
      private$connection$is_failed() ||
        any(vapply(private$datasets, function(x) x$is_failed(), logical(1)))
    },

    #' @description
    #' Run simple application that uses its \code{ui} and \code{server} fields to pull data from
    #' connection.
    #'
    #' Useful for debugging
    #'
    #' @return An object that represents the app
    launch = function() {
      # load RelationDatasetConnector objects
      if (self$is_pulled()) {
        stop("All the datasets have already been pulled.")
      }

      shinyApp(
        ui = fluidPage(
          useShinyjs(),
          fluidRow(
            column(
              width = 8,
              offset = 2,
              tags$div(
                id = "data_inputs",
                private$ui(id = "data_connector"),
                actionButton("submit", "Submit")
              ),
              shinyjs::hidden(
                tags$div(
                  id = "data_loaded",
                  div(
                    h3("Data successfully loaded."),
                    p("You can close this window and get back to R console.")
                  )
                )
              )

            )
          )
        ),
        server = function(input, output, session) {
          session$onSessionEnded(stopApp)
          observeEvent(input$submit, {
            rv <- reactiveVal(NULL)
            rv(
              callModule(self$get_server(),
                         id = "data_connector",
                         connection = private$connection,
                         connectors = private$datasets)
            )

            observeEvent(rv(), {
              if (self$is_pulled()) {
                removeUI(sprintf("#%s", session$ns("data_inputs")))
                shinyjs::show("data_loaded")
                stopApp()
              }
            })

          })
        }
      )
    },
    #' @description
    #' Load data from each \code{RelationalDatasetConnector}
    #'
    #' @param con_args (\code{NULL} or named \code{list})\cr
    #'   additional dynamic arguments for connection function. \code{args} will be passed to each
    #'  \code{RelationalDatasetConnector} object to evaluate \code{CallableFunction} assigned to
    #'  this dataset. If \code{args} is null than default set of arguments will be used, otherwise
    #'  call will be executed on these arguments only (arguments set before will be ignored).
    #'  \code{pull} function doesn't update reproducible call, it's just evaluate function.
    #'
    #' @param args (\code{NULL} or named \code{list})\cr
    #'  additional dynamic arguments to pull dataset. \code{args} will be passed to each
    #'  \code{RelationalDatasetConnector} object to evaluate \code{CallableFunction} assigned to
    #'  this dataset. If \code{args} is null than default set of arguments will be used, otherwise
    #'  call will be executed on these arguments only (arguments set before will be ignored).
    #'  \code{pull} function doesn't update reproducible call, it's just evaluate function.
    #'
    #' @param try (\code{logical} value)\cr
    #'  whether perform function evaluation inside \code{try} clause
    #'
    #' @return \code{self} invisibly for chaining. In order to get the data please use \code{get_datasets} method.
    pull = function(con_args = NULL, args = NULL, try = TRUE) {
      # open connection
      if (!is.null(private$connection)) {
        private$connection$open(args = con_args, try = try)

        conn <- private$connection$get_conn()
        if (!is.null(conn)) {
          for (connector in private$datasets) {
            connector$get_pull_callable()$assign_to_env("conn", conn)
          }
        }
      }

      # load datasets
      for (dataset in private$datasets) {
        load_dataset(dataset, args = args)
      }

      # close connection
      if_not_null(private$connection, private$connection$close(silent = TRUE))

      return(invisible(self))
    },
    #' @description
    #' Set connector UI function
    #'
    #' @param data_input (\code{function})\cr
    #'  shiny module as function. Inputs specified in this \code{ui} are passed to server module
    #'  defined by \code{set_server} method.
    #'
    #' @return nothing
    set_ui = function(data_input) {
      stopifnot(is(data_input, "function"))
      stopifnot(identical(names(formals(data_input)), "id"))

      private$ui <- function(id) {
        ns <- NS(id)
        tags$div(
          h3("Data Connector for:", lapply(self$get_datanames(), code)),
          tags$div(
            id = ns("data_input"),
            data_input(id = ns("data_input"))
          )
        )
      }

      return(invisible(NULL))
    },
    #' @description
    #' Set connector server function
    #'
    #' This function will be called after submit button will be hit. There is no possibility to
    #' specify some dynamic \code{ui} as \code{server} function is executed after hitting submit
    #' button.
    #'
    #' @param data_module (\code{function})\cr
    #'  A shiny module server function that should load data from all connectors
    #'
    #' @return \code{NULL} or \code{self} if data is loaded.
    set_server = function(data_module) {
      stopifnot(is(data_module, "function"))
      stopifnot(all(c("input", "output", "session") %in% names(formals(data_module))))

      private$server <- function(input, output, session, connection, connectors) {
        rv <- reactiveVal(NULL)
        callModule(data_module, id = "data_input", connection = connection, connectors = connectors)

        if (self$is_pulled()) {
          return(rv(TRUE))
        } else {
          return(rv(FALSE))
        }
      }

      return(invisible(NULL))
    }
  ),
  ## __Private Fields ====
  private = list(
    server = NULL,
    ui = NULL,
    connection = NULL,
    ## __Private Methods ====

    # adds open/close connection code at beginning/end of the dataset code
    append_connection_code = function() {
      lapply(
        private$datasets,
        function(connector) {
          dataset <- get_dataset(connector)
          try(
            dataset$set_code(code = paste(
              c(
                if_not_null(private$connection, private$connection$get_open_call(deparse = TRUE)),
                get_code(dataset, deparse = TRUE, FUN.VALUE = character(1)),
                if_not_null(private$connection, private$connection$get_close_call(deparse = TRUE, silent = TRUE))
              ),
              collapse = "\n"
            ))
          )
        }
      )

    },
    stop_on_error = function(x, submit_id = character(0), progress = NULL) {
      if (is(x, "try-error")) {
        private$connection$close(silent = TRUE)
        if (shiny::isRunning()) {
          shinyjs::enable(submit_id)
          if_not_null(progress, progress$close())
        }
        error_dialog(x)
      } else {
        x
      }
    }
  )
)
