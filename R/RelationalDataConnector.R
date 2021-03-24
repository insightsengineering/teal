# RelationalDataConnector ------
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @title Manage multiple and \code{DatasetConnector} of the same type.
#'
#' @description
#' Class manages \code{DatasetConnector} to specify additional dynamic arguments and to
#' open/close connection.
#'
#' @param connection (\code{DataConnection})\cr
#'   connection to data source
#' @param connectors (\code{list} of \code{DatasetConnector} elements)\cr
#'   list with dataset connectors
#'
#' @examples
#' library(random.cdisc.data)
#' adsl <- rcd_cdisc_dataset_connector("ADSL", radsl)
#' adlb <- rcd_cdisc_dataset_connector("ADLB", radlb, ADSL = adsl)
#' con <- teal:::rcd_connection()
#'
#' ui <- function(id, ...) {
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
#'
RelationalDataConnector <- R6::R6Class( #nolint
  classname = "RelationalDataConnector",
  inherit = DataAbstract,

  ## __Public Methods ====
  public = list(
    #' @description
    #' Create a new `RelationalDataConnector` object
    initialize = function(connection, connectors) {
      stopifnot(is_class_list("DatasetConnector")(connectors))

      connectors_names <- vapply(connectors, get_dataname, character(1))
      connectors <- setNames(connectors, connectors_names)

      private$check_names(connectors_names)

      if (!missing(connection)) {
        stopifnot(is(connection, "DataConnection"))
        private$connection <- connection
      }

      private$datasets <- connectors

      private$pull_code <- CodeClass$new()
      private$mutate_code <- CodeClass$new()

      return(invisible(self))
    },

    # ___ getters ====
    #' @description
    #' Get connection to data source
    #'
    #' @return connector's connection
    get_connection = function() {
      return(private$connection)
    },
    #' @description
    #' Get internal \code{CodeClass} object
    #'
    #' @return `\code{CodeClass}`
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
    #'
    #' @return the \code{server} function
    get_server = function() {
      if (is.null(private$server)) {
        stop("No server function set yet. Please use set_server method first.")
      }
      function(input, output, session, connection = private$connection, connectors = private$datasets) {
        rv <- reactiveVal(NULL)
        callModule(private$server, id = "data_input", connection = connection, connectors = connectors)

        if (self$is_pulled()) {
          return(rv(TRUE))
        } else {
          return(rv(FALSE))
        }
      }
    },
    #' @description
    #' Get Shiny module with inputs for all \code{DatasetConnector} objects
    #'
    #' @param id \code{character} shiny element id
    #'
    #' @return the \code{ui} function
    get_ui = function(id) {
      if (is.null(private$ui)) {
        stop("No UI set yet. Please use set_ui method first.")
      }
      x <- function(id, connection = private$connection, connectors = private$datasets) {
        ns <- NS(id)
        tags$div(
          h3("Data Connector for:", lapply(self$get_datanames(), code)),
          tags$div(
            id = ns("data_input"),
            private$ui(id = ns("data_input"), connection = connection, connectors = connectors)
          )
        )
      }
      x(id)
    },

    # ___ setters ====
    #' @description
    #' Set argument to the \code{pull_fun}
    #'
    #' @param args (named \code{list})\cr
    #'  arguments values as separate list elements named by argument name. These arguments
    #'  are passed to each dataset.
    #'
    #' @return nothing
    set_pull_args = function(args) {
      lapply(private$datasets, function(x) set_args(x, args))
      return(invisible(NULL))
    },
    #' @description
    #' Set connector UI function
    #'
    #' @param f (\code{function})\cr
    #'  shiny module as function. Inputs specified in this \code{ui} are passed to server module
    #'  defined by \code{set_server} method.
    #'
    #' @return nothing
    set_ui = function(f) {
      stopifnot(is(f, "function"))
      stopifnot("id" %in% names(formals(f)))
      stopifnot(all(c("connection", "connectors") %in% names(formals(f))) || "..." %in% names(formals(f)))

      private$ui <- f
      return(invisible(NULL))
    },
    #' @description
    #' Set connector server function
    #'
    #' This function will be called after submit button will be hit. There is no possibility to
    #' specify some dynamic \code{ui} as \code{server} function is executed after hitting submit
    #' button.
    #'
    #' @param f (\code{function})\cr
    #'  A shiny module server function that should load data from all connectors
    #'
    #' @return \code{NULL} or \code{self} if data is loaded.
    set_server = function(f) {
      stopifnot(is(f, "function"))
      stopifnot(all(c("input", "output", "session") %in% names(formals(f))))

      private$server <- f
      return(invisible(NULL))
    },

    # ___ pull ====
    #' @description
    #' Load data from each \code{DatasetConnector}
    #'
    #' @param con_args (\code{NULL} or named \code{list})\cr
    #'   additional dynamic arguments for connection function. \code{args} will be passed to each
    #'  \code{DatasetConnector} object to evaluate \code{CallableFunction} assigned to
    #'  this dataset. If \code{args} is null than default set of arguments will be used, otherwise
    #'  call will be executed on these arguments only (arguments set before will be ignored).
    #'  \code{pull} function doesn't update reproducible call, it's just evaluate function.
    #'
    #' @param args (\code{NULL} or named \code{list})\cr
    #'  additional dynamic arguments to pull dataset. \code{args} will be passed to each
    #'  \code{DatasetConnector} object to evaluate \code{CallableFunction} assigned to
    #'  this dataset. If \code{args} is null than default set of arguments will be used, otherwise
    #'  call will be executed on these arguments only (arguments set before will be ignored).
    #'  \code{pull} function doesn't update reproducible call, it's just evaluate function.
    #'
    #' @param try (\code{logical} value)\cr
    #'  whether perform function evaluation inside \code{try} clause
    #'
    #' @return (`self`) invisibly for chaining. In order to get the data please use \code{get_datasets} method.
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
          include_teal_css_js(),
          shinyjs::useShinyjs(),
          fluidRow(
            column(
              width = 8,
              offset = 2,
              tags$div(
                id = "data_inputs",
                self$get_ui(id = "data_connector"),
                actionButton("submit", "Submit"),
                `data-proxy-click` = "submit"
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

    # ___ status ====
    #' @description
    #' Check if pull or connection has not failed.
    #'
    #' @return \code{TRUE} if pull or connection failed, else \code{FALSE}
    is_failed = function() {
      private$connection$is_failed() ||
        any(vapply(private$datasets, function(x) x$is_failed(), logical(1)))
    }
  ),
  ## __Private Fields ====
  private = list(
    server = NULL, # shiny server function
    ui = NULL, # shiny ui function
    connection = NULL, # DataConnection

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
    }
  )
)
