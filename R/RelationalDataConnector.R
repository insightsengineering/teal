# RelationalDataConnector ------
#' @title Manage multiple and \code{RelationalDatasetConnector} of the same type.
#' @description
#' Class manages \code{RelationalDatasetConnector} to specify additional dynamic arguments and to
#' open/close connection.
#'
#' @importFrom R6 R6Class
#' @importFrom shinyjs disable enable
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
#' tc$launch()
#' tc$get_datasets()
#' }
RelationalDataConnector <- R6::R6Class( #nolint
  classname = "RelationalDataConnector",
  inherit = RelationalData,
  # ..public ------
  public = list(
    #' @description
    #' Create a new \code{RelationalDataConnector} object
    #'
    #' @param connection (\code{DataConnection}) connection to data source
    #' @param connectors (\code{list} of \code{RelationalDatasetConnector} elements) list with dataset connectors
    #'
    #' @return new \code{RelationalDataConnector} object
    initialize = function(connection, connectors) {
      if (!missing(connection)) {
        private$set_connection(connection)
      }
      private$set_dataset_connectors(connectors)

      invisible(self)
    },
    #' Get names of the datasets.
    #'
    #' @return \code{character} vector with names of all datasets.
    get_datanames = function() {
      vapply(private$datasets, get_dataname, character(1))
    },
    #' Get pulled \code{RelationalDataset} objects
    #'
    #' @return \code{list} \code{RelationalDataset(s)} named by dataname.
    get_datasets = function() {
      sapply(private$datasets, get_dataset, USE.NAMES = TRUE, simplify = FALSE)
    },
    #' @description
    #' Get \code{RelationalDataset} object.
    #' @param dataname (\code{character} value)\cr
    #'   name of dataset to be returned. If \code{NULL}, all datasets are returned.
    #'
    #' @return \code{RelationalDataset}.
    get_dataset = function(dataname) {
      stopifnot(is_character_single(dataname))
      get_dataname(private$datasets[[dataname]])
    },
    #' Get \code{RelationalDatasetConnector} objects
    #'
    #' @return \code{list} \code{RelationalDatasetConnector(s)} named by dataname.
    get_dataset_connectors = function() {
      private$datasets
    },
    #' @description
    #'
    #' Derive the code for all datasets
    #' @param dataname (\code{character}) dataname or \code{NULL} for all datasets
    #' @param deparse (\code{logical}) whether to return the deparsed form of a call
    #' @return \code{list} of \code{character} containing code
    get_code = function(dataname = NULL, deparse = TRUE) {
      stopifnot(is_logical_single(deparse))

      connection_code <- if_not_null(private$connection, private$connection$get_open_call(deparse = deparse))
      datasets_code <- private$get_code_datasets(dataname = dataname, deparse = deparse)
      mutate_code <- private$get_mutate_code(deparse = deparse)

      all_code <- c(connection_code, datasets_code, mutate_code)

      if (isTRUE(deparse)) {
        all_code <- paste0(all_code, collapse = "\n")
      }

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
      return(private$ui(id))
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
      if (is.null(private$datasets)) {
        stop("the data has no dataset connectors yet")
      }
      if (all(vapply(private$datasets, function(el) el$is_pulled(), logical(1L)))) {
        stop("all the datasets has been already pulled")
      }

      shinyApp(
        ui = fluidPage(
          useShinyjs(),
          fluidRow(
            column(
              width = 8,
              offset = 2,
              tags$div(
                private$ui(id = "data_connector"),
                actionButton("submit", "Submit")
              )
            )
          )
        ),
        server = function(input, output, session) {
          session$onSessionEnded(stopApp)
          observeEvent(input$submit, {
            dat <- callModule(self$get_server(),
                              id = "data_connector",
                              connection = private$connection,
                              connectors = private$datasets)


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
    #' @return nothing, in order to get the data please use \code{get_data} method
    pull = function(con_args = NULL, args = NULL, try = TRUE) {
      # open connection
      if (!is.null(private$connection)) {
        private$connection$open(args = con_args, try = try)
      }

      # load datasets
      for (dataset in private$datasets) {
        load_dataset(dataset, args = args)
      }

      # close connection
      if_not_null(private$connection, private$connection$close(silent = TRUE))

      return(invisible(NULL))
    },
    #' @description
    #' Set arguments to connection and connectors. Using \code{set_args} will save these
    #' arguments in reproducible code unlike using \code{pull} function.
    #'
    #' @param con_args (named \code{list})\cr
    #'   arguments values for opening connection.
    #'
    #' @param args (named \code{list})\cr
    #'   arguments values for function to pull data.
    #'
    #' @return nothing
    set_args = function(con_args = NULL, args = NULL) {
      # set connection args
      if_not_null(con_args, private$connection$set_open_args(args = con_args))

      # set CallableFunction args for each DatasetConnector
      for (dataset in private$datasets) {
        if_not_null(args, set_args(dataset, args = args))
      }

      return(invisible(NULL))
    },
    #' @description
    #' Set reproducibility check
    #'
    #' @param check (\code{logical}) whether to perform reproducibility check
    #'
    #' @return nothing
    set_check = function(check = FALSE) {
      stopifnot(is_logical_single(check))
      private$check <- check
      return(invisible(NULL))
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
    #' @return nothing
    set_server = function(data_module) {
      stopifnot(is(data_module, "function"))
      stopifnot(all(c("input", "output", "session") %in% names(formals(data_module))))

      private$server <- function(input, output, session, connection, connectors) {
        callModule(data_module,
                   id = "data_input",
                   connection = connection,
                   connectors = connectors)

        if (all(vapply(connectors, is_pulled, logical(1)))) {
          private$append_connection_code()
        }

        return(invisible(NULL))
      }

      return(invisible(NULL))
    },
    #' @description
    #' Check if dataset has already been pulled.
    #'
    #' @return \code{TRUE} if connector has been already pulled, else \code{FALSE}.
    is_pulled = function() {
      if (!is.null(private$datasets)) {
        all(vapply(private$datasets, is_pulled, logical(1)))
      } else {
        return(FALSE)
      }
    }
  ),
  # ..private ------
  private = list(
    # ....fields ----
    datasets = NULL,
    server = NULL,
    ui = NULL,
    connection = NULL,
    check = FALSE,
    # ....methods ----

    # adds open/close connection code at beginning/end of the dataset code
    append_connection_code = function() {
      lapply(
        private$datasets,
        function(connector) {
          dataset <- get_dataset(connector)
          try(
            dataset$set_code(code = paste(
              c(
                if_not_null(private$connection,
                            private$connection$get_open_call(deparse = TRUE)),
                get_code(dataset, deparse = TRUE, FUN.VALUE = character(1)),
                if_not_null(private$connection,
                            private$connection$get_close_call(deparse = TRUE, silent = TRUE))
              ),
              collapse = "\n"
            ))
          )
        }
      )

    },
    # Set data connection
    #
    # @param connection (\code{DataConnection}) data connection
    #
    # @return nothing
    set_connection = function(connection) {
      stopifnot(is(connection, "DataConnection"))
      private$connection <- connection
      return(invisible(NULL))
    },
    #\ Set dataset connectors
    #
    # @param connectors (\code{list} of \code{RelationalDatasetConnector} elements) data connectors
    #
    # @return nothing
    set_dataset_connectors = function(connectors) {
      stopifnot(is_class_list("RelationalDatasetConnector")(connectors))
      connector_names <- vapply(connectors, get_dataname, character(1))
      if (any(duplicated(connector_names))) {
        stop("Connector names should be unique")
      }
      if (any(connector_names %in% self$get_datanames())) {
        stop("Some datanames already exists")
      }
      names(connectors) <- connector_names
      private$datasets <- connectors
      return(invisible(NULL))
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
