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
#' tc$launch()
#' tc$get_datasets()
#' }
#' @importFrom R6 R6Class
#' @importFrom shinyjs enable
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
      stopifnot(is_class_list("RelationalDatasetConnector")(connectors))

      if (!missing(connection)) {
        stopifnot(is(connection, "DataConnection"))
        private$connection <- connection
      }

      connectors_names <- vapply(connectors, get_dataname, character(1))
      setNames(connectors, connectors_names)

      private$check_names(connectors)

      private$datasets <- connectors

      private$code <- CodeClass$new()

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

      if (is.null(private$code)) {
        sapply(private$datasets, get_dataset, USE.NAMES = TRUE, simplify = FALSE)
      } else {
        # have to evaluate post-processing code (i.e. private$code) before returning dataset
        new_env <- new.env()
        for (dataset in private$datasets) {
          assign(dataset$get_dataname(), get_raw_data(dataset), envir = new_env)
        }
        private$code$eval(envir = new_env)
        lapply(
          private$datasets,
          function(x) {
            x_name <- x$get_dataname()
            relational_dataset(
              dataname = x_name,
              x = get(x_name, new_env),
              keys = x$get_keys(),
              code = x$get_code(),
              label = x$get_dataset_label()
            )
          }
        )
      }
    },
    #' @description
    #' Get \code{RelationalDataset} object.
    #' @param dataname (\code{character} value)\cr
    #'   name of dataset to be returned. If \code{NULL}, all datasets are returned.
    #'
    #' @return \code{RelationalDataset}.
    get_dataset = function(dataname) {
      stopifnot(is_character_single(dataname))
      res <- Filter(function(x) get_dataname(x) == dataname, self$get_datasets())
      return(res[[1]])
    },
    #' Get \code{RelationalDatasetConnector} objects
    #'
    #' @return \code{list} \code{RelationalDatasetConnector(s)} named by dataname.
    get_dataset_connectors = function() {
      private$datasets
    },
    #' @description
    #' Get internal \code{CodeClass} object
    #'
    #' @return \code{CodeClass}
    get_code_class = function() {
      all_code <- CodeClass$new()

      connection_code <- if_not_null(private$connection, private$connection$get_open_call(deparse = TRUE))
      all_code$set_code(connection_code)

      datasets_code_class <- private$get_datasets_code_class()
      all_code$append(datasets_code_class)

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
        stop("all the datasets have already been pulled")
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
    #' @return \code{NULL} or \code{self} if data is loaded.
    set_server = function(data_module) {
      stopifnot(is(data_module, "function"))
      stopifnot(all(c("input", "output", "session") %in% names(formals(data_module))))

      private$server <- function(input, output, session, connection, connectors) {
        rv <- reactiveVal(NULL)
        callModule(data_module,
                   id = "data_input",
                   connection = connection,
                   connectors = connectors)

        if (self$is_pulled()) {
          return(rv(TRUE))
        } else {
          return(rv(FALSE))
        }
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
