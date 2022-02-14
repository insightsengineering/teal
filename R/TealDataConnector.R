# TealDataConnector ------
#'
#'
#' @title Manage multiple and `TealDatasetConnector` of the same type.
#'
#' @description `r lifecycle::badge("stable")`
#' Class manages `TealDatasetConnector` to specify additional dynamic arguments and to
#' open/close connection.
#'
#' @param connection (`TealDataConnection`)\cr
#'   connection to data source
#' @param connectors (`list` of `TealDatasetConnector` elements)\cr
#'   list with dataset connectors
#'
#' @examples
#'
#' library(scda)
#' adsl <- scda_cdisc_dataset_connector(dataname = "ADSL", "adsl")
#' adlb <- scda_cdisc_dataset_connector(dataname = "ADLB", "adlb")
#'
#' open_fun <- callable_function(library)
#' open_fun$set_args(list(package = "scda"))
#'
#' con <- data_connection(open_fun = open_fun)
#' con$set_open_server(
#'   function(id, connection) {
#'     moduleServer(
#'       id = id,
#'       module = function(input, output, session) {
#'         connection$open(try = TRUE)
#'         return(invisible(connection))
#'       }
#'     )
#'   }
#' )
#'
#' x <- teal:::TealDataConnector$new(connection = con, connectors = list(adsl, adlb))
#'
#' x$set_ui(
#'   function(id, connection, connectors) {
#'     ns <- NS(id)
#'     tagList(
#'       connection$get_open_ui(ns("open_connection")),
#'       textInput(ns("name"), p("Choose", code("scda data version")), value = "latest"),
#'       do.call(
#'         what = "tagList",
#'         args = lapply(
#'           connectors,
#'           function(connector) {
#'             div(
#'               connector$get_ui(
#'                 id = ns(connector$get_dataname())
#'               ),
#'               br()
#'             )
#'           }
#'         )
#'       )
#'     )
#'   }
#' )
#'
#' x$set_server(
#'   function(id, connection, connectors) {
#'     moduleServer(
#'       id = id,
#'       module = function(input, output, session) {
#'         # opens connection
#'         connection$get_open_server()(id = "open_connection", connection = connection)
#'         if (connection$is_opened()) {
#'           for (connector in connectors) {
#'             set_args(connector, args = list(name = input$name))
#'             # pull each dataset
#'             connector$get_server()(id = connector$get_dataname())
#'             if (connector$is_failed()) {
#'               break
#'             }
#'           }
#'         }
#'       }
#'     )
#'   }
#' )
#' \dontrun{
#' x$launch()
#' x$get_datasets()
#' }
TealDataConnector <- R6::R6Class( # nolint
  classname = "TealDataConnector",
  inherit = TealDataAbstract,

  ## __Public Methods ====
  public = list(
    #' @description
    #' Create a new `TealDataConnector` object
    initialize = function(connection, connectors) {
      checkmate::assert_list(connectors, types = "TealDatasetConnector", min.len = 1)

      connectors_names <- vapply(connectors, get_dataname, character(1))
      connectors <- setNames(connectors, connectors_names)

      private$check_names(connectors_names)

      if (!missing(connection)) {
        stopifnot(is(connection, "TealDataConnection"))
        private$connection <- connection
      }

      private$datasets <- connectors

      private$pull_code <- CodeClass$new()
      private$mutate_code <- CodeClass$new()

      self$id <- sample.int(1e11, 1, useHash = TRUE)


      logger::log_trace(
        "TealDataConnector initialized with data: { paste(self$get_datanames(), collapse = ' ') }."
      )
      return(invisible(self))
    },
    #' @description
    #' Prints this `TealDataConnector`.
    #'
    #' @param ... additional arguments to the printing method
    #' @return invisibly self
    print = function(...) {
      check_ellipsis(...)

      cat(sprintf(
        "A currently %s %s object containing %d TealDataset/TealDatasetConnector object(s) as element(s).\n",
        ifelse(self$get_connection()$is_opened(), "opened", "not yet opened"),
        class(self)[1],
        length(private$datasets)
      ))
      cat(sprintf(
        "%d of which is/are loaded/pulled:\n",
        sum(vapply(private$datasets, function(x) x$is_pulled(), FUN.VALUE = logical(1)))
      ))

      for (i in seq_along(private$datasets)) {
        cat(sprintf("--> Element %d:\n", i))
        print(private$datasets[[i]])
      }

      invisible(self)
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
    #' Get internal `CodeClass` object
    #'
    #' @return `CodeClass`
    get_code_class = function() {
      all_code <- CodeClass$new()

      open_connection_code <- if (!is.null(private$connection)) {
        private$connection$get_open_call(deparse = TRUE)
      } else {
        NULL
      }

      if (!is.null(open_connection_code)) {
        all_code$set_code(open_connection_code, dataname = "*open")
      }
      datasets_code_class <- private$get_datasets_code_class()
      all_code$append(datasets_code_class)

      close_connection_code <- if (!is.null(private$connection)) {
        private$connection$get_close_call(deparse = TRUE, silent = TRUE)
      } else {
        NULL
      }

      if (!is.null(close_connection_code)) {
        all_code$set_code(close_connection_code, dataname = "*close")
      }

      mutate_code_class <- private$get_mutate_code_class()
      all_code$append(mutate_code_class)

      return(all_code)
    },
    #' @description
    #'
    #' @return the `server` function
    get_server = function() {
      if (is.null(private$server)) {
        stop("No server function set yet. Please use set_server method first.")
      }
      function(id, connection = private$connection, connectors = private$datasets) {
        rv <- reactiveVal(NULL)
        moduleServer(
          id = id,
          module = function(input, output, session) {
            private$server(id = "data_input", connection = connection, connectors = connectors)
          }
        )

        if (self$is_pulled()) {
          return(rv(TRUE))
        } else {
          return(rv(FALSE))
        }
      }
    },
    #' @description
    #'
    #' @return the `server` function
    get_preopen_server = function() {
      function(id, connection = private$connection) {
        if (!is.null(private$preopen_server)) {
          moduleServer(
            id = id,
            module = function(input, output, session) {
              private$preopen_server(id = "data_input", connection = connection)
            }
          )
        }
      }
    },
    #' @description
    #' Get Shiny module with inputs for all `TealDatasetConnector` objects
    #'
    #' @param id `character` shiny element id
    #'
    #' @return the `ui` function
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
    #' Set argument to the `pull_fun`
    #'
    #' @param args (named `list`)\cr
    #'  arguments values as separate list elements named by argument name. These arguments
    #'  are passed to each dataset.
    #'
    #' @return nothing
    set_pull_args = function(args) {
      lapply(private$datasets, function(x) set_args(x, args))
      logger::log_trace("TealDataConnector$set_pull_args pull args set.")
      return(invisible(NULL))
    },
    #' @description
    #' Set connector UI function
    #'
    #' @param f (`function`)\cr
    #'  shiny module as function. Inputs specified in this `ui` are passed to server module
    #'  defined by `set_server` method.
    #'
    #' @return nothing
    set_ui = function(f) {
      stopifnot(is(f, "function"))
      stopifnot("id" %in% names(formals(f)))
      stopifnot(all(c("connection", "connectors") %in% names(formals(f))) || "..." %in% names(formals(f)))
      private$ui <- f
      logger::log_trace("TealDataConnector$set_ui ui set.")
      return(invisible(NULL))
    },
    #' @description
    #' Set connector server function
    #'
    #' This function will be called after submit button will be hit. There is no possibility to
    #' specify some dynamic `ui` as `server` function is executed after hitting submit
    #' button.
    #'
    #' @param f (`function`)\cr
    #'  A shiny module server function that should load data from all connectors
    #'
    #' @return nothing
    set_server = function(f) {
      stopifnot(is(f, "function"))
      stopifnot(all(c("id", "connection", "connectors") %in% names(formals(f))))
      private$server <- f
      logger::log_trace("TealDataConnector$set_server server set.")
      return(invisible(NULL))
    },
    #' @description
    #' Set connector pre-open server function
    #'
    #' This function will be called before submit button will be hit.
    #'
    #' @param f (`function`)\cr
    #'  A shiny module server function
    #'
    #' @return nothing
    set_preopen_server = function(f) {
      stopifnot(is(f, "function"))
      stopifnot(all(c("id", "connection") %in% names(formals(f))))
      private$preopen_server <- f
      logger::log_trace("TealDataConnector$set_preopen_server preopen_server set.")
      return(invisible(NULL))
    },

    # ___ pull ====
    #' @description
    #' Load data from each `TealDatasetConnector`
    #'
    #' @param con_args (`NULL` or named `list`)\cr
    #'   additional dynamic arguments for connection function. `args` will be passed to each
    #'  `TealDatasetConnector` object to evaluate `CallableFunction` assigned to
    #'  this dataset. If `args` is null than default set of arguments will be used, otherwise
    #'  call will be executed on these arguments only (arguments set before will be ignored).
    #'  `pull` function doesn't update reproducible call, it's just evaluate function.
    #'
    #' @param args (`NULL` or named `list`)\cr
    #'  additional dynamic arguments to pull dataset. `args` will be passed to each
    #'  `TealDatasetConnector` object to evaluate `CallableFunction` assigned to
    #'  this dataset. If `args` is null than default set of arguments will be used, otherwise
    #'  call will be executed on these arguments only (arguments set before will be ignored).
    #'  `pull` function doesn't update reproducible call, it's just evaluate function.
    #'
    #' @param try (`logical` value)\cr
    #'  whether perform function evaluation inside `try` clause
    #'
    #' @return (`self`) invisibly for chaining. In order to get the data please use `get_datasets` method.
    pull = function(con_args = NULL, args = NULL, try = TRUE) {
      logger::log_trace("TealDataConnector$pull pulling data...")
      # open connection
      if (!is.null(private$connection)) {
        private$connection$open(args = con_args, try = try)

        conn <- private$connection$get_conn()
        for (connector in private$datasets) {
          connector$get_pull_callable()$assign_to_env("conn", conn)
        }
      }

      # load datasets
      for (dataset in private$datasets) {
        load_dataset(dataset, args = args)
      }

      # close connection
      if (!is.null(private$connection)) private$connection$close(silent = TRUE)

      logger::log_trace("TealDataConnector$pull data pulled.")

      return(invisible(self))
    },
    #' @description
    #' Run simple application that uses its `ui` and `server` fields to pull data from
    #' connection.
    #'
    #' Useful for debugging
    #'
    #' @return An object that represents the app
    launch = function() {
      # load TealDatasetConnector objects
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
          self$get_preopen_server()(
            id = "data_connector",
            connection = private$connection
          )
          observeEvent(input$submit, {
            rv <- reactiveVal(NULL)
            rv(
              self$get_server()(
                id = "data_connector",
                connection = private$connection,
                connectors = private$datasets
              )
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

    # ___ mutate ====
    #' @description
    #' Mutate data by code.
    #'
    #' @param ... parameters inherited from `TealDataAbstract`.
    #'
    #' @return Informational message to not use mutate_data() with `TealDataConnectors`.
    mutate = function(...) {
      stop("TealDataConnectors do not support mutate_data().
      Please use mutate_data() with teal_data() or cdisc_data()")
    },

    # ___ status ====
    #' @description
    #' Check if pull or connection has not failed.
    #'
    #' @return `TRUE` if pull or connection failed, else `FALSE`
    is_failed = function() {
      private$connection$is_failed() ||
        any(vapply(private$datasets, function(x) x$is_failed(), logical(1)))
    }
  ),
  ## __Private Fields ====
  private = list(
    server = NULL, # shiny server function
    preopen_server = NULL, # shiny server function
    ui = NULL, # shiny ui function
    connection = NULL, # TealDataConnection

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
                if (!is.null(private$connection)) private$connection$get_open_call(deparse = TRUE),
                get_code(dataset, deparse = TRUE, FUN.VALUE = character(1)),
                if (!is.null(private$connection)) private$connection$get_close_call(deparse = TRUE, silent = TRUE)
              ),
              collapse = "\n"
            ))
          )
        }
      )
    }
  )
)

#' The constructor for `TealDataConnector` class.
#'
#' @description `r lifecycle::badge("stable")`
#' @param connection (`TealDataConnection`)\cr
#'   connection to data source
#' @param connectors (`list` of `TealDatasetConnector` elements)\cr
#'   list with dataset connectors
#'
#' @examples
#'
#' library(scda)
#' adsl <- scda_cdisc_dataset_connector(dataname = "ADSL", "adsl")
#' adlb <- scda_cdisc_dataset_connector(dataname = "ADLB", "adlb")
#'
#' open_fun <- callable_function(library)
#' open_fun$set_args(list(package = "scda"))
#'
#' con <- data_connection(open_fun = open_fun)
#' con$set_open_server(
#'   function(id, connection) {
#'     moduleServer(
#'       id = id,
#'       module = function(input, output, session) {
#'         connection$open(try = TRUE)
#'         return(invisible(connection))
#'       }
#'     )
#'   }
#' )
#'
#' x <- relational_data_connector(connection = con, connectors = list(adsl, adlb))
#'
#' x$set_ui(
#'   function(id, connection, connectors) {
#'     ns <- NS(id)
#'     tagList(
#'       connection$get_open_ui(ns("open_connection")),
#'       textInput(ns("name"), p("Choose", code("scda data version")), value = "latest"),
#'       do.call(
#'         what = "tagList",
#'         args = lapply(
#'           connectors,
#'           function(connector) {
#'             div(
#'               connector$get_ui(
#'                 id = ns(connector$get_dataname())
#'               ),
#'               br()
#'             )
#'           }
#'         )
#'       )
#'     )
#'   }
#' )
#'
#' x$set_server(
#'   function(id, connection, connectors) {
#'     moduleServer(
#'       id = id,
#'       module = function(input, output, session) {
#'         # opens connection
#'         connection$get_open_server()(id = "open_connection", connection = connection)
#'         if (connection$is_opened()) {
#'           for (connector in connectors) {
#'             set_args(connector, args = list(name = input$name))
#'             # pull each dataset
#'             connector$get_server()(id = connector$get_dataname())
#'             if (connector$is_failed()) {
#'               break
#'             }
#'           }
#'         }
#'       }
#'     )
#'   }
#' )
#' \dontrun{
#' x$launch()
#' x$get_datasets()
#' }
#'
#' @return `TealDataConnector` object
#' @export
relational_data_connector <- function(connection, connectors) {
  stopifnot(is(connection, "TealDataConnection"))
  checkmate::assert_list(connectors, types = "TealDatasetConnector", min.len = 1)
  TealDataConnector$new(connection, connectors)
}
