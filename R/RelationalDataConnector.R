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
#' x <- rcd_cdisc_dataset_connector("ADSL", radsl, cached = TRUE)
#' x2 <- rcd_cdisc_dataset_connector("ADLB", radlb, cached = TRUE)
#' x3 <- rcd_cdisc_dataset_connector("ADRS", radrs, cached = TRUE)
#'
#' tc <- teal:::RelationalDataConnector$new()
#' tc$set_dataset_connectors(list(x, x2, x3))
#' tc$set_ui(
#' function(id) {
#'   ns <- NS(id)
#'   tagList(
#'     numericInput(ns("seed"), "Choose seed", min = 1, max = 1000, value = 1),
#'     actionButton(ns("submit"), "Submit")
#'   )
#' }
#' )
#' tc$set_server_helper(
#'   submit_id = "submit",
#'   fun_args_fixed = list(seed = quote(input$seed))
#' )
#' tc$set_server_info(
#'   submit_id = "submit",
#'   fun_args_fixed = list(seed = quote(input$seed))
#' )
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
    #' @param connectors (\code{list} of \code{DatasetConnector} elements) list with dataset connectors
    #'
    #' @return new \code{DataConnector} object
    initialize = function() {
      self$set_ui(
        function(id) {
          tags$span("empty page")
        }
      )

      self$set_server_helper()

      invisible(self)
    },
    #' Get names of the datasets.
    #'
    #' @return \code{character} vector with names of all datasets.
    get_datanames = function() {
      c(
        vapply(private$datasets, get_dataname, character(1)),
        vapply(private$dataset_connectors, get_dataname, character(1))
      )

    },
    #' @description
    #'
    #' Derive the code for all datasets
    #' @return \code{list} of \code{character} containing code
    get_code = function() {
      c(
        vapply(private$datasets, get_code, character(1)),
        vapply(private$dataset_connectors, get_code, character(1))
      )
    },
    #' @description
    #' Get connection to data source
    #'
    #' @return connector's connection
    get_connection = function() {
      return(private$connection)
    },
    #' @description
    #' Get connectors
    #'
    #' @return \code{list} with all connectors
    get_dataset_connectors = function() {
      return(private$dataset_connectors)
    },
    #' @description
    #'
    #' @return the \code{server} function of the \code{RelationalDataConnector}
    get_server = function() {
      return(private$server)
    },
    #' @description
    #'
    #' Derive the ID of the button that triggers the
    #' \code{DataConnection} to be pulled
    #'
    #' @return
    #' A character string of the \code{id} of the
    #' \code{actionButton}.
    get_submit_id = function() {
      return(if_null(private$server_info$submit_id, character(0)))
    },
    #' @description
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
      if (is.null(private$dataset_connectors)) {
        stop("the data has no dataset connectors yet")
      }
      if (all(vapply(private$dataset_connectors, function(el) el$is_pulled(), logical(1L)))) {
        stop("all the datasets has been already pulled")
      }

      shinyApp(
        ui = fluidPage(private$ui(id = "main_app"),
                       br(),
                       uiOutput("result")),
        server = function(input, output, session) {
          session$onSessionEnded(stopApp)
          observeEvent(input[[submit_id]], {
            dat <- callModule(private$server, id = "main_app")
          })
        }
      )
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
    #' @param ui (\code{function}) ui function of a shiny module
    #'
    #' @return nothing
    set_ui = function(ui) {
      stopifnot(is(ui, "function"))
      stopifnot(names(formals(ui)) == "id")
      private$ui <- ui
      return(invisible(NULL))
    },
    #' @description
    #' Set connector server function
    #'
    #' Please also consider using \code{set_server_helper} method
    #'
    #' @param server (\code{function}) A shiny module server function
    #'   that should return reactive value of type \code{cdisc_data}
    #'
    #' @return nothing
    set_server = function(server) {
      stopifnot(is(server, "function"))
      stopifnot(all(c("input", "output", "session") %in% names(formals(server))))
      private$server <- server
      return(invisible(NULL))
    },
    #' @description
    #' Helper function to set connector server function
    #'
    #' @param submit_id (\code{character}) id of the submit button
    #' @param con_args_fixed (\code{NULL} or named \code{list}) fixed argument to connection function
    #' @param con_args_dynamic (\code{NULL} or named \code{list}) dynamic argument to connection function
    #'   (not shown in generated code)
    #' @param con_args_replacement (\code{NULL} or named \code{list}) replacement of dynamic argument of connection
    #'   function
    #' @param fun_args_fixed (\code{NULL} or named \code{list}) fixed argument to pull function
    #' @param fun_args_dynamic (\code{NULL} or named \code{list}) dynamic argument to pull function
    #'   (not shown in generated code)
    #' @param fun_args_replacement (\code{NULL} or named \code{list}) replacement of dynamic argument of pull function
    #'
    #' @return nothing
    set_server_helper = function(submit_id = "submit",
                                 con_args_fixed = NULL,
                                 con_args_dynamic = NULL,
                                 con_args_replacement = NULL,
                                 fun_args_fixed = NULL,
                                 fun_args_dynamic = NULL,
                                 fun_args_replacement = NULL
    ) {
      stopifnot(is_character_single(submit_id))
      stopifnot(is.null(con_args_fixed) || is.list(con_args_fixed))
      stopifnot(is.null(con_args_dynamic) || is.list(con_args_dynamic))
      stopifnot(is.null(con_args_replacement) ||
                  (is.list(con_args_replacement) && identical(names(con_args_dynamic), names(con_args_replacement))))
      stopifnot(is.null(fun_args_fixed) || is.list(fun_args_fixed))
      stopifnot(is.null(fun_args_dynamic) || is.list(fun_args_dynamic))
      stopifnot(is.null(fun_args_replacement) ||
                  (is.list(fun_args_replacement) && identical(names(fun_args_dynamic), names(fun_args_replacement))))


      private$con_args_fixed <- con_args_fixed
      private$con_args_dynamic <- con_args_dynamic
      private$con_args_replacement <- con_args_replacement
      private$fun_args_fixed <- fun_args_fixed
      private$fun_args_dynamic <- fun_args_dynamic
      private$fun_args_replacement <- fun_args_replacement

      self$set_server(
        function(input, output, session, return_cdisc_data = TRUE) {

          # will be replaced by call module of each dataset
          private$pull(try = TRUE)

          res <- if (return_cdisc_data) {
            self$get_cdisc_data()
          } else {
            private$datasets
          }

          return(res)
        }
      )

      return(invisible(NULL))
    },
    #' @description
    #'
    #' Store the call of \code{set_server}
    #'
    #' @param ... All arguments used inside set_server
    #'
    set_server_info = function(...) {
      private$server_info <- list(...)
      return(invisible(NULL))
    },
    #' Set data connection
    #'
    #' @param connection (\code{DataConnection}) data connection
    #'
    #' @return nothing
    set_connection = function(connection) {
      stopifnot(is(connection, "DataConnection"))
      private$connection <- connection
      return(invisible(NULL))
    },
    #' Set dataset connectors
    #'
    #' @param connectors (\code{list} of \code{RelationalDatasetConnector} elements) data connectors
    #'
    #' @return nothing
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
      private$dataset_connectors <- connectors
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
    server = NULL,
    server_info = NULL,
    ui = NULL,
    connection = NULL,
    dataset_connectors = NULL,
    check = FALSE,
    datasets = NULL,
    # args helper
    con_args_fixed = NULL,
    con_args_dynamic = NULL,
    con_args_replacement = NULL,
    fun_args_fixed = NULL,
    fun_args_dynamic = NULL,
    fun_args_replacement = NULL,


    # ....methods ----

    # adds open/close connection code at beginning/end of the dataset code
    append_connection_code = function(con_args_replacement, fun_args_replacement) {
      lapply(
        private$datasets,
        function(ds) {

          try(
            ds$set_code(code = paste(
              c(
                if_not_null(private$connection,
                            private$connection$get_open_call(deparse = TRUE, args = con_args_replacement)),
                get_code(ds, deparse = TRUE, args = fun_args_replacement, FUN.VALUE = character(1)),
                if_not_null(private$connection,
                            private$connection$get_close_call(deparse = TRUE, silent = TRUE))
              ),
              collapse = "\n"
            ))
          )
        }
      )

    },
    pull = function(try = shiny::isRunning()) {
      optional_eval <- function(x, envir = parent.frame(1L)) {
        if (is.null(x)) {
          return(NULL)
        } else {
          lapply(
            x,
            function(el) {
              if (is.call(el)) {
                eval(el, envir = envir)
              } else {
                el
              }
            }
          )
        }
      }

      con_args_fixed <- optional_eval(private$con_args_fixed)
      con_args_dynamic <- optional_eval(private$con_args_dynamic)
      fun_args_fixed <- optional_eval(private$fun_args_fixed, parent.frame())
      fun_args_dynamic <- optional_eval(private$fun_args_dynamic)

      datanames <- vapply(private$dataset_connectors, get_dataname, character(1))

      if (!is.null(private$connection)) {
        private$connection$set_open_args(args = con_args_fixed)
        private$connection$open(args = con_args_dynamic, try = try)
      }

      datasets <- list()
      datanames <- self$get_datanames()
      for (i in seq_along(private$dataset_connectors)) {
        if (!is_empty(fun_args_fixed)) {
          set_args(x = private$dataset_connectors[[i]], args = fun_args_fixed)
        }

        dataset <- get_dataset(
          load_dataset(
            private$dataset_connectors[[i]],
            args = fun_args_dynamic,
            try = try
          )
        )
        datasets[[datanames[i]]] <- dataset
      }

      private$dataset_connectors <- NULL

      if_not_null(private$connection, private$connection$close(silent = TRUE))

      private$datasets <- datasets
      rm(datasets)

      private$append_connection_code(private$con_args_replacement,
                                     private$fun_args_replacement)

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
