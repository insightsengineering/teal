# RelationalDataConnector ------
#' @title Manage multiple and \code{RelationalDatasetConnector} of the same type.
#' @description
#' Class manages \code{RelationalDatasetConnector} to specify additional dynamic arguments and to
#' open/close connection.
#'
#' @examples
#' library(random.cdisc.data)
#' x <- rcd_cdisc_dataset_connector("ADSL", radsl, cached = TRUE)
#' x2 <- rcd_cdisc_dataset_connector("ADLB", radlb, cached = TRUE)
#' x3 <- rcd_cdisc_dataset_connector("ADRS", radrs, cached = TRUE)
#'
#' tc <- teal:::RelationalDataConnector$new()
#' tc$set_connectors(list(x, x2, x3))
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
#' @importFrom R6 R6Class
RelationalDataConnector <- R6::R6Class( #nolint
  classname = "RelationalDataConnector",
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

    #' @description
    #' Function to get \code{RelationalDataset} objects interactively, useful for debugging
    #' @param refresh (\code{logical}) should the data be downloaded?
    #' Defaults to FALSE, which returns last downloaded data.
    #' @param try (\code{logical}) whether perform function evaluation inside \code{try} clause
    #' @param con_args_fixed (\code{NULL} or named \code{list}) fixed arguments to connection function
    #' @param con_args_dynamic (\code{NULL} or named \code{list}) dynamic arguments to connection function
    #'   (not shown in generated code)
    #' @param con_args_replacement (\code{NULL} or named \code{list}) replacement of dynamic argument of connection
    #'   function
    #' @param fun_args_fixed (\code{NULL} or named \code{list}) fixed argument to pull function
    #' @param fun_args_dynamic (\code{NULL} or named \code{list}) dynamic argument to pull function
    #'   (not shown in generated code)
    #' @param fun_args_replacement (\code{NULL} or named \code{list}) replacement of dynamic argument of pull function
    #'
    #' @return list of \code{RelationalDataset} objects
    get_datasets = function(refresh = FALSE,
                            try = FALSE,
                            con_args_fixed = NULL,
                            con_args_dynamic = NULL,
                            con_args_replacement = NULL,
                            fun_args_fixed = NULL,
                            fun_args_dynamic = NULL,
                            fun_args_replacement = NULL) {
      stopifnot(is_logical_single(refresh))
      if (is.null(private$datasets) || refresh) {
        private$refresh_data(
          try = try,
          con_args_fixed = con_args_fixed,
          con_args_dynamic = con_args_dynamic,
          con_args_replacement = con_args_replacement,
          fun_args_fixed = fun_args_fixed,
          fun_args_dynamic = fun_args_dynamic,
          fun_args_replacement = fun_args_replacement
        )
      }
      return(private$datasets)
    },
    #' @description
    #'
    #' Return the \code{datanames} of all connectors
    get_datanames = function() {
      `if`(
        length(private$connectors) > 0,
        vapply(private$connectors, function(c) c$get_dataname(), character(1)),
        NULL
      )
    },
    #' Get connection to data source
    #'
    #' @return connector's connection
    get_connection = function() {
      return(private$connection)
    },
    #' Get connectors
    #'
    #' @return \code{list} with all connectors
    get_connectors = function() {
      return(private$connectors)
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
      id <- private$server_info$submit_id
      if (!is.null(id)) {
        return(id)
      } else {
        return(character(0))
      }
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
      shinyApp(
        ui = fluidPage(private$ui(id = "main_app"),
                       br(),
                       uiOutput("result")),
        server = function(input, output, session) {
          session$onSessionEnded(stopApp)

          dat <- callModule(private$server, id = "main_app")
          output$result <- renderUI({
            if (length(dat()) > 0 &&
                all(vapply(dat(), is, logical(1), "RelationalDataset")) &&
                !any(vapply(dat(), is.null, logical(1)))) {
              return(h3("Data successfully loaded!"))
            }
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

      self$set_server(
        function(input, output, session, return_cdisc_data = TRUE) {
          rv <- reactiveVal(NULL)
          observeEvent(input[[submit_id]], {
            private$refresh_data(
              input = input,
              submit_id = submit_id,
              session = session,
              try = TRUE,
              con_args_fixed = con_args_fixed,
              con_args_dynamic = con_args_dynamic,
              con_args_replacement = con_args_replacement,
              fun_args_fixed = fun_args_fixed,
              fun_args_dynamic = fun_args_dynamic,
              fun_args_replacement = fun_args_replacement
            )

            if (return_cdisc_data) {
              res <- private$datasets
              do.call(what = "cdisc_data", res)
            } else {
              res <- private$datasets
            }

            rv(res)
          })
          return(rv)
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
    #' @param connectors (\code{list} of \code{DatasetConnector} elements) data connectors
    #'
    #' @return nothing
    set_connectors = function(connectors) {
      stopifnot(is_class_list("RelationalDatasetConnector")(connectors))
      private$connectors <- connectors
      return(invisible(NULL))
    }
  ),
  # ..private ------
  private = list(
    # ....fields ----
    server = NULL,
    server_info = NULL,
    ui = NULL,
    connection = NULL,
    connectors = NULL,
    check = FALSE,
    datasets = NULL,
    # ....methods ----

    # adds open/close connection code at beginning/end of the dataset code
    append_connection_code = function(con_args_replacement, fun_args_replacement) {
      lapply(
        private$datasets,
        function(ds) {
          private$stop_on_error(
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
            ),
            submit_id = submit_id,
            progress = progress
          )
        }
      )

    },
    refresh_data = function(input = NULL,
                            submit_id = character(0),
                            session = NULL,
                            try = shiny::isRunning(),
                            con_args_fixed = NULL,
                            con_args_dynamic = NULL,
                            con_args_replacement = NULL,
                            fun_args_fixed = NULL,
                            fun_args_dynamic = NULL,
                            fun_args_replacement = NULL) {

      progress <- NULL
      if (shiny::isRunning()) {
        shinyjs::disable(submit_id)

        progress <- shiny::Progress$new(session)
        progress$set(0.1, message = "Setting up connection ...")
      }

      optional_eval <- function(x, envir = parent.frame(1L)) {
        if (is.null(x)) {
          return(x)
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

      con_args_fixed <- optional_eval(con_args_fixed)
      con_args_dynamic <- optional_eval(con_args_dynamic)
      fun_args_fixed <- optional_eval(fun_args_fixed, parent.frame())
      fun_args_dynamic <- optional_eval(fun_args_dynamic)

      datanames <- vapply(private$connectors, get_dataname, character(1))

      if (!is.null(private$connection)) {
        private$connection$set_open_args(args = con_args_fixed)
        private$stop_on_error(
          private$connection$open(args = con_args_dynamic, try = try),
          submit_id, progress
        )
      }

      env_data <- new.env()
      for (i in seq_along(private$connectors)) {
        if_not_null(progress,
                    progress$set(0.2 + 0.4 * (i - 1) / length(private$connectors),
                                 message = "Loading data ..."))
        set_args(x = private$connectors[[i]],
                 args = fun_args_fixed)

        dataset <- private$stop_on_error(
          get_dataset(
            load_dataset(
              private$connectors[[i]],
              args = fun_args_dynamic,
              try = try
            )
          ),
          submit_id, progress
        )

        assign(
          datanames[[i]],
          dataset,
          envir = env_data
        )
      }

      if_not_null(private$connection, private$connection$close(silent = TRUE))

      if_not_null(progress, progress$set(0.7, message = "Setting relational datasets ..."))

      private$datasets <- lapply(
        datanames,
        function(x) {
          get(x, env_data)
        }
      )
      rm(env_data)

      private$append_connection_code(con_args_replacement, fun_args_replacement)

      if_not_null(progress, progress$set(1, message = "Loading complete!"))

      Sys.sleep(0.5) #just for the progess bar to be shown
      if_not_null(progress, progress$close())

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
