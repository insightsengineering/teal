## RawDatasetConnector ====
#' A \code{RawDatasetConnector} class of objects
#'
#' @md
#' @description `r lifecycle::badge("experimental")`
#' Objects of this class store the connection function to fetch a single dataset.
#' Note that for some specific connection types (e.g. \code{RICE} or \code{SAICE}),
#' an object of class \code{DataConnection} must be provided.
#' Data can be pulled via the \code{pull} method and accessed directly
#' through the \code{dataset} active binding.
#' Pulled data inherits from the class \link{RelationalDataset}.
#'
#' @importFrom R6 R6Class
#' @importFrom shinyjs alert useShinyjs
RawDatasetConnector <- R6::R6Class( #nolint

  ## __Public Methods ====
  classname = "RawDatasetConnector",
  public = list(
    #' @description
    #' Create a new \code{RawDatasetConnector} object. Set the pulling function
    #' \link{CallableFunction} which returns a \code{data.frame}, e.g. by reading
    #' from a function or creating it on the fly.
    #'
    #' @param pull_callable (\code{CallableFunction})\cr
    #'  function to pull the data.
    #' @param vars (list)\cr
    #'   In case when this object code depends on the \code{raw_data} from the other
    #'   \code{RelationalDataset}, \code{RelationalDatasetConnector} object(s) or other constant value,
    #'   this/these object(s) should be included
    #'
    #' @return new \code{RawDatasetConnector} object
    initialize = function(pull_callable, vars = list()) {
      private$set_pull_callable(pull_callable)
      private$set_pull_vars(vars)

      private$set_ui()
      private$set_server()

      return(invisible(self))
    },
    #' @description
    #' Set arguments to the pulling function
    #'
    #' @param args (\code{NULL} or named \code{list}) dynamic arguments to function
    #'
    #' @return \code{self} invisibly for chaining
    set_args = function(args) {
      private$pull_callable$set_args(args)
      return(invisible(self))
    },

    #' @description
    #' Get code to get data
    #'
    #' @param deparse (\code{logical})\cr
    #'  whether return deparsed form of a call
    #'
    #' @return optionally deparsed \code{call} object
    get_code = function(deparse = TRUE) {
      stopifnot(is_logical_single(deparse))

      return(self$get_code_class()$get_code(deparse = deparse))
    },
    #' @description
    #' Get internal \code{CodeClass} object
    #'
    #' @return \code{CodeClass}
    get_code_class = function() {
      pull_code_class <- private$get_pull_code_class()

      code_class <- CodeClass$new()
      code_class$append(pull_code_class)

      return(code_class)
    },

    #' @description
    #' Get dataset
    #'
    #' @return dataset (\code{RawDataset})
    get_dataset = function() {
      if (!self$is_pulled()) {
        stop("dataset has not been pulled yet\n - please use `load_dataset()` first.",
             call. = FALSE)
      }
      return(private$dataset)
    },
    #' @description
    #' Get error message from last pull
    #'
    #' @return \code{character} object with error message or \code{character(0)} if last
    #'  pull was successful.
    get_error_message = function() {
      return(private$pull_callable$get_error_message())
    },
    #' @description
    #' Get pull function
    #'
    #' @return \code{CallableFunction}
    get_pull_callable = function() {
      return(private$pull_callable)
    },
    #' @description
    #' Get raw data from dataset
    #'
    #' @return \code{data.frame} data
    get_raw_data = function() {
      dataset <- self$get_dataset()
      return(dataset$get_raw_data())
    },
    #' @description
    #' Pull the data
    #'
    #' Read or create data using \code{pull_callable} specified in the constructor.
    #'
    #' @param args (\code{NULL} or named \code{list})\cr
    #'  additional dynamic arguments for pull function. \code{args} can be omitted if \code{pull_callable}
    #'  from constructor already contains all necessary arguments to pull data. One can try
    #'  to execute \code{pull_callable} directly by \code{x$pull_callable$run()} or to get code using
    #'  \code{x$pull_callable$get_code()}. \code{args} specified in pull are used temporary to get data but
    #'  not saved in code.
    #' @param try (\code{logical} value)\cr
    #'  whether perform function evaluation inside \code{try} clause
    #'
    #' @return \code{self} if successful.
    pull = function(args = NULL, try = FALSE) {
      data <- private$pull_internal(args = args, try = try)
      if (!self$is_failed()) {
        private$dataset <- raw_dataset(data)
      }
      return(invisible(self))
    },
    #' @description
    #' Check if pull has not failed.
    #'
    #' @return \code{TRUE} if pull failed, else \code{FALSE}
    is_failed = function() {
      return(private$pull_callable$is_failed())
    },
    #' @description
    #' Check if dataset has already been pulled.
    #'
    #' @return \code{TRUE} if connector has been already pulled, else \code{FALSE}
    is_pulled = function() {
      isFALSE(is.null(private$dataset))
    },
    #' @description Sets the shiny UI according to the given inputs.
    #' Inputs must provide only scalar (length of 1) variables.
    #' @param inputs (\code{function}) A shiny module UI function with single argument \code{ns}.
    #' This function needs to return a list of shiny inputs with their \code{inputId} wrapped
    #' in function \code{ns}, see example.
    #' Nested lists are not allowed.
    #' @return \code{self} invisibly for chaining.
    #' @examples
    #' ds <- raw_dataset_connector(pull_callable = callable_function(data.frame))
    #' ds$set_ui_input(
    #'   function(ns) {
    #'     list(sliderInput(ns("colA"), "Select value for colA", min = 0, max = 10, value = 3),
    #'          sliderInput(ns("colB"), "Select value for colB", min = 0, max = 10, value = 7))
    #'   }
    #' )
    #' \dontrun{
    #' ds$launch()
    #' }
    set_ui_input = function(inputs = NULL) {
      stopifnot(is.null(inputs) || is.function(inputs))
      if (is.function(inputs)) {
        stop_if_not(list(
          length(formals(inputs)) == 1 && names(formals(inputs)) == "ns",
          "'inputs' must be a function of a single argument called 'ns'"
        ))
      }
      private$ui_input <- inputs
      private$set_ui(inputs)
      private$set_server()
      return(invisible(self))
    },
    #' @description Get shiny ui function
    #' @param id (\code{character}) namespace id
    #' @return shiny UI in given namespace id
    get_ui = function(id) {
      stopifnot(is_character_single(id))
      return(if_not_null(private$ui, private$ui(id)))
    },
    #' @description Get shiny server function
    #' @return shiny server function
    get_server = function() {
      return(private$server)
    },
    #' @description Launches a shiny app.
    #' @return Shiny app
    #' @examples
    #' ds <- raw_dataset_connector(pull_callable = callable_function(data.frame))
    #' ds$set_ui_input(
    #'   function(ns) {
    #'     list(sliderInput(ns("colA"), "Select value for colA", min = 0, max = 10, value = 3),
    #'          sliderInput(ns("colB"), "Select value for colB", min = 0, max = 10, value = 7))
    #'   }
    #' )
    #' \dontrun{
    #' ds$launch()
    #' }
    launch = function() {
      if (is.null(private$server)) {
        stop("No arguments set yet. Please use set_ui_input method first.")
      }
      shinyApp(
        ui = fluidPage(
          private$ui(id = "main_app"),
          shinyjs::useShinyjs(),
          br(),
          actionButton("pull", "Get data"),
          br(),
          tableOutput("result")
        ),
        server = function(input, output, session) {
          session$onSessionEnded(stopApp)
          observeEvent(input$pull, {
            callModule(private$server, id = "main_app")
            if (self$is_pulled()) {
              output$result <- renderTable(head(self$get_raw_data()))
            }
          })


        }
      )
    }
  ),
  ## __Private Fields ====
  private = list(
    dataset = NULL, # RawDataset
    pull_callable = NULL, # Callable
    pull_vars = list(), # named list
    ui_input = NULL, # NULL or list
    ui = NULL, # NULL or shiny.tag.list
    server = NULL, # NULL or shiny server function

    ## __Private Methods ====
    get_pull_code_class = function(args = NULL) {
      res <- CodeClass$new()
      res$append(list_to_code_class(private$pull_vars))
      res$set_code(
        code = private$pull_callable$get_call(deparse = TRUE, args = args),
        deps = names(private$pull_vars)
      )
      return(res)
    },
    set_pull_callable = function(pull_callable) {
      stopifnot(is(pull_callable, "Callable"))
      private$pull_callable <- pull_callable
      return(invisible(self))
    },
    set_pull_vars = function(pull_vars) {
      stopifnot(is_fully_named_list(pull_vars))
      private$pull_vars <- pull_vars
      return(invisible(self))
    },
    pull_internal = function(args = NULL, try = FALSE) {
      # include objects CallableFunction environment
      if (!is_empty(private$pull_vars)) {
        for (var_idx in seq_along(private$pull_vars)) {
          var_name <- names(private$pull_vars)[[var_idx]]
          var_value <- private$pull_vars[[var_idx]]

          # assignment is done in pull_callable only once
          # because x is locked within local environment
          # this meas that re-assignment is not possible and will be silently skipped
          # During the app loading, assign is called only once.
          private$pull_callable$assign_to_env(
            x = var_name,
            value = if (is(var_value, "RawDatasetConnector") || is(var_value, "RawDataset")) {
              get_raw_data(var_value)
            } else if (is(var_value, "data.frame")) {
              var_value
            } else {
              var_value
            }
          )
        }
      }

      # eval CallableFunction with dynamic args
      tryCatch({
        private$pull_callable$run(args = args, try = try)
        }, error = function(e) {
          if (grepl("object 'conn' not found", e$message)) {
            output_message <- "This dataset connector requires connection object (conn) to be provided."
          } else {
            output_message <- e$message
          }
          stop(output_message)
        })
    },
    set_failure = function(res) {
      if (is(res, "error")) {
        private$failed <- TRUE
        private$failure_msg <- conditionMessage(res)
      } else {
        private$failed <- FALSE
        private$failure_msg <- NULL
      }
      return(NULL)
    },
    set_ui = function(args = NULL) {
      private$ui <- function(id) {
        ns <- NS(id)
        # add namespace to input ids
        ui <- if_not_null(args, do.call(args, list(ns = ns)))
        # check ui inputs
        if (!is.null(ui)) {
          stopifnot(is.list(ui))
          stop_if_not(
            list(
              all(vapply(ui, is, logical(1), class2 = "shiny.tag")),
              "All elements must be of class shiny.tag"
            )
          )
          stop_if_not(
            list(
              all(
                grepl(
                  "shiny-input-container",
                  vapply(lapply(ui, "[[", i = "attribs"), "[[", character(1), i = "class")
                )
              ),
              "All elements must be shiny inputs"
            )
          )
        }
        # create ui
        tags$div(
          tags$div(
            id = ns("inputs"),
            if_not_null(ui, h4("Dataset Connector:")),
            ui
          )
        )

      }
      return(invisible(self))
    },
    set_server = function() {
      private$server <- function(input, output, session, data_args = NULL) {
        withProgress(value = 1, message = "Pulling dataset", {
          # set args to save them - args set will be returned in the call
          dataset_args <- if_not_null(private$ui_input, reactiveValuesToList(input))
          if (!is_empty(dataset_args)) {
            self$set_args(args = dataset_args)
          }

          # print error if any
          out <- self$pull(args = data_args, try = TRUE)
          if (self$is_failed()) {
            shinyjs::alert(
              paste(
                "Error pulling dataset\nError message: ",
                self$get_error_message()
              )
            )
          }
        })
        return(invisible(self))
      }
      return(invisible(self))
    }
  )
)
