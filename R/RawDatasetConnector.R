## RawDatasetConnector ====
#' A \code{RawDatasetConnector} class of objects
#'
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
    #' @param pull_fun (\code{CallableFunction})\cr
    #'  function to pull the data.
    #' @param vars (list)\cr
    #'   In case when this object code depends on the \code{raw_data} from the other
    #'   \code{RelationalDataset}, \code{RelationalDatasetConnector} object(s) or other constant value,
    #'   this/these object(s) should be included
    #'
    #' @return new \code{RawDatasetConnector} object
    initialize = function(pull_fun, vars = list()) {
      private$set_pull_fun(pull_fun)
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
    #' @return self invisibly for chaining
    set_args = function(args) {
      private$pull_fun$set_args(args)
      return(invisible(self))
    },

    #' @description
    #' Get code to get data
    #'
    #' @param deparse (\code{logical})\cr
    #'  whether return deparsed form of a call
    #' @param args (\code{NULL} or named \code{list})\cr
    #'  dynamic arguments to function which loads data
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
    #' @return \code{try-error} object with error message or \code{character(0)} if last
    #'  pull was successful.
    get_error_message = function() {
      return(private$pull_fun$get_error_message())
    },
    #' @description
    #' Get pull function
    #'
    #' @return \code{CallableFunction}
    get_pull_fun = function() {
      return(private$pull_fun)
    },
    #' @description
    #' Get raw data from dataset
    #'
    #' @return data (data.frame)
    get_raw_data = function() {
      dataset <- self$get_dataset()
      return(dataset$get_raw_data())
    },
    #' @description
    #' Pull the data
    #'
    #' Read or create data using \code{pull_fun} specified in the constructor.
    #'
    #' @param args (\code{NULL} or named \code{list})\cr
    #'  additional dynamic arguments for pull function. \code{args} can be omitted if \code{pull_fun}
    #'  from constructor already contains all necessary arguments to pull data. One can try
    #'  to execute \code{pull_fun} directly by \code{x$pull_fun$run()} or to get code using
    #'  \code{x$pull_fun$get_code()}. \code{args} specified in pull are used temporary to get data but
    #'  not saved in code.
    #' @param try (\code{logical} value)\cr
    #'  whether perform function evaluation inside \code{try} clause
    #'
    #' @return \code{self} if successful or \code{try-error} if not.
    pull = function(args = NULL, try = FALSE) {
      data <- private$pull_internal(args = args, try = try)
      if (!self$is_failed()) {
        private$dataset <- RawDataset$new(data)
      }
      return(invisible(self))
    },
    #' @description
    #' Check if pull has not failed.
    #'
    #' @return \code{TRUE} if pull failed, else \code{FALSE}
    is_failed = function() {
      return(private$pull_fun$is_failed())
    },
    #' @description
    #' Check if dataset has already been pulled.
    #'
    #' @return \code{TRUE} if connector has been already pulled, else \code{FALSE}
    is_pulled = function() {
      isFALSE(is.null(private$dataset))
    },
    #' @description Sets the shiny UI according to the given inputs.
    #' It creates a checkboxInput for logical type, numericInput for numeric type,
    #' and textInput for character type. No other types are allowed.
    #' Inputs provide only scalar (length of 1) variables.
    #' @param inputs (\code{list}) Named list with specification of what inputs
    #' should be provided in the UI.
    #' Elements of the list should be one of \code{c("logical", "numeric", "character")}
    #' with length 1.
    #' Names of the list elements will become the labels of the respective UI widgets.
    #' Nested lists are not allowed.
    #' @return Invisible self for chaining.
    #' @examples
    #' ds <- raw_dataset_connector(pull_fun = callable_function(data.frame))
    #' ds$set_ui_input(list(z = "character", w = "character", xx = 2))
    #' \dontrun{
    #' ds$launch()
    #' }
    set_ui_input = function(inputs = NULL) {
      stopifnot(is.null(inputs) || is.list(inputs))
      if (!is.null(inputs)) {
        stopifnot(is_fully_named_list(inputs))
        stopifnot(all(vapply(inputs, length, integer(1)) == 1)) # do not allow nested lists
        stopifnot(all(vapply(inputs, mode, character(1)) %in% c("logical", "numeric", "character")))
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
    #' ds <- raw_dataset_connector(pull_fun = callable_function(data.frame))
    #' ds$set_ui_input(list(z = "character 1", w = "character 2", xx = 2L))
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
    pull_fun = NULL, # CallableFunction
    pull_vars = list(), # named list
    ui_input = NULL, # NULL or list
    ui = NULL, # NULL or shiny.tag.list
    server = NULL, # NULL or shiny server function

    ## __Private Methods ====
    get_pull_code_class = function(args = NULL) {
      res <- CodeClass$new()
      res$append(list_to_code_class(private$pull_vars))
      res$set_code(
        code = private$pull_fun$get_call(deparse = TRUE, args = args),
        deps = names(private$pull_vars)
      )
      return(res)
    },
    set_pull_fun = function(pull_fun) {
      stopifnot(is(pull_fun, "CallableFunction"))
      private$pull_fun <- pull_fun
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

          private$pull_fun$assign_to_env(
            x = var_name,
            value = if (is(var_value, "RawDatasetConnector") || is(var_value, "RawDataset")) {
              get_raw_data(var_value)
            } else if (is(var_value, "data.frame")) {
              var_value
            } else {
              stop(
                sprintf(
                  "'%1$s' is of class '%2$s'.
                   '%1$s' should be 'RawDatasetConnector', 'RawDataset' or 'data.frame'",
                  var_name,
                  paste(class(var_value), collapse = "/")
                ),
                call. = FALSE
              )
            }
          )
        }
      }

      # eval CallableFunction with dynamic args
      private$pull_fun$run(args = args, try = try)
    },
    set_failure = function(res) {
      if (is(res, "try-error")) {
        private$failed <- TRUE
        private$failure_msg <- res
      } else {
        private$failed <- FALSE
        private$failure_msg <- NULL
      }
      return(NULL)
    },
    set_ui = function(args = NULL) {
      private$ui <- function(id) {
        ns <- NS(id)
        tags$div(
          tags$div(
            id = ns("inputs"),
            if_not_null(args, h4("Dataset Connector:")),
            lapply(seq_along(args),
                   function(i) match_ui(ns = ns, value = args[[i]], label = names(args[i]))
            )
          )
        )

      }
      return(invisible(self))
    },
    set_server = function() {
      private$server <- function(input, output, session, data_args = NULL) {

        withProgress(value = 1, message = "Pulling dataset", {
          # set args to save them - args set will be returned in the call
          dataset_args <- if_not_null(private$ui_input,
                                     reactiveValuesToList(input)[names(private$ui_input)])
          if (!is_empty(dataset_args)) {
            self$set_args(args = dataset_args)
          }

          # print error if any
          out <- self$pull(args = data_args, try = TRUE)
          if (self$is_failed()) {
            shinyjs::alert(paste("Error pulling dataset\nError message: ", self$get_error_message()))
          }
        })
        return(invisible(self))
      }
      return(invisible(self))
    }
  )
)

## Functions ====

# create UI with given ns function and label according to the type of variable
match_ui <- function(ns, value, label) {
  type <- mode(value)

  if (type == "logical") {
    out <- checkboxInput(ns(label), code(label), value)
  } else if (type == "numeric") {
    out <- numericInput(ns(label), code(label), value)
  } else if (type == "character") {
    out <- textInput(ns(label), code(label), value)
  } else {
    stop(paste("Unknown type", type))
  }
  return(out)
}

## Constructors ====

#' Create \code{RawDatasetConnector} object
#'
#' Create \link{RawDatasetConnector} object to execute specific call to fetch data
#' @param pull_fun (\code{CallableFunction})\cr
#'   function with necessary arguments set to fetch data from connection.
#' @examples
#' ds <- raw_dataset_connector(pull_fun = callable_function(data.frame))
#' set_args(ds, list(x = 1:5, y = letters[1:5], stringsAsFactors = FALSE))
#' ds$pull()
#' ds$get_raw_data()
#' ds$get_code()
#' @return \code{RawDatasetConnector} object
#' @export
raw_dataset_connector <- function(pull_fun) {
  stopifnot(is(pull_fun, "CallableFunction"))

  RawDatasetConnector$new(pull_fun = pull_fun)
}
