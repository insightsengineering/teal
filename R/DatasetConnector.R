## DatasetConnector ====
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @title A `DatasetConnector` class of objects
#'
#' Objects of this class store the connection function to fetch a single dataset.
#' Note that for some specific connection types (e.g. \code{RICE} or \code{SAICE}),
#' an object of class \code{DataConnection} must be provided.
#' Data can be pulled via the \code{pull} method and accessed directly
#' through the \code{dataset} active binding.
#' Pulled data inherits from the class \code{\link{Dataset}}
#'
#' @param dataname (`character`)\cr
#'  A given name for the dataset it may not contain spaces
#'
#' @param pull_callable (`CallableFunction`)\cr
#'   function with necessary arguments set to fetch data from connection.
#'
#' @param keys optional, (`character`)\cr
#'  vector of dataset primary keys column names
#'
#' @param label (`character`)\cr
#'  Label to describe the dataset.
#'
#' @param code (`character`)\cr
#'  A character string defining code to modify `raw_data` from this dataset. To modify
#'  current dataset code should contain at least one assignment to object defined in `dataname`
#'  argument. For example if `dataname = ADSL` example code should contain
#'  `ADSL <- <some R code>`. Can't be used simultaneously with `script`
#'
#' @param script (`character`)\cr
#'   Alternatively to `code` - location of the file containing modification code.
#'   Can't be used simultaneously with `script`.
#'
#' @param vars (named `list`)) \cr
#'   In case when this object code depends on other `Dataset` object(s) or
#'   other constant value, this/these object(s) should be included as named
#'   element(s) of the list. For example if this object code needs `ADSL`
#'   object we should specify `vars = list(ADSL = <adsl object>)`.
#'   It's recommended to include `Dataset` or `DatasetConnector` objects to
#'   the `vars` list to preserve reproducibility. Please note that `vars`
#'   are included to this object as local `vars` and they cannot be modified
#'   within another dataset.
#'
DatasetConnector <- R6::R6Class( #nolint

  ## __Public Methods ====
  classname = "DatasetConnector",
  public = list(
    #' @description
    #' Create a new `DatasetConnector` object. Set the pulling function
    #' `CallableFunction` which returns a `data.frame` or `MultiAssayExperiment`,
    #' e.g. by reading from a function or creating it on the fly.
    initialize = function(dataname,
                          pull_callable,
                          keys = character(0),
                          label = character(0),
                          code = character(0),
                          vars = list()) {
      private$set_pull_callable(pull_callable)
      private$set_var_r6(vars)
      private$set_pull_vars(vars)

      private$set_dataname(dataname)
      self$set_dataset_label(label)
      self$set_keys(keys)

      if (!is_empty(code)) {
        # just needs a dummy Dataset object to store mutate code, hence col = 1
        private$dataset <- Dataset$new(dataname = self$get_dataname(), x = data.frame(col = 1))
        private$dataset$mutate(code = code, vars = vars, force_delay = TRUE)
      }

      return(invisible(self))
    },
    #' Prints this DatasetConnector.
    #'
    #' @param ... additional arguments to the printing method
    #' @return invisibly self
    print = function(...) {
      check_ellipsis(...)

      cat(sprintf(
        "A DatasetConnector object, named %s, containing a Dataset object that has %sbeen loaded/pulled\n",
        self$get_dataname(),
        ifelse(self$is_pulled(), "", "not ")
      ))

      invisible(self)
    },

    # ___ getters ====
    #' @description
    #' Get dataname of dataset
    #'
    #' @return dataname of the dataset
    get_dataname = function() {
      return(private$dataname)
    },
    #' @description
    #' Get dataname of dataset
    #'
    #' @return \code{character} dataname of the dataset
    get_datanames = function() {
      return(private$dataname)
    },
    #' @description
    #' Get label of dataset
    #'
    #' @return \code{character} dataset label
    get_dataset_label = function() {
      return(private$dataset_label)
    },
    #' @description
    #' Get primary keys of dataset
    #' @return \code{character} vector with dataset primary keys
    get_keys = function() {
      return(private$keys)
    },
    #' @description
    #' Get `JoinKeys` object with keys used for joining.
    #' @return (`JoinKeys`)
    get_join_keys = function() {
      if (is.null(private$join_keys)) {
        private$join_keys <- join_keys()
      }
      private$join_keys
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
    #' @return `\code{CodeClass}`
    get_code_class = function() {
      code_class <- CodeClass$new()

      pull_code_class <- private$get_pull_code_class()
      code_class$append(pull_code_class)

      if (!is.null(private$dataset)) {
        executed_code_in_dataset <- private$dataset$get_code_class()
        code_class$append(executed_code_in_dataset)
      }

      return(code_class)
    },
    #' @description
    #'
    #' Derive the arguments this connector will pull with
    #' @return \code{list} of pull function fixed arguments
    get_pull_args = function() {
      private$pull_callable$get_args()
    },
    #' @description
    #' Get dataset
    #'
    #' @return dataset (\code{Dataset})
    get_dataset = function() {
      if (!self$is_pulled()) {
        stop(
          sprintf("'%s' has not been pulled yet\n - please use `load_dataset()` first.", self$get_dataname()),
          call. = FALSE
        )
      }
      private$dataset$merge_join_keys(self$get_join_keys())
      private$dataset$get_dataset()
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
    #' @return \code{data.frame} or \code{MultiAssayExperiment} data
    get_raw_data = function() {
      dataset <- self$get_dataset()
      return(dataset$get_raw_data())
    },
    #' @description
    #' Get the list of dependencies that are Dataset or DatasetConnector objects
    #'
    #' @return \code{list}
    get_var_r6 = function() {
      return(private$var_r6)
    },

    # ___ setters ====
    #' @description
    #' Set label of the \code{dataset} object
    #'
    #' @return (`self`) invisibly for chaining
    set_dataset_label = function(label) {
      if (is.null(label)) {
        label <- character(0)
      }
      stopifnot(is_character_vector(label, min_length = 0, max_length = 1))
      private$dataset_label <- label
      if (self$is_pulled()) {
        private$dataset$set_dataset_label(label)
      }
      return(invisible(self))
    },
    #' @description
    #' Set new keys
    #' @return (`self`) invisibly for chaining.
    set_keys = function(keys) {
      stopifnot(is_character_vector(keys, min_length = 0))
      if (isTRUE(self$is_pulled())) {
        set_keys(private$dataset, keys)
      }
      private$keys <- keys
      return(invisible(self))
    },
    #' @description
    #' set join_keys for a given dataset and self
    #' @param x `list` of `JoinKeySet` objects (which are created using the `join_key` function)
    #' or single `JoinKeySet` objects
    #' @return (`self`) invisibly for chaining
    set_join_keys = function(x) {
      self$get_join_keys()$set(x)
      return(invisible(self))
    },
    #' @description
    #' mutate the join_keys for a given dataset and self
    #' @param dataset (`character`) dataset for which join_keys are to be set against self
    #' @param val (named `character`) column names used to join
    #' @return (`self`) invisibly for chaining
    mutate_join_keys = function(dataset, val) {
      self$get_join_keys()$mutate(private$dataname, dataset, val)
      return(invisible(self))
    },

    # ___ pull ====
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
    #' @return (`self`) if successful.
    pull = function(args = NULL, try = FALSE) {
      data <- private$pull_internal(args = args, try = try)
      if (!self$is_failed()) {
        # The first time object is pulled, private$dataset may be NULL if mutate method was never called
        has_dataset <- !is.null(private$dataset)
        if (has_dataset) {
          code_in_dataset <- private$dataset$get_code_class()
          vars_in_dataset <- private$dataset$get_vars()
        }
        private$dataset <- dataset(
          dataname = self$get_dataname(),
          x = data,
          keys = character(0), # keys need to be set after mutate
          label = self$get_dataset_label(),
          code = private$get_pull_code_class()
        )
        if (has_dataset) {
          private$dataset$mutate(
            code = code_in_dataset,
            vars = vars_in_dataset
          )
        }
        set_keys(private$dataset, self$get_keys())
        private$is_pulled_flag <- TRUE
      }

      return(invisible(self))
    },
    #' @description
    #' Set arguments to the pulling function
    #'
    #' @param args (\code{NULL} or named \code{list}) dynamic arguments to function
    #'
    #' @return (`self`) invisibly for chaining
    set_args = function(args) {
      set_args(private$pull_callable, args)
      return(invisible(self))
    },

    # ___ mutate ====
    #' @description
    #' Dispatcher for either eager or delayed mutate methods
    #'
    #' Either code or script must be provided, but not both.
    #'
    #' @return (`self`) invisibly for chaining.
    mutate = function(code, vars = list()) {
      stopifnot(is_fully_named_list(vars))

      if (is.null(private$dataset)) {
        # just needs a dummy Dataset object to store mutate code, hence col = 1
        private$dataset <- Dataset$new(dataname = self$get_dataname(), x = data.frame(col = 1))
      }
      private$dataset$mutate(code = code, vars = vars, force_delay = !self$is_pulled())
      # should be called at the end so that failure in Dataset object will prevent it.
      private$set_var_r6(vars)
      return(invisible(self))
    },

    # ___ status ====
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
      private$is_pulled_flag
    },
    #' @description
    #' Check if dataset has mutations that are delayed
    #'
    #' @return \code{logical}
    is_mutate_delayed = function() {
      if (is.null(private$dataset)) {
        FALSE
      } else {
        private$dataset$is_mutate_delayed()
      }
    },

    # ___ check ====
    #' @description
    #'   Check to determine if the raw data is reproducible from the
    #'   \code{get_code()} code.
    #' @return
    #'   \code{TRUE} always for all connectors to avoid evaluating the same code multiple times.
    check = function() {
      return(TRUE)
    },
    # ___ shiny ====
    #' @description Sets the shiny UI according to the given inputs.
    #' Inputs must provide only scalar (length of 1) variables.
    #' @param inputs (\code{function}) A shiny module UI function with single argument \code{ns}.
    #' This function needs to return a list of shiny inputs with their \code{inputId} wrapped
    #' in function \code{ns}. The \code{inputId} must match exactly the argument name to be set.
    #' See example.
    #' Nested lists are not allowed.
    #' @return (`self`) invisibly for chaining.
    #' @examples
    #' ds <- dataset_connector("xyz", pull_callable = callable_function(data.frame))
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
    #' ds <- dataset_connector("xyz", pull_callable = callable_function(data.frame))
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
          self$get_ui(id = "main_app"),
          shinyjs::useShinyjs(),
          br(),
          actionButton("pull", "Get data"),
          br(),
          tableOutput("result")
        ),
        server = function(input, output, session) {
          session$onSessionEnded(stopApp)
          observeEvent(input$pull, {
            callModule(self$get_server(), id = "main_app")
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
    dataset = NULL, # Dataset
    pull_callable = NULL, # Callable
    pull_vars = list(), # named list
    dataname = character(0),
    dataset_label = character(0),
    keys = NULL,
    var_r6 = list(),
    ui_input = NULL, # NULL or list
    is_pulled_flag = FALSE,
    join_keys = NULL,

    ## __Private Methods ====
    ui = function(id) {
      ns <- NS(id)
      # add namespace to input ids
      ui <- if_not_null(private$ui_input, do.call(private$ui_input, list(ns = ns)))
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
      if_not_null(
        ui,
        tags$div(
          tags$div(
            id = ns("inputs"),
            h4("Dataset Connector for ", code(self$get_dataname())),
            ui
          )
        )
      )
    },
    server = function(input, output, session, data_args = NULL) {
      withProgress(value = 1, message = paste("Pulling", self$get_dataname()), {
        # set args to save them - args set will be returned in the call
        dataset_args <- if_not_null(private$ui_input, reactiveValuesToList(input))
        if (!is_empty(dataset_args)) {
          self$set_args(args = dataset_args)
        }

        self$pull(args = data_args, try = TRUE)

        # print error if any
        # error doesn't break an app
        if (self$is_failed()) {
          shinyjs::alert(
            sprintf(
              "Error pulling %s:\nError message: %s",
              self$get_dataname(),
              self$get_error_message()
            )
          )
        }
      })

      return(invisible(self))
    },

    # need to have a custom deep_clone because one of the key fields are reference-type object
    # in particular: dataset is a R6 object that wouldn't be cloned using default clone(deep = T)
    deep_clone = function(name, value) {
      deep_clone_r6(name, value)
    },

    get_pull_code_class = function(args = NULL) {
      res <- CodeClass$new()
      res$append(list_to_code_class(private$pull_vars))

      code <- if (inherits(private$pull_callable, "CallableCode")) {
        tmp <- private$pull_callable$get_call(deparse = FALSE)
        tmp[[length(tmp)]] <- substitute(a <- b, list(a = as.name(private$dataname), b = tmp[[length(tmp)]]))
        paste0(vapply(tmp, pdeparse, character(1)), collapse = "\n")
      } else {
        pdeparse(substitute(
          a <- b,
          list(a = as.name(private$dataname),
               b = private$pull_callable$get_call(deparse = FALSE, args = args))))
      }

      res$set_code(code = code, dataname = private$dataname, deps = names(private$pull_vars))
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
          # this means that re-assignment is not possible and will be silently skipped
          # During the app loading, assign is called only once.
          private$pull_callable$assign_to_env(
            x = var_name,
            value = if (is(var_value, "DatasetConnector") || is(var_value, "Dataset")) {
              get_raw_data(var_value)
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
            output_message <- paste("Could not pull dataset, the following error message was returned:", e$message)
          }
          stop(output_message, call. = FALSE)
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
    set_var_r6 = function(vars) {
      stopifnot(is_fully_named_list(vars))
      for (var in vars) {
        if (is(var, "DatasetConnector") || is(var, "Dataset")) {
          for (var_dep in c(var, var$get_var_r6())) {
            if (identical(self, var_dep)) {
              stop("Circular dependencies detected")
            }
          }
          private$var_r6 <- c(private$var_r6, var, var$get_var_r6())
        }
      }
      return(invisible(self))
    },
    set_dataname = function(dataname) {
      stopifnot(is_character_single(dataname))
      stopifnot(!grepl("\\s", dataname))
      private$dataname <- dataname
      return(invisible(self))
    },

    set_ui = function(ui_args = NULL) {
      private$ui <- function(id) {
        ns <- NS(id)
        # add namespace to input ids
        ui <- if_not_null(ui_args, do.call(ui_args, list(ns = ns)))
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
        if_not_null(
          ui,
          tags$div(
            tags$div(
              id = ns("inputs"),
              h4("Dataset Connector for ", code(self$get_dataname())),
              ui
            )
          )
        )
      }
      return(invisible(self))
    }
  )
)
