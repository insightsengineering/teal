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

      private$mutate_code <- CodeClass$new()
      private$set_mutate_code(code)
      private$staged_mutate_code <- CodeClass$new()

      return(invisible(self))
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
    #' Get code to get data
    #'
    #' @param deparse (\code{logical})\cr
    #'  whether return deparsed form of a call
    #'
    #' @return optionally deparsed \code{call} object
    get_code = function(deparse = TRUE) {
      stopifnot(is_logical_single(deparse))
      if (self$is_mutate_delayed()) {
        message("There are mutate code that are delayed and not part of this output")
      }
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
      if (private$is_mutated) {
        mutate_code_class <- private$get_mutate_code_class()
        code_class$append(mutate_code_class)
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
      private$mutate_eager(is_re_pull = FALSE)
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
        private$dataset <- dataset(
          dataname = self$get_dataname(),
          x = data,
          keys = character(0), # keys needs to be set after mutate
          label = self$get_dataset_label(),
          code = private$get_pull_code_class()
        )
        pre_mutate_dataset <- get_raw_data(private$dataset)
        private$mutate_eager(is_re_pull = TRUE)
        private$mutate_eager(is_re_pull = FALSE)

        if (identical(pre_mutate_dataset, get_raw_data(private$dataset))) {
          private$is_mutated <- FALSE
        }
        set_keys(private$dataset, self$get_keys())
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
      private$set_staged_mutate_vars(vars)
      private$set_mutate_code(code, staged = TRUE)
      if (self$is_pulled()) {
        private$mutate_eager(is_re_pull = FALSE)
      }  else {
        private$mutate_delayed()
      }
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
      isFALSE(is.null(private$dataset))
    },
    #' @description
    #' Check if dataset has mutations that are delayed
    #'
    #' @return \code{logical}
    is_mutate_delayed = function() {
      private$is_mutate_delayed_flag
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
    mutate_code = NULL, # CodeClass after initialization. used for storing code that has already been executed.
    staged_mutate_code = NULL, # CodeClass after initialization. used for storing code that has not been executed.
    mutate_vars = list(), # named list with vars used to mutate object
    staged_mutate_vars = list(),
    var_r6 = list(),
    ui_input = NULL, # NULL or list
    is_mutate_delayed_flag = FALSE,
    is_mutated = FALSE,

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

    # There are two CodeClass objects that store code used for mutating self, mutate_code and staged_mutate_code.
    # mutate_eager by default (is_re_pull = FALSE) will execute code from staged_mutate_code because code inside of
    # mutate_code has already been executed. However, when the self$pull method is called, the entire dataset is
    # recomputed. This means that all code from `mutate_code` need to be recomputed.
    mutate_eager = function(is_re_pull = FALSE) {
      if (!is_empty(private$get_mutate_code_class(staged = ! is_re_pull)$code)) {
        mutate_code <- private$get_mutate_code_class(staged = ! is_re_pull)$get_code(deparse = TRUE)
        if (inherits(private$get_mutate_code_class(staged = ! is_re_pull), "PythonCodeClass")) {
          mutate_code <- private$get_mutate_code_class(staged = ! is_re_pull)
        }

        private$dataset <- mutate_dataset(
          x = private$dataset,
          code = mutate_code,
          vars = private[[ifelse(is_re_pull, "mutate_vars", "staged_mutate_vars")]]
        )

        private$is_mutate_delayed_flag <- private$dataset$is_mutate_delayed()
        # allowing private$dataset to decide whether the mutate code of self has been delayed or not
        # i.e. if private$dataset is delayed, then self will be delayed,
        # if private$dataset has been mutated then self will be mutated.
        # For example,
        #   private$dataset could be delayed if one of the vars is a DataConnector object that has not been pulled yet.
        if (! private$is_mutate_delayed_flag && !is_re_pull) {
          private$is_mutated <- TRUE
          if (!is_re_pull) {
            private$mutate_code$set_code(
              if (inherits(mutate_code, "PythonCodeClass")) mutate_code$get_code(deparse = TRUE) else mutate_code,
              dataname = private$dataname,
              deps = names(private$staged_mutate_vars)
            )
            private$mutate_vars <- c(
              private$mutate_vars[!names(private$mutate_vars) %in% names(private$staged_mutate_vars)],
              private$staged_mutate_vars
            )
            private$staged_mutate_code <- CodeClass$new()
            private$staged_mutate_vars <- list()
          }
        }
      } else {
        private$is_mutate_delayed_flag <- FALSE
      }
      return(invisible(self))
    },

    mutate_delayed = function() {
      message("Mutation is delayed")
      private$is_mutate_delayed_flag <- TRUE
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

    get_mutate_code_class = function(staged = FALSE) {
      code_obj <- ifelse(staged, "staged_mutate_code", "mutate_code")
      vars_list <- ifelse(staged, "staged_mutate_vars", "mutate_vars")

      res <- CodeClass$new()
      if (inherits(private[[code_obj]], "PythonCodeClass")) {
        res <- PythonCodeClass$new()
      }

      res$append(list_to_code_class(private[[vars_list]]))
      res$append(private[[code_obj]])
      return(res)
    },
    set_mutate_code = function(code, staged = FALSE) {
      stopifnot(is_character_vector(code, 0, 1) || inherits(code, "PythonCodeClass"))
      code_obj <- ifelse(staged, "staged_mutate_code", "mutate_code")
      vars_list <- ifelse(staged, "staged_mutate_vars", "mutate_vars")

      if (inherits(code, "PythonCodeClass")) {
        r <- PythonCodeClass$new()
        r$append(private[[code_obj]])
        private[[code_obj]] <- r

        code <- code$get_code()
      }

      if (length(code) > 0 && code != "") {
        private[[code_obj]]$set_code(
          code = code,
          dataname = private$dataname,
          deps = names(private[[vars_list]])
        )
      }

      return(invisible(self))
    },
    set_staged_mutate_vars = function(vars) {
      stopifnot(is_fully_named_list(vars))
      total_vars <- c(private$staged_mutate_vars, private$mutate_vars)
      private$set_var_r6(vars)
      if (length(vars) > 0) {
        # now allowing overriding variable names
        over_rides <- names(vars)[vapply(
          names(vars), function(var_name) {
            var_name %in% names(total_vars) &&
              !identical(total_vars[[var_name]], vars[[var_name]])
          },
          FUN.VALUE = logical(1)
        )]
        if (length(over_rides) > 0) {
          stop(paste("Variable name(s) already used:", paste(over_rides, collapse = ", ")))
        }
        private$staged_mutate_vars <- c(
          private$staged_mutate_vars[!names(private$staged_mutate_vars) %in% names(vars)],
          vars
        )
      }

      return(invisible(self))
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
