## NamedDatasetConnector ====
#' A \code{NamedDatasetConnector} class of objects
#'
#' Objects of this class store the connection function to fetch a single dataset.
#' Note that for some specific connection types (e.g. \code{RICE} or \code{SAICE}),
#' an object of class \code{DataConnection} must be provided.
#' Data can be pulled via the \code{pull} method and accessed directly
#' through the \code{dataset} active binding.
#' Pulled data inherits from the class \link{NamedDataset}.
#'
#' @importFrom R6 R6Class
#' @importFrom shinyjs alert
NamedDatasetConnector <- R6::R6Class( #nolint

  ## __Public Methods ====
  classname = "NamedDatasetConnector",
  inherit = RawDatasetConnector,
  public = list(
    #' @description
    #' Create a new \code{NamedDatasetConnector} object. Set the pulling function
    #' load the data. \code{dataname} will be used as name
    #' of object to be assigned.
    #'
    #' @param dataname (\code{character})\cr
    #'  A given name for the dataset, it may not contain spaces.
    #'
    #' @param pull_callable (\code{CallableFunction})\cr
    #'  function to load the data, must return a \code{data.frame}.
    #'
    #' @param code (\code{character})\cr
    #'  A character string defining code to modify \code{raw_data} from this dataset. To modify
    #'  current dataset code should contain at least one assignment to object defined in \code{dataname}
    #'  argument. For example if \code{dataname = ADSL} example code should contain
    #'  \code{ADSL <- <some modification code>}.
    #'
    #' @param label (\code{character})\cr
    #'  Label to describe the dataset.
    #'
    #' @param vars (list)\cr
    #'   In case when this object code depends on the \code{raw_data} from the other
    #'   \code{NamedDataset}, \code{NamedDatasetConnector} object(s) or other constant value,
    #'   this/these object(s) should be included.
    #'
    #' @return new \code{NamedDatasetConnector} object
    initialize = function(dataname, pull_callable, code = character(0), label = character(0), vars = list()) {
      super$initialize(pull_callable = pull_callable, vars = vars)

      private$set_dataname(dataname)

      private$mutate_code <- CodeClass$new()
      private$set_mutate_code(code)

      self$set_dataset_label(label)

      return(invisible(self))
    },
    #' @description
    #' Get dataname of dataset
    #'
    #' @return dataname of the dataset
    get_dataname = function() {
      return(private$dataname)
    },
    #' @description
    #' Get dataset
    #'
    #' @return dataset (\code{<...>Dataset})
    get_dataset = function() {
      if (!self$is_pulled()) {
        stop(
          sprintf("'%s' has not been pulled yet\n - please use `load_dataset()` first.", self$get_dataname()),
          call. = FALSE
        )
      }
      return(private$dataset)
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
    #' Set label of the \code{dataset} object
    #'
    #' @param label (\code{character})\cr
    #'  Label to describe the dataset
    #' @return \code{self} invisibly for chaining
    set_dataset_label = function(label) {
      if (is.null(label)) {
        label <- character(0)
      }
      stopifnot(is_character_vector(label, min_length = 0, max_length = 1))
      private$dataset_label <- label
      return(invisible(self))
    },
    #' @description
    #' Get internal \code{CodeClass} object
    #'
    #' @return \code{CodeClass}
    get_code_class = function() {
      code_class <- CodeClass$new()

      pull_code_class <- private$get_pull_code_class()
      code_class$append(pull_code_class)

      mutate_code_class <- private$get_mutate_code_class()
      code_class$append(mutate_code_class)

      return(code_class)
    },

    #' @description
    #' Mutate dataset by code
    #'
    #' Either code or script must be provided, but not both.
    #'
    #' @param code (\code{character}) Code to mutate the dataset. Must contain the
    #'  \code{dataset$dataname}
    #' @param vars (list)\cr
    #'   In case when this object code depends on the \code{raw_data} from the other
    #'   \code{RelationalDataset}, \code{RelationalDatasetConnector} object(s) or other constant value,
    #'   this/these object(s) should be included
    #' @return \code{self} invisibly for chaining.
    mutate = function(code, vars = list()) {
      if (!is.null(private$dataset)) {
        private$dataset <- mutate_dataset(private$dataset, code = code, vars = vars)
      }
      private$set_mutate_vars(vars)
      private$set_mutate_code(code)

      return(invisible(self))
    },


    #' @description
    #' Pull the data
    #'
    #' Read or create the data using \code{pull_callable} specified in the constructor.
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
    #' @return \code{self} invisibly for chaining.
    pull = function(args = NULL, try = FALSE) {
      data <- private$pull_internal(args = args, try = try)

      if (!self$is_failed()) {
        private$dataset <- named_dataset(
          dataname = self$get_dataname(),
          x = data,
          code = private$get_pull_code_class()$get_code(deparse = TRUE),
          label = self$get_dataset_label()
        )

        if (!is_empty(private$get_mutate_code_class()$code)) {
          private$dataset <- mutate_dataset(
            x = self$get_dataset(),
            code = private$get_mutate_code_class()$get_code(deparse = TRUE),
            vars = private$mutate_vars
          )
        }
      }

      return(invisible(self))
    },
    #' @description
    #'
    #' Derive the arguments this connector will pull with
    #' @return \code{list} of pull function fixed arguments
    get_pull_args = function() {
      private$pull_callable$get_args()
    },
    #' @description
    #'   Check to determine if the raw data is reproducible from the
    #'   \code{get_code()} code.
    #' @return
    #'   \code{TRUE} always for all connectors to avoid evaluating the same code multiple times.
    check = function() {
      if (!self$is_pulled()) {
        stop(
          sprintf(
            "Cannot check the raw data of '%s' until it is pulled.",
            self$get_dataname()
          )
        )
      }
      return(TRUE)
    }
  ),

  private = list(
    ## __Private Fields ====
    dataname = character(0),
    dataset_label = character(0),
    mutate_code = NULL, # CodeClass after initialization
    mutate_vars = list(), # named list with vars used to mutate object
    ## __Private Methods ====
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

    get_mutate_code_class = function() {
      res <- CodeClass$new()
      if (inherits(private$mutate_code, "PythonCodeClass")) {
        res <- PythonCodeClass$new()
      }

      res$append(list_to_code_class(private$mutate_vars))
      res$append(private$mutate_code)
      return(res)
    },

    set_mutate_code = function(code) {
      stopifnot(is_character_vector(code, 0, 1) || inherits(code, "PythonCodeClass"))

      if (inherits(code, "PythonCodeClass")) {
        r <- PythonCodeClass$new()
        r$append(private$mutate_code)
        private$mutate_code <- r

        code <- code$get_code()
      }

      if (length(code) > 0 && code != "") {
        private$mutate_code$set_code(
          code = code,
          dataname = private$dataname,
          deps = names(private$mutate_vars)
          )
      }

      return(invisible(self))
    },

    set_mutate_vars = function(vars) {
      stopifnot(is_fully_named_list(vars))

      if (length(vars) > 0) {
        private$mutate_vars <- c(
          private$mutate_vars,
          vars[!names(vars) %in% private$mutate_vars]
        )
      }

      return(invisible(self))
    },

    set_dataname = function(dataname) {
      stopifnot(is_character_single(dataname))
      stopifnot(!grepl("\\s", dataname))
      private$dataname <- dataname
      return(invisible(self))
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
    },
    set_server = function() {
      #set_server function in super class does not have a dataname so override that
      #function here
      private$server <- function(input, output, session, data_args = NULL) {
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

        #
        return(invisible(self))
      }
      return(invisible(self))
    }
  )
)
