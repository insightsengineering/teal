#' A \code{NamedDatasetConnector} class of objects
#'
#' Objects of this class store the connection function to fetch a single dataset.
#' Note that for some specific connection types (e.g. \code{RICE} or \code{SAICE}),
#' an object of class \code{DataConnection} must be provided.
#' Data can be pulled via the \code{pull} method and accessed directly
#' through the \code{dataset} active binding.
#' Pulled data inherits from the class \link{NamedDataset}.
#'
#' @name NamedDatasetConnector
NamedDatasetConnector <- R6::R6Class( #nolint
  # NamedDatasetConnector public ----
  classname = "NamedDatasetConnector",
  inherit = RawDatasetConnector,
  public = list(
    #' @description
    #' Create a new \code{NamedDatasetConnector} object. Set the pulling function
    #' load the data. \code{dataname} will be used as name
    #' of object to be assigned.
    #'
    #' @param pull_fun (\code{CallableFunction})\cr
    #'  function to load the data, must return a \code{data.frame}.
    #' @param dataname (\code{character})\cr
    #'  A given name for the dataset, it may not contain spaces
    #' @param code (\code{character})\cr
    #'  A character string defining the code needed to produce the data set in \code{x}
    #' @param label (\code{character})\cr
    #'  Label to describe the dataset
    #' @param vars (list)\cr
    #'   In case when this object code depends on the \code{raw_data} from the other
    #'   \code{NamedDataset}, \code{NamedDatasetConnector} object(s) or other constant value,
    #'   this/these object(s) should be included
    #'
    #' @return new \code{NamedDatasetConnector} object
    initialize = function(pull_fun, dataname, code = character(0), label = character(0), vars = list()) {
      super$initialize(pull_fun = pull_fun, vars = vars)
      private$set_dataname(dataname)
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
    #' Get label of dataset
    #'
    #' @return \code{character}
    get_dataset_label = function() {
      return(private$dataset_label)
    },
    #' @description
    #' Set label of the \code{dataset} object
    #'
    #' @param label (\code{character})\cr
    #'  Label to describe the dataset
    set_dataset_label = function(label) {
      stopifnot(is_character_vector(label, 0, 1))
      private$dataset_label <- label
      return(invisible(NULL))
    },
    #' @description
    #' Get code to get data
    #'
    #' @param deparse (\code{logical})\cr
    #'  whether to return the deparsed form of a call
    #'
    #' @param args (empty or named \code{list})\cr
    #'  dynamic arguments to function which loads data
    #'
    #' @return optionally deparsed \code{call} object
    get_code = function(deparse = TRUE, args = NULL) {
      stopifnot(is_logical_single(deparse))

      pull_vars_code <- private$get_pull_vars_code(deparse = deparse)
      pull_code <- private$get_pull_code(deparse = deparse, args = args)
      mutate_code <- private$get_mutate_code(deparse = deparse)

      code <- c(pull_vars_code, pull_code, mutate_code)

      if (isTRUE(deparse)) {
        code <- paste0(code, collapse = "\n")
      }

      return(code)
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
    mutate_dataset = function(code, vars = list()) {
      if (!is.null(private$dataset)) {
        private$dataset <- mutate_dataset(private$dataset, code = code, vars = vars)
      }
      private$set_mutate_code(code)
      private$set_mutate_vars(vars)

      return(invisible(self))
    },


    #' @description
    #' Pull the data
    #'
    #' Read or create the data using \code{pull_fun} specified in the constructor.
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
    #' @return \code{NULL} if successful or \code{try-error} if not.
    pull = function(args = NULL, try = FALSE) {
      data <- private$pull_internal(args = args, try = try)

      if (try && is(data, "try-error")) {
        return(data)
      }
      private$dataset <- NamedDataset$new(
        x = data,
        dataname = self$get_dataname(),
        code = private$get_pull_code(deparse = TRUE),
        label = self$get_dataset_label()
      )

      if (!is_empty(private$get_mutate_code())) {
        private$dataset <- mutate_dataset(
          private$dataset,
          code = private$get_mutate_code(deparse = TRUE),
          vars = private$mutate_vars
        )
      }

      return(invisible(self))
    },
    #' @description
    #'
    #' Derive the arguments this connector will pull with
    get_pull_args = function() {
      private$pull_fun$get_args()
    },
    #' @description
    #'   Check to determine if the raw data is reproducible from the
    #'   \code{get_code()} code.
    #' @return
    #'   \code{TRUE} if the dataset generated from evaluating the
    #'   \code{get_code()} code is identical to the raw data, else \code{FALSE}.
    check = function() {

      if (!is_character_single(self$get_code()) || !grepl("\\w+", self$get_code())) {
        stop("Cannot check preprocessing code - code is empty.")
      } else if (!super$is_pulled()) {
        stop("Cannot check the raw data until it is pulled.")
      }

      code <- self$get_code()

      new_env <- new.env(parent = parent.env(.GlobalEnv))
      tryCatch({
        eval(parse(text = code), new_env)
      }, error = function(e) {
        error_dialog(e)
      })

      res_check <- tryCatch({
        identical(super$get_raw_data(), get(self$get_dataname(), envir = new_env))
      }, error = function(e) {
        FALSE
      })

      return(res_check)
    }
  ),

  # NamedDatasetConnector private ----
  private = list(
    dataname = character(0),
    dataset_label = character(0),
    mutate_code = NULL, # list of calls
    mutate_vars = list(), # named list with vars used to mutate object
    # assigns the pull code call to the dataname
    get_pull_code = function(deparse = TRUE, args = NULL) {
      code <- if (deparse) {
        sprintf("%s <- %s",
                private$dataname,
                super$get_pull_code(deparse = deparse, args = args))
      } else {
        substitute(
          a <- b,
          list(a = as.name(private$dataname),
               b = super$get_pull_code(deparse, args))
        )
      }

      return(code)
    },

    # formats the code if necessary
    get_mutate_code = function(deparse = TRUE) {

      code <- if (deparse) {
        if (length(private$mutate_code) > 0) {
          paste0(
            vapply(
              private$mutate_code,
              FUN = deparse,
              FUN.VALUE = character(1)
            ),
            collapse = "\n"
          )
        } else {
          character(0)
        }
      } else {
        private$mutate_code
      }

      return(code)
    },

    set_mutate_code = function(code) {
      stopifnot(is_character_vector(code, 0, 1))

      if (length(code) > 0 && code != "") {
        private$mutate_code <- c(private$mutate_code, as.list(as.call(parse(text = code))))
      }

      return(invisible(NULL))
    },

    set_mutate_vars = function(vars) {
      stopifnot(is_fully_named_list(vars))

      if (length(vars) > 0) {
        private$mutate_vars <- c(private$mutate_vars, vars)
      }

      return(invisible(NULL))
    },

    set_dataname = function(dataname) {
      stopifnot(utils.nest::is_character_single(dataname))
      private$dataname <- dataname
      return(invisible(NULL))
    },
    set_ui = function(args = NULL) {
      private$ui <- function(id) {
        ns <- NS(id)
        tags$div(
          tags$div(
            id = ns("inputs"),
            if_not_null(args, h4(sprintf("Inputs for %s only:", self$get_dataname()))),
            lapply(seq_along(args),
                   function(i) match_ui(ns = ns, value = args[[i]], label = names(args[i]))
            )
          )
        )

      }
      return(invisible(NULL))
    },
    set_server = function() {
      #set_server function in super class does not have a dataname so override that
      #function here
      private$server <- function(input, output, session, data_args = NULL) {
        withProgress(value = 1, message = paste("Pulling", self$get_dataname()), {
          # set args to save them - args set will be returned in the call
          dataset_args <- if_not_null(private$ui_input,
                                      reactiveValuesToList(input)[names(private$ui_input)])
          if (!is_empty(dataset_args)) {
            self$set_args(args = dataset_args)
          }

          # print error if any

          out <- self$pull(args = data_args, try = TRUE)
          observeEvent(out, {
            if (is(out, "try-error")) {
              shinyjs::alert(sprintf("Error pulling %s:\nError message:%s", self$get_dataname(), out))
              stop(out)
            }
          })
        })
        return(invisible(NULL))
      }
      return(invisible(NULL))
    }
  )
)

# NamedDatasetConnector constructors ----

#' Create a new \code{NamedDatasetConnector} object
#'
#' @description
#'  Create \code{NamedDatasetConnector} from \link{callable_function}.
#'
#' @param pull_fun (\code{CallableFunction})\cr
#'  Set the pulling function \link{CallableFunction} to load \code{data.frame}.
#'  \code{dataname} will be used as name of object to be assigned.
#' @param dataname (\code{character})\cr
#'  A given name for the dataset it may not contain spaces
#' @param code (\code{character})\cr
#'  A character string defining the code needed to produce the data set in \code{x}
#' @param label (\code{character})\cr
#'  Label to describe the dataset
#' @param vars (list)\cr
#'   In case when this object code depends on the \code{raw_data} from the other
#'   \code{NamedDataset}, \code{NamedDatasetConnector} object(s) or other constant value,
#'   this/these object(s) should be included
#'
#' @return new \code{NamedDatasetConnector} object
#'
#' @rdname named_dataset_connector
#'
#' @export
named_dataset_connector <- function(pull_fun,
                                    dataname,
                                    code = character(0),
                                    label = character(0),
                                    vars = list()) {
  stopifnot(is(pull_fun, "CallableFunction"))
  stopifnot(is_character_single(dataname))
  stopifnot(is_character_empty(code) || is_character_vector(code))
  stopifnot(is_character_empty(label) || is_character_vector(label))

  x <- NamedDatasetConnector$new(
    pull_fun = pull_fun,
    dataname = dataname,
    code = code,
    label = label,
    vars = vars
  )

  return(x)
}
