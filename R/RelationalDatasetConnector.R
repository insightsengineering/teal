## RelationalDatasetConnector ====
#' A \code{RelationalDatasetConnector} class of objects
#'
#' Objects of this class store the connection function to fetch a single dataset.
#' Note that for some specific connection types (e.g. \code{RICE} or \code{SAICE}),
#' an object of class \code{DataConnection} must be provided.
#' Data can be pulled via the \code{pull} method and accessed directly
#' through the \code{dataset} active binding.
#' Pulled data inherits from the class \link{RelationalDataset}.
#'
#' @importFrom R6 R6Class
RelationalDatasetConnector <- R6::R6Class( #nolint

  ## __Public Methods ====
  classname = "RelationalDatasetConnector",
  inherit = NamedDatasetConnector,
  public = list(
    #' @description
    #' Create a new \code{RelationalDatasetConnector} object. Set the pulling function
    #' load the data. \code{dataname} will be used as name
    #' of object to be assigned.
    #'
    #' @param pull_fun (\code{CallableFunction})\cr
    #'  function to load the data, must return a \code{data.frame}.
    #' @param dataname (\code{character})\cr
    #'  A given name for the dataset, it may not contain spaces
    #' @param keys (\code{keys})\cr
    #'  object of S3 class \code{keys} containing foreign, primary keys and parent information
    #' @param code (\code{character})\cr
    #'  A character string defining the code needed to produce the data set in \code{x}
    #' @param label (\code{character})\cr
    #'  Label to describe the dataset
    #' @param vars (list)\cr
    #'   In case when this object code depends on the \code{raw_data} from the other
    #'   \code{RelationalDataset}, \code{RelationalDatasetConnector} object(s) or other constant value,
    #'   this/these object(s) should be included
    #'
    #' @return new \code{RelationalDatasetConnector} object
    initialize = function(pull_fun, dataname, keys, code = character(0), label = character(0), vars = list()) {
      super$initialize(pull_fun = pull_fun, dataname = dataname, code = code, label = label, vars = vars)
      private$set_keys(keys)
      return(invisible(self))
    },

    #' @description
    #' Get keys of dataset
    #'
    #' @return \code{keys} object
    get_keys = function() {
      return(private$keys)
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
    #' @return \code{self}
    pull = function(args = NULL, try = FALSE) {
      data <- private$pull_internal(args = args, try = try)

      if (!self$is_failed()) {
        private$dataset <- RelationalDataset$new(
          x = data,
          dataname = self$get_dataname(),
          code = private$get_pull_code_class()$get_code(deparse = TRUE),
          keys = self$get_keys(),
          label = self$get_dataset_label()
        )

        if (!is_empty_string(private$get_mutate_code_class()$get_code())) {
          private$dataset <- mutate_dataset(
            private$dataset,
            code = private$get_mutate_code_class()$get_code(deparse = TRUE),
            vars = private$mutate_vars
          )
        }
      }

      return(invisible(self))
    }
  ),

  ## __Private Fields ====
  private = list(
    keys = NULL,

    ## __Private Methods ====
    set_keys = function(keys) {
      stopifnot(is(keys, "keys"))
      private$keys <- keys
      return(invisible(self))
    }
  )
)
