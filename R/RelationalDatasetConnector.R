#' A \code{RelationalDatasetConnector} class of objects
#'
#' Objects of this class store the connection function to fetch a single dataset.
#' Note that for some specific connection types (e.g. \code{RICE} or \code{SAICE}),
#' an object of class \code{DataConnection} must be provided.
#' Data can be pulled via the \code{pull} method and accessed directly
#' through the \code{dataset} active binding.
#' Pulled data inherits from the class \link{RelationalDataset}.
#'
#' @name RelationalDatasetConnector
RelationalDatasetConnector <- R6::R6Class( #nolint
  # RelationalDatasetConnector public ----
  classname = "RelationalDatasetConnector",
  inherit = RawDatasetConnector,
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
    #' @return new \code{RawDatasetConnector} object
    initialize = function(pull_fun, dataname, keys, code = character(0), label = character(0), vars = list()) {
      super$initialize(pull_fun = pull_fun, vars = vars)
      private$set_dataname(dataname)
      private$set_keys(keys)
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
    #' Get keys of dataset
    #'
    #' @return \code{keys} object
    get_keys = function() {
      return(private$keys)
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
      stopifnot(utils.nest::is_character_vector(label, 0, 1))
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
    mutate_dataset = function(code) {
      if (!is.null(private$dataset)) {
        private$dataset <- mutate_dataset(private$dataset, code = code)
      } else {
        private$set_mutate_code(code)
      }

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
    #' @return nothing, in order to get the data please use \code{get_data} method
    pull = function(args = NULL, try = FALSE) {
      data <- private$pull_internal(args = args, try = try)

      private$dataset <- RelationalDataset$new(
        x = data,
        dataname = self$get_dataname(),
        code = private$get_pull_code(deparse = TRUE),
        keys = self$get_keys(),
        label = self$get_dataset_label()
      )

      if (!is_empty(private$get_mutate_code())) {
        private$dataset <- mutate_dataset(
          private$dataset,
          code = private$get_mutate_code(deparse = TRUE)
        )
      }

      return(invisible(self))
    },
    #' @description
    #' Check if dataset has already been pulled.
    #'
    #' @return \code{TRUE} if connector has been already pulled, else \code{FALSE}
    is_pulled = function() {
      isFALSE(is.null(private$dataset))
    },
    #' @description
    #'
    #' Derive the arguments this connector will pull with
    get_pull_args = function() {
      private$pull_fun$get_args()
    }
  ),

  # RelationalDatasetConnector private ----
  private = list(
    dataname = character(0),
    keys = NULL,
    dataset_label = character(0),
    mutate_code = NULL,
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
      stopifnot(utils.nest::is_character_vector(code, 0, 1))
      if (length(code) > 0) {
        private$mutate_code <- as.list(as.call(parse(text = code)))
      } else {
        private$mutate_code <- NULL
      }

      return(invisible(NULL))
    },

    set_dataname = function(dataname) {
      stopifnot(utils.nest::is_character_single(dataname))
      private$dataname <- dataname
      return(invisible(NULL))
    },

    set_keys = function(keys) {
      stopifnot(is(keys, "keys"))
      private$keys <- keys
      return(invisible(NULL))
    }
  )
)
