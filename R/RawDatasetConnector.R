#' A \code{RawDatasetConnector} class of objects
#'
#' Objects of this class store the connection function to fetch a single dataset.
#' Note that for some specific connection types (e.g. \code{RICE} or \code{SAICE}),
#' an object of class \code{DataConnection} must be provided.
#' Data can be pulled via the \code{pull} method and accessed directly
#' through the \code{dataset} active binding.
#' Pulled data inherits from the class \link{RelationalDataset}.
#'
#' @name RawDatasetConnector
RawDatasetConnector <- R6::R6Class( #nolint
  # RawDatasetConnector public ----
  classname = "RawDatasetConnector",
  public = list(
    #' @description
    #' Create a new \code{RawDatasetConnector} object. Set the pulling function
    #' \link{CallableFunction} which returns a \code{data.frame}, e.g. by reading
    #' from a function or creating it on the fly.
    #'
    #' @param pull_fun (\code{CallableFunction})\cr
    #' function to pull the data.
    #'
    #' @return new \code{RawDatasetConnector} object
    initialize = function(pull_fun) {
      private$set_pull_fun(pull_fun)
      return(invisible(self))
    },

    #' @description
    #' Set arguments to the pulling function
    #'
    #' @param args (\code{NULL} or named \code{list}) dynamic arguments to function
    #'
    #' @return nothing
    set_args = function(args) {
      private$pull_fun$set_args(args)
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
      private$get_pull_code(deparse)
    },

    #' @description
    #' Get dataset
    #'
    #' @return dataset (\code{<...>Dataset})
    get_dataset = function() {
      if (is.null(private$dataset)) {
        stop("dataset has not been pulled yet\n - please use `load_dataset()` first.",
             call. = FALSE)
      }
      return(private$dataset)
    },

    #' @description
    #' Get pull function
    #'
    #' @return dataset (\code{CallableFunction})
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
    #' additional dynamic arguments for pull function. \code{args} can be omitted if \code{pull_fun}
    #' from constructor already contains all necessary arguments to pull data. One can try
    #' to execute \code{pull_fun} directly by \code{x$pull_fun$run()} or to get code using
    #' \code{x$pull_fun$get_code()}. \code{args} specified in pull are used temporary to get data but
    #' not saved in code.
    #' @param try (\code{logical}) whether perform function evaluation inside \code{try} clause
    #'
    #' @return nothing, in order to get the data please use \code{get_data} method
    pull = function(args = NULL, try = FALSE) {
      if (is.null(args)) {
        data <- private$pull_fun$run(try = try)
      } else {
        data <- private$pull_fun$run(args = args, try = try)
      }
      private$dataset <- RawDataset$new(data)
      return(invisible(self))
    }
  ),
  # RawDatasetConnector private -----
  private = list(
    dataset = NULL, # RawDataset
    pull_fun = NULL, # CallableFunction

    get_pull_code = function(deparse, args = NULL) {
      return(private$pull_fun$get_call(deparse, args))
    },

    set_pull_fun = function(pull_fun) {
      stopifnot(is(pull_fun, "CallableFunction"))
      private$pull_fun <- pull_fun
      return(invisible(NULL))
    }
  )
)

#' Create \code{RawDatasetConnector} object
#'
#' Create \link{RawDatasetConnector} object to execute specific call to fetch data
#' @param pull_fun (\code{CallableFunction})\cr
#'   function with necessary arguments set to fetch data from connection.
#' @examples
#' ds <- raw_dataset_connector(pull_fun = callable_function(data.frame))
#' set_args(ds, list(x = 1:5, y = letters[1:5]))
#' ds$pull()
#' ds$get_raw_data()
#' ds$get_code()
#' @return \code{RawDatasetConnector} object
#' @export
raw_dataset_connector <- function(pull_fun) {
  RawDatasetConnector$new(pull_fun = pull_fun)
}
