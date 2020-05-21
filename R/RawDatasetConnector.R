#' A \code{RawDatasetConnector} class of objects
#'
#' Objects of this class store connection function to single dataset. Note that for some specific
#' connection type (e.g. \code{RAICE} or \code{SAICE}), pre-requisite object of class
#' \code{DataConnection} is required. Data can be pulled via \code{pull} method and accessed directly
#' by \code{dataset} active binding. Pulled data is of class \link{RawDataset} and it can be accessed
#'
#' @name RawDatasetConnector
RawDatasetConnector <- R6::R6Class( #nolint
  # RawDatasetConnector public ----
  classname = "RawDatasetConnector",
  public = list(
    #' @description
    #' Create a new \code{RawDatasetConnector} object. Set the pulling function
    #' \link{CallableFunction} which read or create \code{data.frame}.
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
      private$.pull_fun$set_args(args)
    },

    #' @description
    #' Get code to get data
    #'
    #' @param deparse (\code{logical}) whether return deparsed form of a call
    #'
    #' @return optionally deparsed \code{call} object
    get_code = function(deparse = TRUE) {
      stopifnot(is_logical_single(deparse))
      private$get_pull_code(deparse)
    },

    #' @description
    #' Get dataset
    #'
    #' @return dataset (\code{<...>Dataset})
    get_dataset = function() {
      return(self$dataset)
    },

    #' @description
    #' Get raw data from dataset
    #'
    #' @return data (data.frame)
    get_raw_data = function() {
      return(self$dataset$get_raw_data())
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
      data <- private$.pull_fun$run(args = args, try = try)
      private$.dataset <- RawDataset$new(data)
      return(invisible(self))
    }
  ),
  # RawDatasetConnector private -----
  private = list(
    .dataset = NULL, # RawDataset
    .pull_fun = NULL, # CallableFunction

    get_pull_code = function(deparse) {
      return(private$.pull_fun$get_call(deparse))
    },

    set_pull_fun = function(pull_fun) {
      stopifnot(is(pull_fun, "CallableFunction"))
      private$.pull_fun <- pull_fun
      return(invisible(NULL))
    }
  ),
  # RawDatasetConnector active -----
  active = list(
    #' @field dataset (read-only) object of class \code{RawDataset}.
    dataset = function() {
      if (is.null(private$.dataset)) {
        stop("dataset has not been pulled yet\n - please use `load_dataset()` first.",
             call. = FALSE)
      }

      return(private$.dataset)
    },
    #' @field pull_fun (read-only) object of class \code{CallableFunction}.
    pull_fun = function() {
      return(private$.pull_fun)
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
