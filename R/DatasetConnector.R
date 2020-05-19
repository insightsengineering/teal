#' A \code{DatasetConnector} class of objects
#'
#' Objects of this class store connection function to single dataset. Note that for some specific connection type
#' (e.g. \code{RAICE} or \code{SAICE}), pre-requisite object of class \code{DataConnection} is required.
#' Data can be pulled via \code{pull} method and returned via \code{get_dataset} method.
#'
#' @name DatasetConnector
DatasetConnector <- R6::R6Class( #nolint
  # DatasetConnector public ----
  "DatasetConnector",
  public = list(
    #' @description
    #' Create a new \code{DatasetConnector} object
    #'
    #' @return new \code{DatasetConnector} object
    initialize = function() {
    },
    #' @description
    #' Get executed call
    #'
    #' @param deparse (\code{logical}) whether return deparsed form of a call
    #' @param args (\code{NULL} or named \code{list}) additional dynamic arguments for pull function
    #' @param silent (\code{logical}) whether convert all "missing function" errors to messages
    #'
    #' @return optionally deparsed \code{call} object
    get_call = function(deparse = TRUE, args = NULL, silent = FALSE) {
      stopifnot(is_logical_single(deparse))
      if_cond(private$check_pull_fun(silent = silent), return(), isFALSE)
      res <- call("<-", as.name(private$dataname), private$pull_fun$get_call(deparse = FALSE, args = args))
      if (deparse) {
        return(paste0(deparse(res, width.cutoff = 80L), collapse = "\n"))
      } else {
        return(res)
      }
    },
    #' @description
    #' Get the data from connection
    #'
    #' Pull (if it is not already pulled) and return data
    #'
    #' @param args (\code{NULL} or named \code{list}) additional dynamic arguments for pull function
    #' @param silent (\code{logical}) whether convert all "missing function" errors to messages
    #' @param try (\code{logical}) whether perform function evaluation inside \code{try} clause
    #'
    #' @return if \code{try = TRUE} then \code{try-error} on error, object returned from connection function
    get_dataset = function(args = NULL, silent = FALSE, try = FALSE) {
      if (!private$is_pulled) {
        self$pull(args = args, silent = silent, try = try)
      }

      return(private$dataset)
    },
    #' @description
    #' Get connection dataname
    #'
    #' @return (\code{character}) connection data dataname
    get_dataname = function() {
      return(private$dataname)
    },
    #' @description
    #' Get keys of the dataset
    #'
    #' @return (\code{list}) of keys
    get_keys = function() {
      return(private$keys)
    },
    #' @description
    #' Get path of the dataset
    #'
    #' @return (\code{character})
    get_path = function() {
      return(private$path)
    },
    #' @description
    #' Pull the data
    #'
    #' Force execution of connection function
    #'
    #' @param args (\code{NULL} or named \code{list}) additional dynamic arguments for pull function
    #' @param silent (\code{logical}) whether convert all "missing function" errors to messages
    #' @param try (\code{logical}) whether perform function evaluation inside \code{try} clause
    #'
    #' @return nothing, in order to get the data please use \code{get_dataset} method
    pull = function(args = NULL, silent = FALSE, try = FALSE) {
      if_cond(private$check_pull_fun(silent = silent), return(), isFALSE)
      private$dataset <- private$pull_fun$run(args = args, try = try)
      private$is_pulled <- is(private$dataset, "try-error")
      return(invisible(NULL))
    },
    #' @description
    #' Set dataname
    #'
    #' @param dataname (\code{character}) dataname
    #'
    #' @return nothing
    set_dataname = function(dataname) {
      private$dataname <- dataname
      return(invisible(NULL))
    },
    #' @description
    #' Set dataset keys
    #'
    #' @param keys (\code{keys}) object
    #'
    #' @return nothing
    #'
    #' @importFrom methods is
    set_keys = function(keys) {
      stopifnot(is(keys, "keys"))

      if (!is.null(keys$foreign) && is.null(keys$parent) || (is.null(keys$foreign) && !is.null(keys$parent))) {
        stop(dataname, ": Please specify both foreign keys and a parent!")
      }

      private$keys <- keys
      return(invisible(NULL))
    },
    #' @description
    #' Set connection path
    #'
    #' @param path (\code{character}) path
    #'
    #' @return nothing
    set_path = function(path) {
      stopifnot(is_character_vector(path))
      private$path <- path
      return(invisible(NULL))
    },
    #' @description
    #' Set connection function single argument with value
    #'
    #' @param name (\code{character}) function argument name
    #' @param value value to pass as a \code{name}
    #' @param silent (\code{logical}) whether convert all "missing function" errors to messages
    #'
    #' @return nothing
    set_pull_arg_value = function(name, value, silent = FALSE) {
      if_cond(private$check_pull_fun(silent = silent), return(), isFALSE)
      private$pull_fun$set_arg_value(name, value)
      private$is_pulled <- FALSE
      return(invisible(NULL))
    },
    #' @description
    #' Set connection function arguments with values
    #'
    #' @param args (\code{NULL} or named \code{list}) with values where list names are argument names
    #' @param silent (\code{logical}) whether convert all "missing function" errors to messages
    #'
    #' @return nothing
    set_pull_args = function(args, silent = FALSE) {
      if_cond(private$check_pull_fun(silent = silent), return(), isFALSE)
      private$pull_fun$set_args(args)
      private$is_pulled <- FALSE
      return(invisible(NULL))
    },
    #' @description
    #' Set the pulling function, which itself defines the connection through which the data gets pulled.
    #'
    #' @param fun (\code{CallableFunction}) function to pull the data
    #'
    #'  @return nothing
    set_pull_fun = function(fun) {
      stopifnot(is(fun, "CallableFunction"))
      stop_if_not(list(!is_character_empty(private$dataname), "Please set up dataname before function"))
      private$pull_fun <- fun
      private$is_pulled <- FALSE
      return(invisible(NULL))
    }
  ),
  # DatasetConnector private -----
  private = list(
    path = character(0),
    keys = NULL,
    dataset = NULL,
    dataname = character(0),
    is_pulled = FALSE,
    pull_fun = NULL, # CallableFunction
    silent = FALSE,
    check_pull_fun = function(silent = FALSE) {
      stopifnot(is_logical_single(silent))
      if (is.null(private$pull_fun)) {
        msg <- "Pull function not set"
        if (silent) {
          message(msg)
          return(FALSE)
        } else {
          stop(msg)
        }
      } else {
        return(TRUE)
      }
    }
  )
)
