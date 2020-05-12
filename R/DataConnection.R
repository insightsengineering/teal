#' A \code{DataConnection} class of objects
#'
#' Objects of this class store connection to data source.
#' It can be DB or server (\code{RICE} or \code{SAICE}) connection.
#'
#' @name DatasetConnector
#'
#' @examples
#' \dontrun{
#' open_fun <- CallableFunction$new(data.frame)  # define opening function
#' open_fun$set_args(list(x = 1:5))   # define fixed arguments to opening function
#'
#' close_fun <- CallableFunction$new(print) # define closing function
#' close_fun$set_args(list(x = "Hi there"))  # define fixed arguments to closing function
#'
#' x <- DataConnection$new() # define connection
#' x$set_open_fun(open_fun) # define opening function
#' x$set_close_fun(close_fun) # define closing function
#'
#' x$set_open_args(args = list(y = letters[1:5])) # define additional arguments if necessary
#'
#' x$open() # call opening function
#'
#' x$open(args = list(x = 1:5, y = letters[1:5])) # able to call opening function with arguments
#'
#' x$get_open_call() # check reroducible R code
#' # get data from connection via DataConnector$get_dataset()
#'
#' x$close() # call closing function
#' }
DataConnection <- R6::R6Class( #nolint
  # DataConnection public ----
  "DataConnection",
  public = list(
    #' @description
    #' Create a new \code{DataConnection} object
    #'
    #' @return new \code{DataConnection} object
    init = function() {
    },
    #' @description
    #' Open the connection.
    #'
    #' Note that if the connection is already opened then it does nothing.
    #'
    #' @param args (\code{NULL} or named \code{list}) additional arguments not set up previously
    #' @param silent (\code{logical}) whether convert all "missing function" errors to messages
    #' @param try (\code{logical}) whether perform function evaluation inside \code{try} clause
    #'
    #' @return if \code{try = TRUE} then \code{try-error} on error, \code{NULL} otherwise
    open = function(args = NULL, silent = FALSE, try = FALSE) {
      stopifnot(is.null(args) || (is.list(args) && is_fully_named_list(args)))
      if_cond(private$check_open_fun(silent = silent), return(), isFALSE)
      if (private$opened) {
        return(invisible(NULL))
      } else {
        open_res <- private$open_fun$run(args = args, try = try)
        if (is(open_res, "try-error")) {
          return(open_res)
        } else {
          private$opened <- TRUE
          return(invisible(NULL))
        }
      }
    },
    #' @description
    #' Get executed open connection call
    #'
    #' @param deparse (\code{logical}) whether return deparsed form of a call
    #' @param args (\code{NULL} or named \code{list}) additional arguments not set up previously
    #' @param silent (\code{logical}) whether convert all "missing function" errors to messages
    #'
    #' @return optionally deparsed \code{call} object
    get_open_call = function(deparse = TRUE, args = NULL, silent = FALSE) {
      stopifnot(is_logical_single(deparse))
      stopifnot(is.null(args) || (is.list(args) && is_fully_named_list(args)))
      if_cond(private$check_open_fun(silent = silent), return(), isFALSE)
      private$open_fun$get_call(deparse = deparse, args = args)
    },
    #' @description
    #' Close the connection.
    #'
    #' @param silent (\code{logical}) whether convert all "missing function" errors to messages
    #' @param try (\code{logical}) whether perform function evaluation inside \code{try} clause
    #'
    #' @return if \code{try = TRUE} then \code{try-error} on error, \code{NULL} otherwise
    close = function(silent = FALSE, try = FALSE) {
      if_cond(private$check_close_fun(silent = silent), return(), isFALSE)
      if (private$opened) {
        close_res <- private$close_fun$run(try = try)
        if (is(close_res, "try-error")) {
          return(close_res)
        } else {
          private$opened <- FALSE
          return(invisible(NULL))
        }
      }
    },
    #' @description
    #' Get executed close connection call
    #'
    #' @param deparse (\code{logical}) whether return deparsed form of a call
    #' @param silent (\code{logical}) whether convert all "missing function" errors to messages
    #'
    #' @return optionally deparsed \code{call} object
    get_close_call = function(deparse = TRUE, silent = FALSE) {
      stopifnot(is_logical_single(deparse))
      if_cond(private$check_close_fun(silent = silent), return(), isFALSE)
      private$close_fun$get_call(deparse = deparse)
    },
    #' @description
    #' Set open connection function
    #'
    #' @param fun (\code{CallableFunction}) function to open connection
    #'
    #' @return nothing
    set_open_fun = function(fun) {
      stopifnot(is(fun, "CallableFunction"))
      private$open_fun <- fun
      self$refresh_calls()
      return(invisible(NULL))
    },
    #' @description
    #' Set open connection function argument
    #'
    #' @param args (\code{NULL} or named \code{list}) with values where list names are argument names
    #' @param silent (\code{logical}) whether convert all "missing function" errors to messages
    #'
    #' @return nothing
    set_open_args = function(args, silent = FALSE) {
      stopifnot(is.null(args) || (is.list(args) && is_fully_named_list(args)))
      if_cond(private$check_open_fun(silent = silent), return(), isFALSE)
      private$open_fun$set_args(args)
    },
    #' @description
    #' Set close connection function
    #'
    #' @param fun (\code{CallableFunction}) function to close connection
    #'
    #' @return nothing
    set_close_fun = function(fun) {
      stopifnot(is(fun, "CallableFunction"))
      private$close_fun <- fun
      self$refresh_calls()
      return(invisible(NULL))
    },
    #' @description
    #' Set close connection function argument
    #'
    #' @param args (named \code{list}) with values where list names are argument names
    #' @param silent (\code{logical}) whether convert all "missing function" errors to messages
    #'
    #' @return nothing
    set_close_args = function(args, silent = FALSE) {
      stopifnot(is.null(args) || (is.list(args) && is_fully_named_list(args)))
      if_cond(private$check_close_fun(silent = silent), return(), isFALSE)
      private$close_fun$set_args(args)
    },
    #' @description
    #' Refresh both open and close calls
    #'
    #' @return nothing
    refresh_calls = function() {
      if (!is.null(private$open_fun)) {
        private$open_fun$refresh()
      }
      if (!is.null(private$close_fun)) {
        private$close_fun$refresh()
      }
    }
  ),
  # DataConnection private ----
  private = list(
    open_fun = NULL, # ArgFun
    check_open_fun = function(silent = FALSE) {
      stopifnot(is_logical_single(silent))

      if (is.null(private$open_fun)) {
        msg <- "Open connection function not set"
        if (silent) {
          message(msg)
          return(FALSE)
        } else {
          stop(msg)
        }
      } else {
        return(TRUE)
      }
    },
    close_fun = NULL, # ArgFun
    check_close_fun = function(silent = FALSE) {
      stopifnot(is_logical_single(silent))

      if (is.null(private$close_fun)) {
        msg <- "Close connection function not set"
        if (silent) {
          message(msg)
          return(FALSE)
        } else {
          stop(msg)
        }
      } else {
        return(TRUE)
      }
    },
    opened = FALSE
  )
)


# DataConnection wrappers ----
#' Open connection to \code{random.cdisc.data}
#'
#' @return \code{DataConnection} type of object
rcd_connection <- function() {

  check_pckg_quietly("random.cdisc.data", "random.cdisc.data package not available.") # nolint

  fun <- CallableFunction$new(library) # nolint
  fun$set_args(list(package = "random.cdisc.data"))

  x <- DataConnection$new() # nolint
  x$set_open_fun(fun)

  return(x)
}


#' Open connection to \code{random.cdisc.data}
#'
#' @return \code{DataConnection} type of object
rice_connection <- function() {

  check_pckg_quietly("rice",
                     paste0("Connection to entimICE via rice was requested, but rice package is not available.",
                            "Please install it from https://github.roche.com/Rpackages/rice."))

  open_fun <- CallableFunction$new(rice::rice_session_open) # nolint

  close_fun <- CallableFunction$new(rice::rice_session_close) # nolint
  close_fun$set_args(list(message = FALSE))

  x <- DataConnection$new() # nolint
  x$set_open_fun(open_fun)
  x$set_close_fun(close_fun)

  return(x)
}
