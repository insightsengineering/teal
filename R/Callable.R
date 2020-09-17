## Callable ====
#' A \code{Callable} class of objects
#'
#' Object that stores function name with its arguments. Methods to get call and run it.
#'
#' @importFrom R6 R6Class
Callable <- R6::R6Class( #nolint
  "Callable",

  ## __Public Methods ====
  public = list(
    #' @description
    #' Create a new \code{CallableCode} object
    #'
    #' @param fun (\code{function})\cr
    #'  function to be evaluated in class. Function should be named
    #' @param env (\code{environment})\cr
    #'  environment where the call will be evaluated
    #'
    #' @return new \code{CallableCode} object
    initialize = function(env) {
      stopifnot(is.environment(env))
      private$env <- env
      return(invisible(self))
    },
    #' @description
    #' Assigns \code{x <- value} object to \code{env}
    #' @param x (\code{character} value)\cr
    #'  name of the variable in class environment
    #' @param value (\code{data.frame})\cr
    #'  object to be assigned to \code{x}
    #'
    #' @return arguments the function gets called with
    assign_to_env = function(x, value) {
      assign(x, value, envir = private$env)
      return(invisible(self))
    },
    #' @description
    #' Run function
    #'
    #' @param return (\code{logical} value)\cr
    #'  whether to return an object
    #' @param args (\code{NULL} or named \code{list})\cr
    #'  supplied for callable functions only, these are dynamic arguments passed to function.
    #'  Dynamic arguments are executed in this call and are not saved which means that
    #'  \code{self$get_call()} won't include them later.
    #' @param try (\code{logical} value)\cr
    #'  whether perform function evaluation inside \code{try} clause
    #'
    #' @return nothing or output from function depending on \code{return} argument
    run = function(return = TRUE, args = NULL, try = FALSE) {
      stopifnot(is_logical_single(return))
      stopifnot(is_empty(args) || is_fully_named_list(args))
      stopifnot(is_logical_single(try))

      # args are "dynamic" are used only to evaluate this call
      # - args not saved to private$call persistently
      expr <- self$get_call(deparse = FALSE, args = args)

      res <- if (try) {
        try(
          eval(expr, envir = private$env),
          silent = TRUE
        )
      } else {
        eval(expr, envir = private$env)
      }

      private$check_run_output(res)

      if (return) {
        return(res)
      } else {
        return(invisible(NULL))
      }
    },
    #' @description
    #' Check if evaluation of the function has not failed.
    #'
    #' @return \code{TRUE} if evaluation of the function failed or \code{FALSE}
    #'  if evaluation failed or function hasn't yet been called.
    is_failed = function() {
      return(private$failed)
    },
    #' @description
    #' Get error message from last function execution
    #'
    #' @return \code{try-error} object with error message or \code{character(0)} if last
    #'  function evaluation was successful.
    get_error_message = function() {
      return(private$error_msg)
    }
  ),

  ## __Private Fields ====
  private = list(
    call = NULL, # a call object
    env = NULL, # environment where function is called
    failed = FALSE,
    error_msg = character(0),
    ## __Private Methods ====
    # check if the function was evaluated properly - if not keep error message
    check_run_output = function(res) {
      if (is(res, "try-error")) {
        private$failed <- TRUE
        private$error_msg <- res
      } else {
        private$failed <- FALSE
        private$error_msg <- character(0)
      }

      return(NULL)
    }
  )
)
