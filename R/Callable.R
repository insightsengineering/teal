## Callable ====
#'
#' @title A \code{Callable} class of objects
#'
#' Object that stores function name with its arguments. Methods to get call and run it.
#' @keywords internal
#'
Callable <- R6::R6Class( # nolint
  "Callable",

  ## __Public Methods ====
  public = list(
    #' @description
    #' Create a new \code{CallableCode} object
    #'
    #' @param env (\code{environment})\cr
    #'  environment where the call will be evaluated
    #'
    #' @return new \code{CallableCode} object
    initialize = function(env) {
      stopifnot(is.environment(env))
      private$env <- env
      logger::log_trace("Callable initialized.")
      return(invisible(self))
    },
    #' @description
    #' Assigns \code{x <- value} object to \code{env}. Assigned object can't
    #' be modified within local environment as it will be locked by using
    #' \code{lockBinding}. This also means that this object can't be reassigned
    #' which will throw an error.
    #' @param x (\code{character} value)\cr
    #'  name of the variable in class environment
    #' @param value (\code{data.frame})\cr
    #'  object to be assigned to \code{x}
    #'
    #' @return (\code{self}) invisibly for chaining.
    assign_to_env = function(x, value) {
      # assign variable once
      if (!exists(x, envir = private$env)) {
        assign(x, value, envir = private$env)

        # variable can't be modified
        lockBinding(sym = x, env = private$env)
        logger::log_trace("Callable$assign_to_env assigned '{ x }' to the environment.")
      }

      return(invisible(self))
    },
    #' @description
    #' Execute \code{Callable} function or code.
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
    #' @return nothing or output from function depending on \code{return}
    #' argument. If \code{run} fails it will return object of class \code{simple-error error}
    #' when \code{try = TRUE} or will stop if \code{try = FALSE}.
    run = function(return = TRUE, args = NULL, try = FALSE) {
      checkmate::assert_flag(return)
      checkmate::assert_list(args, names = "unique", min.len = 0, null.ok = TRUE)
      checkmate::assert_flag(try)

      # args are "dynamic" are used only to evaluate this call
      # - args not saved to private$call persistently
      expr <- self$get_call(deparse = FALSE, args = args)

      res <- tryCatch(
        eval(expr, envir = private$env),
        error = function(e) e
      )
      private$check_run_output(res, try = try)

      logger::log_trace("Callable$run callable has been run.")
      if (return) {
        return(res)
      } else {
        return(invisible(NULL))
      }
    },
    #' @description
    #' Check if evaluation of the function has not failed.
    #'
    #' @return (\code{logical}) \code{TRUE} if evaluation of the function failed or \code{FALSE}
    #'  if evaluation failed or function hasn't yet been called.
    is_failed = function() {
      return(private$failed)
    },
    #' @description
    #' Get error message from last function execution
    #'
    #' @return (\code{character}) object with error message or \code{character(0)} if last
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

    # The deep clone function deep clones the environment of the Callable so
    # that it is distinct for the copy
    deep_clone = function(name, value) {
      deep_clone_r6(name, value)
    },
    # Checks output and handles error messages
    check_run_output = function(res, try) {
      if (is(res, "error")) {
        msg <- conditionMessage(res)
        is_locked <- grepl(pattern = "cannot change value of locked", x = msg)

        error_msg <- if (is_locked) {
          locked_var <- gsub("^.+\\'(.+)\\'$", "\\1", x = msg)
          sprintf(
            "Modification of the local variable '%1$s' is not allowed. %2$s '%1$s'",
            locked_var,
            "Please add proxy variable to CallableCode to obtain results depending on altered"
          )
        } else {
          msg
        }

        if (try) {
          private$failed <- TRUE
          private$error_msg <- error_msg
          logger::log_error("Callable$check_run_output { deparse1(error_msg) }.")
        } else {
          stop(error_msg, call. = FALSE)
        }
      } else {
        private$failed <- FALSE
        private$error_msg <- character(0)
      }
    }
  )
)
