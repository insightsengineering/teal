## CallableFunction ====
#' A \code{CallableFunction} class of objects
#'
#' Object that stores function name with its arguments. Methods to get call and run it.
#'
#' @importFrom R6 R6Class
#' @importFrom rlang parse_expr
CallableFunction <- R6::R6Class( #nolint
  "CallableFunction",

  ## __Public Methods ====
  public = list(
    #' @description
    #' Create a new \code{CallableFunction} object
    #'
    #' @param fun (\code{function})\cr
    #'  function to be evaluated in class. Function should be named
    #' @param env (\code{environment})\cr
    #'  environment where function will be evaluated
    #'
    #' @return new \code{CallableFunction} object
    initialize = function(fun, env = new.env(parent = parent.env(globalenv()))) {
      stopifnot(is.environment(env))

      fun_name <- private$get_callable_function(fun)
      private$fun_name <- pdeparse(fun_name)
      private$env <- env
      private$refresh()
      invisible(self)
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
    #' Check if evaluation of the function has not failed.
    #'
    #' @return \code{TRUE} if evaluation of the function failed or \code{FALSE}
    #'  if evaluation failed or function hasn't yet been called.
    is_failed = function() {
      return(private$failed)
    },
    #' @description
    #' get the arguments a function gets called with
    #'
    #' @return arguments the function gets called with
    get_args = function() {
      return(private$args)
    },
    #' @description
    #' Get function call with substituted arguments in \code{args}.
    #' These arguments will not be stored in the object.
    #'
    #' @param deparse (\code{logical} value)\cr
    #'  whether to return a deparsed version of call
    #' @param args (\code{NULL} or named \code{list})\cr
    #'  dynamic arguments to function
    #'
    #' @return \code{call} or \code{character} depending on \code{deparse} argument
    get_call = function(deparse = TRUE, args = NULL) {
      stopifnot(is_logical_single(deparse))
      stopifnot(is_empty(args) || is_fully_named_list(args))

      old_args <- private$args
      if_not_empty(args, self$set_args(args))

      res <- if (deparse) {
        pdeparse(private$call)
      } else {
        private$call
      }

      # set args back to default
      if (!is_empty(args)) {
        lapply(names(args), self$set_arg_value, NULL)
        self$set_args(old_args)
      }

      return(res)
    },
    #' @description
    #' Get error message from last function execution
    #'
    #' @return \code{try-error} object with error message or \code{character(0)} if last
    #'  function evaluation was successful.
    get_error_message = function() {
      return(private$error_msg)
    },
    #' @description
    #' Run function
    #'
    #' @param return (\code{logical} value)\cr
    #'  whether to return an object
    #' @param args (\code{NULL} or named \code{list})\cr
    #'  dynamic arguments passed to function. Dynamic arguments are executed in this call and are not
    #'  saved which means that \code{self$get_call()} won't include them later.
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
    #' Set up function arguments
    #'
    #' @param args (\code{NULL} or named \code{list})\cr
    #'  function arguments to be stored persistently in the object. Setting \code{args} doesn't
    #'  remove other \code{args}, only create new of modify previous of the same name.
    #'  To clean arguments specify \code{args = NULL}.
    #'
    #' @return \code{self} invisibly for chaining.
    set_args = function(args) {
      # remove args if empty
      if (is_empty(args)) {
        private$args <- NULL
        private$refresh()
        return(invisible(self))
      }
      stopifnot(is.list(args) && is_fully_named_list(args))

      for (idx in seq_along(args)) {
        self$set_arg_value(name = names(args)[[idx]],
                           value = args[[idx]])
      }
      return(invisible(self))
    },
    #' @description
    #' Set up single function argument with value
    #'
    #' @param name (\code{character}) argument name
    #' @param value argument value
    #'
    #' @return \code{self} invisibly for chaining.
    set_arg_value = function(name, value) {
      stopifnot(is_character_single(name))
      arg_names <- names(formals(eval(str2lang(private$fun_name))))
      stopifnot(name %in% arg_names || "..." %in% arg_names || is.null(arg_names))

      if (is_empty(private$args)) {
        private$args <- list()
      }
      private$args[[name]] <- value

      private$refresh()
      return(invisible(self))
    }
  ),

  ## __Private Fields ====
  private = list(
    fun_name = character(0),
    args = NULL, # named list with argument names and values
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
    },
    # @description
    # Refresh call with function name and saved arguments
    #
    # @return nothing
    refresh = function() {
      if (!is.null(private$fun_name) || !identical(private$fun_name, character(0))) {

        # replaced str2lang found at:
        # https://rlang.r-lib.org/reference/call2.html
        private$call <- as.call(
          c(rlang::parse_expr(private$fun_name), private$args)
        )

        # exception for source(...)$value
        if (private$fun_name == "source") {
          private$call <- rlang::parse_expr(
            sprintf("%s$value", pdeparse(private$call))
          )
        }

      }
    },
    # @description
    # Finds original function name
    #
    # In recursive call when function is passed from environment to environment
    # and name of the function is overwritten
    #
    # @param callable \code{function} function to be found
    #
    # @return name \code{name} of the original function
    #
    get_callable_function = function(callable) {
      if (is.character(callable)) {
        callable <- str2lang(callable)
      }

      fr <- rev(sys.frames())
      callable <- substitute(callable, fr[[1]])

      # search for function object by name sequentially
      # over the entire call stack
      fn <- tryCatch(get(as.character(callable), envir = fr[[1]]),
                     error = function(e) NULL)

      for (i in seq_along(fr)[-1]) {
        callable <- eval(bquote(substitute(.(callable), fr[[i]])))

        fn <- tryCatch(get(as.character(callable), envir = fr[[1]]),
                       error = function(e) NULL)
      }

      # if a function is not found, stop initialization
      stopifnot(is.function(fn))

      return(callable)
    }
  )
)

## Constructors ====

#' Create \code{CallableFunction} object
#'
#' Create \link{CallableFunction} object to execute specific function and get reproducible
#' call.
#' @param fun (\code{function})\cr
#'   any R function, directly by name or \code{character} string.
#' @return \code{CallableFunction} object
#' @examples
#' cf <- callable_function(fun = mean)
#' cf$set_args(list(x = 1:10, na.rm = FALSE))
#' cf$run()
#' cf$get_call()
#' @export
callable_function <- function(fun) {
  CallableFunction$new(fun)
}
