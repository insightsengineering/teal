#' A \code{CallableFunction} class of objects
#'
#' Object that stores function name with it's arguments. Methods to get call and run it.
#'
#' @name CallableFunction
CallableFunction <- R6::R6Class( #nolint
  "CallableFunction",
  public = list(
    #' @description
    #' Create a new \code{CallableFunction} object
    #'
    #' @param fun (\code{function}) function to set up
    #'
    #' @return new \code{CallableFunction} object
    initialize = function(fun) {
      stopifnot(is.function(fun))

      private$fun_name <- deparse(private$get_callable_function(fun))
      self$refresh()
      invisible(self)
    },
    #' @description
    #' Run function
    #'
    #' @param return (\code{logical}) whether to return an object
    #' @param args (\code{NULL} or named \code{list}) dynamic arguments to function
    #' @param try (\code{logical}) whether perform function evaluation inside \code{try} clause
    #'
    #' @return nothing or output from function depending on \code{return} argument
    run = function(return = TRUE, args = NULL, try = FALSE) {
      stopifnot(is_logical_single(return))
      stopifnot(is.null(args) || (is.list(args) && is_fully_named_list(args)))
      stopifnot(is_logical_single(try))

      res <- if (try) {
        try(
          eval(self$get_call(deparse = FALSE, args = args)),
          silent = TRUE
        )
      } else {
        eval(self$get_call(deparse = FALSE, args = args))
      }
      if (return) {
        return(res)
      } else {
        return(invisible(NULL))
      }
    },
    #' @description
    #' Get function call with substituted arguments in \code{args}.
    #' These arguments will not be stored in the object.
    #'
    #' @param deparse (\code{logical}) whether to return a deparsed version of call
    #' @param args (\code{NULL} or named \code{list}) dynamic arguments to function
    #'
    #' @return \code{call} or \code{character} depending on \code{deparse} argument
    get_call = function(deparse = TRUE, args = NULL) {
      stopifnot(is_logical_single(deparse))
      stopifnot(is.null(args) || (is.list(args) && is_fully_named_list(args)))

      old_args <- private$args
      if_not_null(args, self$set_args(args))

      res <- if (deparse) {
        paste0(deparse(private$call, width.cutoff = 80L), collapse = "\n")
      } else {
        private$call
      }

      # set args back to default
      if (!is.null(args)) {
        lapply(names(args), self$set_arg_value, NULL)
        self$set_args(old_args)
      }


      return(res)
    },
    #' @description
    #' Refresh call with function name and saved arguments
    #'
    #' @importFrom rlang parse_expr
    #' @return nothing
    refresh = function() {
      if (!is.null(private$fun_name) || !identical(private$fun_name, character(0))) {

        # replaced str2lang found at:
        # https://rlang.r-lib.org/reference/call2.html
        private$call <- as.call(c(rlang::parse_expr(private$fun_name), if_empty(private$args, NULL)))
      }
    },
    #' @description
    #' Set up function arguments
    #'
    #' @param args (\code{NULL} or named \code{list}) dynamic arguments to function
    #'
    #' @return nothing
    set_args = function(args) {
      if (is.null(args)) {
        return(invisible(NULL))
      }
      stopifnot(is.list(args) && is_fully_named_list(args))

      for (idx in seq_along(args)) {
        self$set_arg_value(names(args)[[idx]], args[[idx]])
      }
      self$refresh()
      return(invisible(NULL))
    },
    #' @description
    #' Set up single function argument with value
    #'
    #' @param name (\code{character}) argument name
    #' @param value argument value
    #'
    #' @return nothing
    set_arg_value = function(name, value) {
      stopifnot(is_character_single(name))
      private$args[[name]] <- value
      self$refresh()
      return(invisible(NULL))
    }
  ),
  private = list(
    fun_name = character(0),
    args = list(), # named list with argument names and values
    call = NULL, # a call object
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
      stopifnot(is.function(callable))

      fr <- rev(sys.frames())
      callable <- substitute(callable, fr[[1]])

      for (i in seq_along(fr)[-1]) {
        callable <- eval(bquote(substitute(.(callable), fr[[i]])))
      }

      return(callable)
    }
  )
)
