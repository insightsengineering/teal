## CallableFunction ====
#' A \code{CallableFunction} class of objects
#'
#' Object that stores function name with its arguments. Methods to get call and run it.
#'
#' @importFrom R6 R6Class
#' @importFrom rlang parse_expr
CallableFunction <- R6::R6Class( #nolint
  "CallableFunction",
  inherit = Callable,

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
      super$initialize(env = env)

      stop_if_not(
        list(!missing(fun),
             "A valid function name must be provided.")
      )

      fun_name <- private$get_callable_function(fun)
      private$fun_name <- pdeparse(fun_name)

      private$refresh()

      return(invisible(self))
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
    },
    #' @description
    #'   For scripts and code that contain multiple objects, save the name
    #'   of the object that corresponds to the final dataset of interest.
    #'   This is required for running python scripts with \code{reticulate}.
    #'
    #' @param x (\code{character}) the name of the object produced by the code
    #'   or script.
    #'
    #' @return (\code{self}) invisibly for chaining.
    set_object = function(x) {
      private$object <- x
      private$refresh()
      return(invisible(self))
    }
  ),

  ## __Private Fields ====
  private = list(
    fun_name = character(0),
    args = NULL, # named list with argument names and values
    object = NULL,
    ## __Private Methods ====
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
        } else if (private$fun_name %in% c("py_run_file", "py_run_string"))
          private$call <- rlang::parse_expr(
            sprintf("%s[[%s]]", pdeparse(private$call), pdeparse(private$object))
          )
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
        fun <- str2lang(callable)
      }

      fr <- rev(sys.frames())

      # search for function name over all environments in the stack
      search <- lapply(fr, function(x) substitute(fun, x))
      found_idx <- which((search != as.name("fun")) & (search != as.name("callable")))[[1]]

      found_name <- search[[found_idx]]

      # search for function object by name sequentially
      # over the entire call stack
      for (i in seq_along(fr)[-1]) {
        fn <- tryCatch(get(as.character(found_name), envir = fr[[i]]), error = function(e) NULL)
        is_symbol <- is.symbol(fn)
        if (is_symbol) {
          found_name <- fn
          fn <- tryCatch(get(as.character(fn), envir = fr[[i]]), error = function(e) NULL)
        }
        if (is.function(fn)) {
          return(found_name)
        }
      }

      # if a function was not found, stop
      stop(paste(as.character(callable), "is not a function"))
    }
  )
)

## Constructors ====

#' Create \code{CallableFunction} object
#'
#' @md
#' @description `r lifecycle::badge("experimental")`
#' Create \link{CallableFunction} object to execute specific function and get reproducible
#' call.
#'
#' @param fun (\code{function})\cr
#'   any R function, directly by name or \code{character} string.
#'
#' @return \code{CallableFunction} object
#'
#' @export
#'
#' @examples
#' cf <- callable_function(fun = mean)
#' cf$set_args(list(x = 1:10, na.rm = FALSE))
#' cf$run()
#' cf$get_call()
callable_function <- function(fun) {
  CallableFunction$new(fun)
}
