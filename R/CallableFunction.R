## CallableFunction ====
#'
#' @title A \code{CallableFunction} class of objects
#'
#' Object that stores function name with its arguments. Methods to get call and run it.
#'
#' @keywords internal
#'
CallableFunction <- R6::R6Class( # nolint
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
      if (missing(fun)) {
        stop("A valid function name must be provided.")
      }
      if (!(checkmate::test_string(fun) || is.function(fun) || is.call(fun) || is.symbol(fun))) {
        stop("CallableFunction can be specified as character, symbol, call or function")
      }

      fun_name <- private$get_callable_function(fun)
      private$fun_name <- deparse1(fun_name, collapse = "\n")

      private$refresh()

      logger::log_trace("CallableFunction initialized with function: { deparse1(private$fun_name) }.")

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
      checkmate::assert_flag(deparse)
      checkmate::assert_list(args, names = "strict", min.len = 0, null.ok = TRUE)

      old_args <- private$args
      if (length(args) > 0) self$set_args(args)

      res <- if (deparse) {
        deparse1(private$call, collapse = "\n")
      } else {
        private$call
      }

      # set args back to default
      if (length(args) > 0) {
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
    #' @return (`self`) invisibly for chaining.
    set_args = function(args) {
      # remove args if empty
      if (length(args) == 0) {
        private$args <- NULL
        private$refresh()
        return(invisible(self))
      }
      checkmate::assert_list(args, min.len = 0, names = "unique")

      for (idx in seq_along(args)) {
        self$set_arg_value(
          name = names(args)[[idx]],
          value = args[[idx]]
        )
      }
      logger::log_trace(
        "CallableFunction$set_args args set for function: { deparse1(private$fun_name) }."
      )

      return(invisible(self))
    },
    #' @description
    #' Set up single function argument with value
    #'
    #' @param name (\code{character}) argument name
    #' @param value argument value
    #'
    #' @return (`self`) invisibly for chaining.
    set_arg_value = function(name, value) {
      checkmate::assert_string(name)
      arg_names <- names(formals(eval(str2lang(private$fun_name))))
      stopifnot(name %in% arg_names || "..." %in% arg_names || is.null(arg_names))

      if (length(private$args) == 0) {
        private$args <- list()
      }
      private$args[[name]] <- value

      private$refresh()
      logger::log_trace("CallableFunction$set_arg_value args values set for arg: { deparse1(name) }.")

      return(invisible(self))
    }
  ),

  ## __Private Fields ====
  private = list(
    fun_name = character(0),
    args = NULL, # named list with argument names and values
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
            sprintf("%s$value", deparse1(private$call, collapse = "\n"))
          )
        } else if (private$fun_name %in% c("py_run_file", "py_run_string")) {
          private$call <- rlang::parse_expr(
            sprintf("%s[[%s]]", deparse1(private$call, collapse = "\n"), deparse1(private$object, collapse = "\n"))
          )
        }
      }
    },
    # @description
    # Returns a call to a function
    #
    # Returns the call to the function as defined in the enclosing environment.
    #
    # @param callable \code{function, character, call, symbol} the function to return
    #
    # @return `call` the call to the function
    #
    get_callable_function = function(callable) {
      if (is.character(callable) && private$is_prefixed_function(callable)) {
        private$get_call_from_prefixed_function(callable)
      } else {
        private$get_call_from_symbol(callable)
      }
    },
    # @param function_name (`character`) the function name prefixed with \code{::}
    # and the package name
    # @return `call` the call to the function passed to this method
    get_call_from_prefixed_function = function(function_name) {
      package_function_names <- strsplit(function_name, "::")[[1]]
      fun <- get(package_function_names[2], envir = getNamespace(package_function_names[1]))
      if (!is.function(fun)) {
        stop(sprintf("object '%s' of mode 'function' was not found", function_name))
      }
      str2lang(function_name)
    },
    # @param symbol (`function`, `symbol` or `character`) the item matching a function
    # @return `call` the call to the function passed to this method
    get_call_from_symbol = function(symbol) {
      fun <- match.fun(symbol)
      fun_environment <- environment(fun)
      if (isNamespace(fun_environment)) {
        fun_name <- get_binding_name(fun, fun_environment)
        namespace_name <- strsplit(rlang::env_name(fun_environment), ":")[[1]][2]
        if (namespace_name != "base") {
          fun_name <- paste(namespace_name, fun_name, sep = "::")
        }
        fun <- str2lang(fun_name)
      }
      fun
    },
    # Checks whether a character vector is of this format
    # <package_name>::<function_name>
    #
    # @param function_name (`character`) the character vector
    # @return `logical` `TRUE` if \code{function_name} is of the specified
    # format; `FALSE` otherwise
    #
    is_prefixed_function = function(function_name) {
      grepl("^[[:ascii:]]+::[[:ascii:]]+$", function_name, perl = TRUE)
    }
  )
)

## Constructors ====

#' Create \code{CallableFunction} object
#'
#' @description `r lifecycle::badge("stable")`
#' Create \code{\link{CallableFunction}} object to execute specific function and get reproducible
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
#' cf <- callable_function(fun = stats::median)
#' cf$set_args(list(x = 1:10, na.rm = FALSE))
#' cf$run()
#' cf$get_call()
callable_function <- function(fun) {
  CallableFunction$new(fun)
}

#' Gets the name of the binding
#'
#' Gets the name of the object by finding its origin.
#' Depending on type of object function uses different methods
#' to obtain original location. If no `env` is specified then
#' object is tracked by `substitute` along the `sys.frames`.
#' If `env` is specified then search is limited to specified
#' environment.\cr
#'
#' @note
#' Raises an error if the object is not found in the environment.
#'
#' @param object (R object)\cr
#'   any R object
#' @param envir (`environment`)\cr
#'  if origin of the object is known then should be provided for
#'  more precise search
#' @return character
#' @keywords internal
#'
get_binding_name <- function(object, envir) {
  bindings_names <- ls(envir)
  identical_binding_mask <- vapply(
    bindings_names,
    function(binding_name) identical(get(binding_name, envir), object),
    FUN.VALUE = logical(1),
    USE.NAMES = FALSE
  )
  if (length(bindings_names[identical_binding_mask]) == 0) {
    stop("Object not found in the environment")
  }
  bindings_names[identical_binding_mask]
}
