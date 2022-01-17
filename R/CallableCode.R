## CallableCode ====
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @title A \code{CallableCode} class of objects
#'
#' Object that stores code to reproduce an object. It includes methods to
#' get or run the code and return the object.
#'
CallableCode <- R6::R6Class( # nolint
  "CallableCode",
  inherit = Callable,

  ## __Public Methods ====
  public = list(
    #' @description
    #'  Create a new \code{CallableCode} object
    #'
    #' @param code (\code{character})\cr
    #'  a string containing R code to reproduce the desired object.
    #' @param env (\code{environment})\cr
    #'  environment where function will be evaluated
    #'
    #' @return new \code{CallableCode} object
    initialize = function(code, env = new.env(parent = parent.env(globalenv()))) {
      if (!checkmate::test_string(code)) {
        stop("A string of length one containing the code needed to produce the object must be provided.")
      }

      # reposition all library calls in the code so that they are
      # visible in the new env
      env$library <- function(...) {
        mc <- match.call()
        mc[[1]] <- quote(base::library)
        eval(mc, envir = globalenv())
        this_env <- parent.frame()

        if (!identical(this_env, globalenv())) {
          parent.env(this_env) <- parent.env(globalenv())
        }
      }

      super$initialize(env = env)

      private$code <- code
      private$call <- private$get_callable_code(code)
      logger::log_trace("CallableCode initialized.")

      return(invisible(self))
    },
    #' @description
    #'  Get sequence of calls from the code supplied to produce the object.
    #'
    #' @param deparse (\code{logical} value)\cr
    #'  whether to return a deparsed version of call
    #' @param args (\code{NULL})\cr
    #'  available to be consistent with \code{CallableFunction} but are not used to
    #'  retrieve the call.
    #'
    #' @return \code{list} of \code{calls} or \code{character} depending on \code{deparse} argument
    get_call = function(deparse = TRUE, args = NULL) {
      checkmate::assert_flag(deparse)
      if (!is.null(args)) {
        stop("'args' are not used to retrieve the call.")
      }

      res <- if (deparse) {
        paste0(vapply(private$call, deparse1, character(1)), collapse = "\n")
      } else {
        private$call
      }

      return(res)
    }
  ),

  ## __Private Fields ====
  private = list(
    code = NULL,
    ## __Private Methods ====
    # @description
    #  Determines whether code is valid and callable. If not then stores error message.
    #
    # @param code \code{character} string to check
    #
    # @return \code{expression} parsed from the supplied code
    #
    get_callable_code = function(code) {
      expr <- tryCatch(
        str2expression(code),
        error = function(e) {
          private$error_msg <- e$message
          private$failed <- TRUE
        }
      )
      if (length(expr) >= 1 && !private$failed) {
        return(expr)
      } else {
        stop(paste("Code supplied is not valid:", private$error_msg))
      }
    }
  )
)

## Constructors ====

#' Create \code{\link{CallableCode}} object
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Create \link{CallableCode} object to execute specific code and get reproducible call.
#'
#' @param code (\code{character})\cr
#'   a string containing R code to reproduce the desired object. Please be aware
#'   that objects assigned to temporary environment are locked which means
#'   that they can't be modified.
#'
#' @return \code{CallableCode} object
#'
#' @export
#'
#' @examples
#' cf <- callable_code(code = "mtcars")
#' cf$run()
#' cf$get_call()
callable_code <- function(code) {
  CallableCode$new(code)
}
