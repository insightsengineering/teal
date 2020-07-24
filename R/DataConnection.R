#' A \code{DataConnection} class of objects
#'
#' Objects of this class store the connection to a data source.
#' It can be a database or server (\code{RICE} or \code{SAICE}) connection.
#'
#' @name DataConnection
#'
#' @examples
#' \dontrun{
#' open_fun <- callable_function(data.frame) # define opening function
#' open_fun$set_args(list(x = 1:5)) # define fixed arguments to opening function
#'
#' close_fun <- callable_function(print) # define closing function
#' close_fun$set_args(list(x = "Hi there")) # define fixed arguments to closing function
#'
#' x <- DataConnection$new() # define connection
#' x$set_ping_fun(ping_fun) # able to set an optional ping function - eg. rice::rice_session_active
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
#' @importFrom R6 R6Class
#' @importFrom shinyjs alert
DataConnection <- R6::R6Class( # nolint
  # DataConnection public ----
  "DataConnection",
  public = list(
    #' @description
    #' Create a new \code{DataConnection} object
    #'
    #' @param open_fun (\code{CallableFunction}) function to open connection
    #' @param close_fun (\code{CallableFunction}) function to close connection
    #' @param ping_fun (\code{CallableFunction}) function to ping connection
    #' @return new \code{DataConnection} object
    initialize = function(open_fun = NULL, close_fun = NULL, ping_fun = NULL) {
      if (!is.null(open_fun)) {
        private$set_open_fun(open_fun)
      }
      if (!is.null(close_fun)) {
        private$set_close_fun(close_fun)
      }
      if (!is.null(ping_fun)) {
        private$set_close_fun(ping_fun)
      }
    },
    #' @description
    #' If connection is opened
    #'
    #' If open connection has been successfully evaluated
    #'
    #' @return \code{logical} if connection is open
    is_opened = function() {
      return(private$opened)
    },
    #' @description
    #' Check if connection has not failed.
    #'
    #' @return \code{TRUE} if connection failed, else \code{FALSE}
    is_failed = function() {
      self$is_open_failed() || self$is_close_failed()
    },
    # .. open connection -----
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
      if (isTRUE(self$ping())) {
        return(invisible(NULL))
      } else {
        open_res <- private$open_fun$run(args = args, try = try)
        if (!self$is_open_failed()) {
          private$opened <- TRUE
        }

        return(invisible(self))
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
    #' Get error message from last connection
    #'
    #' @return \code{try-error} object with error message or \code{character(0)} if last
    #'  connection was successful.
    get_open_error_message = function() {
      return(private$open_fun$get_error_message())
    },
    #' @description
    #' Get shiny server module to open connection.
    #'
    #' @return the \code{server} \code{function} to open connection.
    get_open_server = function() {
      return(private$open_server)
    },
    #' @description
    #' Get Shiny module with inputs to open connection
    #'
    #' @param id \code{character} shiny element id
    #'
    #' @return the \code{ui} function to set arguments to open connection function.
    get_open_ui = function(id) {
      return(private$open_ui(id))
    },
    #' @description
    #' Check if open connection has not failed.
    #'
    #' @return \code{TRUE} if open connection failed, else \code{FALSE}
    is_open_failed = function() {
      if (!is.null(private$open_fun)) {
        private$open_fun$is_failed()
      } else {
        FALSE
      }
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
    #' Set open-connection server function
    #'
    #' This function will be called after submit button will be hit. There is no possibility to
    #' specify some dynamic \code{ui} as \code{server} function is executed after hitting submit
    #' button.
    #'
    #' @param open_module (\code{function})\cr
    #'  A shiny module server function that should load data from all connectors
    #'
    #' @return nothing
    set_open_server = function(open_module) {
      stopifnot(is(open_module, "function"))
      stopifnot(names(formals(open_module)) %in% c("input", "output", "session", "connection"))

      private$open_server <- function(input, output, session, connection) {
        withProgress(message = "Opening connection", value = 1, {
          callModule(open_module, id = "open_conn", connection = connection)
        })
      }

      return(invisible(NULL))
    },
    #' @description
    #' Set open connection UI function
    #'
    #' @param open_module (\code{function})\cr
    #'  shiny module as function. Inputs specified in this \code{ui} are passed to server module
    #'  defined by \code{set_open_server} method.
    #'
    #' @return nothing
    set_open_ui = function(open_module) {
      stopifnot(is(open_module, "function"))
      stopifnot(identical(names(formals(open_module)), "id"))

      private$open_ui <- function(id) {
        ns <- NS(id)
        tags$div(
          tags$div(
            id = ns("open_conn"),
            open_module(id = ns("open_conn"))
          )
        )
      }

      return(invisible(NULL))
    },
    # .. close connection -------
    #' @description
    #' Close the connection.
    #'
    #' @param silent (\code{logical}) whether convert all "missing function" errors to messages
    #' @param try (\code{logical}) whether perform function evaluation inside \code{try} clause
    #'
    #' @return if \code{try = TRUE} then \code{try-error} on error, \code{NULL} otherwise
    close = function(silent = FALSE, try = FALSE) {
      if_cond(private$check_close_fun(silent = silent), return(), isFALSE)
      if (isTRUE(self$ping())) {
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
    #' Get error message from last connection
    #'
    #' @return \code{try-error} object with error message or \code{character(0)} if last
    #'  connection was successful.
    get_close_error_message = function() {
      return(private$close_fun$get_error_message())
    },
    #' @description
    #' Get shiny server module to close connection.
    #'
    #' @return the \code{server} \code{function} to close connection.
    get_close_server = function() {
      return(private$close_server)
    },
    #' @description
    #' Get Shiny module with inputs to close connection
    #'
    #' @param id \code{character} shiny element id
    #'
    #' @return the \code{ui} function to set arguments to close connection function.
    get_close_ui = function(id) {
      return(private$close_ui(id))
    },
    #' @description
    #' Check if close connection has not failed.
    #'
    #' @return \code{TRUE} if close connection failed, else \code{FALSE}
    is_close_failed = function() {
      if (!is.null(private$close_fun)) {
        private$close_fun$is_failed()
      } else {
        FALSE
      }
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
    #' Set close connection UI function
    #'
    #' @param close_module (\code{function})\cr
    #'  shiny module as function. Inputs specified in this \code{ui} are passed to server module
    #'  defined by \code{set_close_server} method.
    #'
    #' @return nothing
    set_close_ui = function(close_module) {
      stopifnot(is(close_module, "function"))
      stopifnot(identical(names(formals(close_module)), "id"))

      private$close_ui <- function(id) {
        ns <- NS(id)
        tags$div(
          tags$div(
            id = ns("close_conn"),
            close_module(id = ns("close_conn"))
          )
        )
      }
      return(invisible(NULL))
    },

    #' @description
    #' Set close-connection server function
    #'
    #' This function will be called after submit button will be hit. There is no possibility to
    #' specify some dynamic \code{ui} as \code{server} function is executed after hitting submit
    #' button.
    #'
    #' @param close_module (\code{function})\cr
    #'  A shiny module server function that should load data from all connectors
    #'
    #' @return nothing
    set_close_server = function(close_module) {
      stopifnot(is(close_module, "function"))
      stopifnot(names(formals(close_module)) %in% c("input", "output", "session", "connection"))

      private$close_server <- function(input, output, session, connection) {
        withProgress(message = "Closing connection", value = 1, {
          callModule(close_module, id = "close_conn", connection = connection)
        })
      }
      return(invisible(NULL))
    },
    #' @description
    #' ping the connection.
    #'
    #' @return logical
    ping = function() {
      if (!is.null(private$ping_fun)) {
        ping_res <- private$ping_fun$run()
        return(isTRUE(ping_res))
      } else {
        return(invisible(NULL))
      }
    }
  ),
  # DataConnection private ----
  private = list(
    # callableFunctions
    open_fun = NULL,
    close_fun = NULL,
    ping_fun = NULL,

    # shiny elements
    open_ui = NULL,
    close_ui = NULL,
    ping_ui = NULL,
    open_server = NULL,
    close_server = NULL,
    ping_server = NULL,

    #
    check_open_fun = function(silent = FALSE) {
      stopifnot(is_logical_single(silent))

      if (is.null(private$open_fun)) {
        msg <- "Open connection function not set"
        if (silent) {
          return(FALSE)
        } else {
          stop(msg)
        }
      } else {
        return(TRUE)
      }
    },
    check_close_fun = function(silent = FALSE) {
      stopifnot(is_logical_single(silent))

      if (is.null(private$close_fun)) {
        msg <- "Close connection function not set"
        if (silent) {
          return(FALSE)
        } else {
          stop(msg)
        }
      } else {
        return(TRUE)
      }
    },
    opened = FALSE,
    # @description
    # Set close connection function
    #
    # @param fun (\code{CallableFunction}) function to close connection
    #
    # @return nothing
    set_close_fun = function(fun) {
      stopifnot(is(fun, "CallableFunction"))
      private$close_fun <- fun
      return(invisible(NULL))
    },
    # @description
    # Set open connection function
    #
    # @param fun (\code{CallableFunction}) function to open connection
    #
    # @return nothing
    set_open_fun = function(fun) {
      stopifnot(is(fun, "CallableFunction"))
      private$open_fun <- fun
      return(invisible(NULL))
    },
    # @description
    # Set a ping function
    #
    # @param fun (\code{CallableFunction}) function to ping connection
    #
    # @return nothing
    set_ping_fun = function(fun) {
      stopifnot(is(fun, "CallableFunction"))
      private$ping_fun <- fun
      return(invisible(NULL))
    }
  )
)

# DataConnection wrappers ----
#' Open connection to \code{random.cdisc.data}
#'
#' @return \code{DataConnection} type of object
rcd_connection <- function() {
  check_pkg_quietly("random.cdisc.data", "random.cdisc.data package not available.") # nolint

  fun <- callable_function(library) # nolint
  fun$set_args(list(package = "random.cdisc.data"))

  x <- DataConnection$new(open_fun = fun)
  x$set_open_ui(
    function(id) {
      NULL
    }
  )

  x$set_open_server(
    function(input, output, session, connection) {
      NULL
    }
  )
  return(x)
}


#' Open connection to \code{rice}
#'
#' @return \code{DataConnection} type of object
rice_connection <- function() {
  check_pkg_quietly(
    "rice",
    paste0(
      "Connection to entimICE via rice was requested, but rice package is not available.",
      "Please install it from https://github.roche.com/Rpackages/rice."
    )
  )

  ping_fun <- callable_function(rice::rice_session_active) # nolint
  open_fun <- callable_function(rice::rice_session_open) # nolint
  open_fun$set_args(list(password = as.call(parse(text = "askpass::askpass"))))

  close_fun <- callable_function(rice::rice_session_close) # nolint
  close_fun$set_args(list(message = FALSE))

  x <- DataConnection$new(open_fun = open_fun,
                          close_fun = close_fun,
                          ping_fun = ping_fun) # nolint

  # open connection
  x$set_open_ui(
    function(id) {
      ns <- NS(id)
      div(
        textInput(ns("username"), "Username"),
        passwordInput(ns("password"), "Password")
      )
    }
  )

  x$set_open_server(
    function(input, output, session, connection) {
      connection$open(args = list(username = input$username,
                                  password = input$password),
                      try = TRUE)

      if (connection$is_open_failed()) {
        shinyjs::alert(
          paste("Error opening connection\nError message:", connection$get_open_error_message())
        )
      }
    }
  )

  # close connection
  x$set_close_ui(
    function(id) {
      NULL
    }
  )

  x$set_close_server(
    function(input, output, session, connection) {
      connection$close(try = TRUE)

      if (connection$is_close_failed()) {
        shinyjs::alert(
          paste("Error closing connection\nError message:", connection$get_close_error_message())
        )
      }
    }
  )

  return(x)
}
