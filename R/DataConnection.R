#' A \code{DataConnection} class of objects
#'
#' Objects of this class store the connection to a data source.
#' It can be a database or server (\code{RICE} or \code{SAICE}) connection.
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
#' x$get_open_call() # check reproducible R code
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
    #' @param if_conn_obj optional, (\code{logical}) whether to store \code{conn} object returned from opening
    #'   connection
    #' @return new \code{DataConnection} object
    initialize = function(open_fun = NULL, close_fun = NULL, ping_fun = NULL, if_conn_obj = FALSE) {
      stopifnot(is_logical_single(if_conn_obj))
      if (!is.null(open_fun)) {
        private$set_open_fun(open_fun)
      }
      if (!is.null(close_fun)) {
        private$set_close_fun(close_fun)
      }
      if (!is.null(ping_fun)) {
        private$set_ping_fun(ping_fun)
      }
      private$if_conn_obj <- if_conn_obj
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
      if (isTRUE(private$opened) && isTRUE(private$ping())) {
        return(invisible(NULL))
      } else {
        open_res <- private$open_fun$run(args = args, try = try)
        if (!self$is_open_failed()) {
          private$opened <- TRUE
          if (private$if_conn_obj && !is.null(open_res)) {
            private$conn <- open_res

            if_not_null(private$close_fun, private$close_fun$assign_to_env("conn", private$conn))
            if_not_null(private$ping_fun, private$ping_fun$assign_to_env("conn", private$conn))
          }
        } else {
          private$opened <- FALSE
          private$conn <- NULL
        }

        return(invisible(self))
      }
    },

    #' @description
    #' Get internal connection object
    #' @return \code{connection} object
    get_conn = function() {
      return(private$conn)
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
      open_call <- private$open_fun$get_call(deparse = FALSE, args = args)

      if (private$if_conn_obj) {
        open_call <- call("<-", as.name("conn"), open_call)
      }

      if (isTRUE(deparse)) {
        return(pdeparse(open_call))
      } else {
        return(open_call)
      }
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
    #' @return \code{self} invisibly for chaining.
    set_open_args = function(args, silent = FALSE) {
      stopifnot(is.null(args) || (is.list(args) && is_fully_named_list(args)))
      if_cond(private$check_open_fun(silent = silent), return(), isFALSE)
      private$open_fun$set_args(args)

      return(invisible(self))
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
    #' @return \code{self} invisibly for chaining.
    set_open_server = function(open_module) {
      stopifnot(is(open_module, "function"))
      stopifnot(names(formals(open_module)) %in% c("input", "output", "session", "connection"))

      private$open_server <- function(input, output, session, connection) {
        withProgress(message = "Opening connection", value = 1, {
          callModule(open_module, id = "open_conn", connection = connection)
        })
      }

      return(invisible(self))
    },
    #' @description
    #' Set open connection UI function
    #'
    #' @param open_module (\code{function})\cr
    #'  shiny module as function. Inputs specified in this \code{ui} are passed to server module
    #'  defined by \code{set_open_server} method.
    #'
    #' @return \code{self} invisibly for chaining.
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

      return(invisible(self))
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
      if (isTRUE(private$ping())) {
        close_res <- private$close_fun$run(try = try)
        if (is(close_res, "try-error")) {
          return(close_res)
        } else {
          private$opened <- FALSE
          private$conn <- NULL
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
    #' @return \code{self} invisibly for chaining.
    set_close_args = function(args, silent = FALSE) {
      stopifnot(is.null(args) || (is.list(args) && is_fully_named_list(args)))
      if_cond(private$check_close_fun(silent = silent), return(), isFALSE)
      private$close_fun$set_args(args)

      return(invisible(self))
    },

    #' @description
    #' Set close connection UI function
    #'
    #' @param close_module (\code{function})\cr
    #'  shiny module as function. Inputs specified in this \code{ui} are passed to server module
    #'  defined by \code{set_close_server} method.
    #'
    #' @return \code{self} invisibly for chaining.
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
      return(invisible(self))
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
    #' @return \code{self} invisibly for chaining.
    set_close_server = function(close_module) {
      stopifnot(is(close_module, "function"))
      stopifnot(names(formals(close_module)) %in% c("input", "output", "session", "connection"))

      private$close_server <- function(input, output, session, connection) {
        withProgress(message = "Closing connection", value = 1, {
          callModule(close_module, id = "close_conn", connection = connection)
        })
      }
      return(invisible(self))
    }
  ),
  # DataConnection private ----
  private = list(
    # callableFunctions
    open_fun = NULL,
    close_fun = NULL,
    ping_fun = NULL,

    # connection object
    if_conn_obj = FALSE,
    conn = NULL,

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
    # @return \code{self} invisibly for chaining.
    set_close_fun = function(fun) {
      stopifnot(is(fun, "CallableFunction"))
      private$close_fun <- fun
      return(invisible(self))
    },
    # @description
    # Set open connection function
    #
    # @param fun (\code{CallableFunction}) function to open connection
    #
    # @return \code{self} invisibly for chaining.
    set_open_fun = function(fun) {
      stopifnot(is(fun, "CallableFunction"))
      private$open_fun <- fun
      return(invisible(self))
    },
    # @description
    # Set a ping function
    #
    # @param fun (\code{CallableFunction}) function to ping connection
    #
    # @return \code{self} invisibly for chaining.
    set_ping_fun = function(fun) {
      stopifnot(is(fun, "CallableFunction"))
      private$ping_fun <- fun
      return(invisible(self))
    },
    # @description
    # Ping the connection.
    #
    # @return logical
    ping = function() {
      if (!is.null(private$ping_fun)) {
        ping_res <- private$ping_fun$run()
        return(isTRUE(ping_res))
      } else {
        return(invisible(NULL))
      }
    }
  )
)

# DataConnection wrappers ----
#' Open connection to \code{random.cdisc.data}
#'
#' @md
#' @description `r lifecycle::badge("experimental")`
#'
#' @param open_args optional, named (\code{list}) of additional parameters for \code{\link{library}} open
#'   function such as \code{quietly}. Please note that the \code{package} argument will be overwritten
#'   with \code{random.cdisc.data}.
#'
#' @return \code{DataConnection} type of object.
#'
#' @export
rcd_connection <- function(open_args = list()) {

  check_pkg_quietly("random.cdisc.data", "random.cdisc.data package not available.")

  stopifnot(is_fully_named_list(open_args))
  stopifnot(all(names(open_args) %in% names(formals(library))))

  open_fun <- callable_function(library)

  open_args$package <- "random.cdisc.data"
  open_fun$set_args(open_args)

  x <- DataConnection$new(open_fun = open_fun)
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
#' @md
#' @description `r lifecycle::badge("experimental")`
#'
#' @param open_args optional, named (\code{list}) of additional parameters for the connection's
#'   \code{\link[rice]{rice_session_open}} open function. Please note that the \code{password} argument will be
#'   overwritten with \code{askpass::askpass}.
#' @param close_args optional, named (\code{list}) of additional parameters for the connection's
#'   \code{\link[rice]{rice_session_close}} close function. Please note that the \code{message} argument
#'   will be overwritten with \code{FALSE}.
#' @param ping_args optional, named (\code{list}) of additional parameters for the connection's
#'   \code{\link[rice]{rice_session_active}} ping function.
#'
#' @return \code{DataConnection} type of object
#'
#' @export
rice_connection <- function(open_args = list(), close_args = list(), ping_args = list()) {
  check_pkg_quietly(
    "rice",
    paste0(
      "Connection to entimICE via rice was requested, but rice package is not available.",
      "Please install it from https://github.roche.com/Rpackages/rice."
    )
  )

  stopifnot(is_fully_named_list(open_args))
  stopifnot(is_fully_named_list(close_args))
  stopifnot(is_fully_named_list(ping_args))

  ping_fun <- callable_function("rice::rice_session_active")
  ping_fun$set_args(ping_args)

  open_fun <- callable_function("rice::rice_session_open")
  open_args$password <- as.call(parse(text = "askpass::askpass"))
  open_fun$set_args(open_args)

  close_fun <- callable_function("rice::rice_session_close")
  close_args$message <- FALSE
  close_fun$set_args(close_args)

  x <- DataConnection$new(open_fun = open_fun,
                          close_fun = close_fun,
                          ping_fun = ping_fun)

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
          paste(
            "Error opening connection\nError message: ",
            conditionMessage(attr(connection$get_open_error_message(), "condition"))
          )
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
          paste(
            "Error closing connection\nError message: ",
            conditionMessage(attr(connection$get_close_error_message(), "condition"))
          )
        )
      }
    }
  )

  return(x)
}




#' Open connection to \code{Teradata}
#'
#' @md
#' @description `r lifecycle::badge("experimental")`
#'
#' @param open_args optional, named (\code{list}) of additional parameters for the connection's
#'   \code{RocheTeradata::connect_teradata} open function. Please note that the \code{type}
#'   argument will be overwritten with \code{ODBC}.
#' @param close_args optional, named (\code{list}) of additional parameters for the connection's
#'   \code{\link[DBI]{dbDisconnect}} close function.
#' @param ping_args optional, named (\code{list}) of additional parameters for the connection's
#'   \code{\link[DBI]{dbIsValid}} ping function.
#'
#' @return \code{DataConnection} type of object
#'
#' @importFrom shinyjs alert
#' @export
teradata_connection <- function(open_args = list(), close_args = list(), ping_args = list()) {
  check_pkg_quietly(
    "RocheTeradata",
    "Connection to Teradata was requested, but RocheTeradata package is not available."
  )
  check_pkg_quietly(
    "DBI",
    "Connection to Teradata was requested, but RocheTeradata package is not available."
  )


  stopifnot(is_fully_named_list(open_args))
  stopifnot(is_fully_named_list(close_args))
  stopifnot(is_fully_named_list(ping_args))

  open_fun <- callable_function("RocheTeradata::connect_teradata")
  open_args$type <- "ODBC"
  open_fun$set_args(open_args)

  close_fun <- callable_function("DBI::dbDisconnect")
  close_args$conn <- as.name("conn")
  close_fun$set_args(close_args)

  ping_fun <- callable_function("DBI::dbIsValid")
  ping_args$dbObj <- as.name("conn") # nolint
  ping_fun$set_args(ping_args)

  x <- DataConnection$new(open_fun = open_fun,
                          close_fun = close_fun,
                          ping_fun = ping_fun,
                          if_conn_obj = TRUE)

  # open connection
  x$set_open_ui(
    function(id) {
      ns <- NS(id)
      div(
        textInput(ns("username"), "uid"),
        passwordInput(ns("password"), "pwd")
      )
    }
  )

  x$set_open_server(
    function(input, output, session, connection) {
      connection$open(args = list(uid = input$username,
                                  pwd = input$password),
                      try = TRUE)

      if (connection$is_open_failed()) {
        shinyjs::alert(
          paste(
            "Error opening connection\nError message: ",
            conditionMessage(attr(connection$get_open_error_message(), "condition"))
          )
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
          paste(
            "Error closing connection\nError message: ",
            conditionMessage(attr(connection$get_close_error_message(), "condition"))
          )
        )
      }
    }
  )

  return(x)
}
