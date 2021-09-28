## DataConnection ====
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @title A \code{DataConnection} class of objects
#'
#' Objects of this class store the connection to a data source.
#' It can be a database or server (\code{RICE} or \code{SAICE}) connection.
#'
#' @examples
#' open_fun <- callable_function(data.frame) # define opening function
#' open_fun$set_args(list(x = 1:5)) # define fixed arguments to opening function
#'
#' close_fun <- callable_function(print) # define closing function
#' close_fun$set_args(list(x = "Hi there")) # define fixed arguments to closing function
#'
#' ping_fun <- callable_function(function() TRUE)
#'
#' x <- teal:::DataConnection$new( # define connection
#'   ping_fun = ping_fun, # define ping function
#'   open_fun = open_fun, # define opening function
#'   close_fun = close_fun) # define closing function
#'
#' x$set_open_args(args = list(y = letters[1:5])) # define additional arguments if necessary
#'
#' x$open() # call opening function
#' x$get_open_call() # check reproducible R code
#'
#' # get data from connection via DataConnector$get_dataset()
#'
#' \dontrun{
#' x$open(args = list(x = 1:5, y = letters[1:5])) # able to call opening function with arguments
#' x$close() # call closing function
#' }
#'
DataConnection <- R6::R6Class( # nolint
  ## __Public Methods ====
  "DataConnection",
  public = list(
    #' @description
    #' Create a new `DataConnection` object
    #'
    #' @param open_fun (`CallableFunction`) function to open connection
    #' @param close_fun (`CallableFunction`) function to close connection
    #' @param ping_fun (`CallableFunction`) function to ping connection
    #' @param if_conn_obj optional, (`logical`) whether to store `conn` object returned from opening
    #'   connection
    #' @return new `DataConnection` object
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

      private$open_ui <- function(id) {
        NULL
      }
      private$ping_ui <- function(id) {
        NULL
      }
      private$close_ui <- function(id) {
        NULL
      }
    },
    #' @description
    #' Finalize method closing the connection.
    #'
    #' @return NULL
    finalize = function() {
      self$close(silent = TRUE, try = TRUE)
      return(NULL)
    },
    #' @description
    #' If connection is opened
    #'
    #' If open connection has been successfully evaluated
    #'
    #' @return (`logical`) if connection is open
    is_opened = function() {
      return(private$opened)
    },
    #' @description
    #' Check if connection has not failed.
    #'
    #' @return (`logical`) `TRUE` if connection failed, else `FALSE`
    is_failed = function() {
      self$is_open_failed() || self$is_close_failed()
    },
    #' @description
    #' Run simple application that uses its \code{ui} and \code{server} fields to open the
    #' connection.
    #'
    #' Useful for debugging
    #'
    #' @return An object that represents the app
    launch = function() {
      shinyApp(
        ui = fluidPage(
          include_teal_css_js(),
          shinyjs::useShinyjs(),
          fluidRow(
            column(
              width = 8,
              offset = 2,
              tags$div(
                id = "connection_inputs",
                self$get_open_ui(id = "data_connection"),
                actionButton("submit", "Submit"),
                `data-proxy-click` = "submit"
              ),
              shinyjs::hidden(
                tags$div(
                  id = "connection_set",
                  div(
                    h3("Connection successfully set."),
                    p("You can close this window and get back to R console.")
                  )
                )
              )
            )
          )
        ),
        server = function(input, output, session) {
          session$onSessionEnded(stopApp)
          callModule(
            self$get_preopen_server(),
            id = "data_connection",
            connection = self
          )
          observeEvent(input$submit, {
            rv <- reactiveVal(NULL)
            rv(
              callModule(
                self$get_open_server(),
                id = "data_connection",
                connection = self
              )
            )

            observeEvent(rv(), {
              if (self$is_opened()) {
                removeUI(sprintf("#%s", session$ns("connection_inputs")))
                shinyjs::show("connection_set")
                stopApp()
              }
            })

          })
        }
      )
    },
    # ___ open connection -----
    #' @description
    #' Open the connection.
    #'
    #' Note that if the connection is already opened then it does nothing.
    #'
    #' @param args (`NULL` or named `list`) additional arguments not set up previously
    #' @param silent (`logical`) whether convert all "missing function" errors to messages
    #' @param try (`logical`) whether perform function evaluation inside `try` clause
    #'
    #' @return returns `self` if successful or if connection has been already
    #' opened. If `open_fun` fails, app returns an error in form of
    #' `shinyjs::alert` (if `try = TRUE`) or breaks the app (if `try = FALSE`)
    #'
    open = function(args = NULL, silent = FALSE, try = FALSE) {
      stopifnot(is.null(args) || (is.list(args) && is_fully_named_list(args)))
      if_cond(private$check_open_fun(silent = silent), return(), isFALSE)
      if (isTRUE(private$opened) && isTRUE(private$ping())) {
        return(invisible(self))
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
    #'
    #' @return `connection` object
    get_conn = function() {
      return(private$conn)
    },
    #' @description
    #' Get executed open connection call
    #'
    #' @param deparse (`logical`) whether return deparsed form of a call
    #' @param args (`NULL` or named `list`) additional arguments not set up previously
    #' @param silent (`logical`) whether convert all "missing function" errors to messages
    #'
    #' @return optionally deparsed `call` object
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
    #' @return (`character`)\cr
    #'  text of the error message or `character(0)` if last
    #'  connection was successful.
    get_open_error_message = function() {
      return(private$open_fun$get_error_message())
    },
    #' @description
    #' Get shiny server module prior opening connection.
    #'
    #' @return (`function`) shiny server prior opening connection.
    get_preopen_server = function() {
      return(private$preopen_server)
    },
    #' @description
    #' Get shiny server module to open connection.
    #'
    #' @return (`function`) shiny server to open connection.
    get_open_server = function() {
      return(private$open_server)
    },
    #' @description
    #' Get Shiny module with inputs to open connection
    #'
    #' @param id `character` shiny element id
    #'
    #' @return (`function`) shiny ui to set arguments to open connection function.
    get_open_ui = function(id) {
      return(private$open_ui(id))
    },
    #' @description
    #' Check if open connection has not failed.
    #'
    #' @return (`logical`) `TRUE` if open connection failed, else `FALSE`
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
    #' @param args (`NULL` or named `list`) with values where list names are argument names
    #' @param silent (`logical`) whether convert all "missing function" errors to messages
    #'
    #' @return (`self`) invisibly for chaining.
    set_open_args = function(args, silent = FALSE) {
      stopifnot(is.null(args) || (is.list(args) && is_fully_named_list(args)))
      if_cond(private$check_open_fun(silent = silent), return(), isFALSE)
      private$open_fun$set_args(args)

      return(invisible(self))
    },
    #' @description
    #' Set pre-open connection server function
    #'
    #' This function will be called before submit button will be hit.
    #'
    #' @param preopen_module (`function`)\cr
    #'  A shiny module server function
    #'
    #' @return (`self`) invisibly for chaining.
    set_preopen_server = function(preopen_module) {
      stopifnot(is(preopen_module, "function"))
      stopifnot(names(formals(preopen_module)) %in% c("input", "output", "session", "connection"))

      private$preopen_server <- function(input, output, session, connection) {
        callModule(preopen_module, id = "open_conn", connection = connection)
      }

      return(invisible(self))
    },
    #' @description
    #' Set open connection server function
    #'
    #' This function will be called after submit button will be hit. There is no possibility to
    #' specify some dynamic `ui` as `server` function is executed after hitting submit
    #' button.
    #'
    #' @param open_module (`function`)\cr
    #'  A shiny module server function that should load data from all connectors
    #'
    #' @return (`self`) invisibly for chaining.
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
    #' @param open_module (`function`)\cr
    #'  shiny module as function. Inputs specified in this `ui` are passed to server module
    #'  defined by `set_open_server` method.
    #'
    #' @return (`self`) invisibly for chaining.
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
    # ___ close connection -------
    #' @description
    #' Close the connection.
    #'
    #' @param silent (`logical`) whether convert all "missing function" errors to messages
    #' @param try (`logical`) whether perform function evaluation inside `try` clause
    #'
    #' @return returns (`self`) if successful. For unsuccessful evaluation it
    #' depends on `try` argument: if `try = TRUE` then returns
    #' `error`, for `try = FALSE` otherwise
    close = function(silent = FALSE, try = FALSE) {
      if_cond(private$check_close_fun(silent = silent), return(), isFALSE)
      close_res <- private$close_fun$run(try = try)
      if (is(close_res, "error")) {
        return(close_res)
      } else {
        private$opened <- FALSE
        private$conn <- NULL
        return(invisible(NULL))
      }
    },
    #' @description
    #' Get executed close connection call
    #'
    #' @param deparse (`logical`) whether return deparsed form of a call
    #' @param silent (`logical`) whether convert all "missing function" errors to messages
    #'
    #' @return optionally deparsed `call` object
    get_close_call = function(deparse = TRUE, silent = FALSE) {
      stopifnot(is_logical_single(deparse))
      if_cond(private$check_close_fun(silent = silent), return(), isFALSE)
      private$close_fun$get_call(deparse = deparse)
    },
    #' @description
    #' Get error message from last connection
    #'
    #' @return (`character`)\cr
    #'  text of the error message or `character(0)` if last
    #'  connection was successful.
    get_close_error_message = function() {
      return(private$close_fun$get_error_message())
    },
    #' @description
    #' Get shiny server module to close connection.
    #'
    #' @return the `server function` to close connection.
    get_close_server = function() {
      return(private$close_server)
    },
    #' @description
    #' Check if close connection has not failed.
    #'
    #' @return (`logical`) `TRUE` if close connection failed, else `FALSE`
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
    #' @param args (named `list`) with values where list names are argument names
    #' @param silent (`logical`) whether convert all "missing function" errors to messages
    #'
    #' @return (`self`) invisibly for chaining.
    set_close_args = function(args, silent = FALSE) {
      stopifnot(is.null(args) || (is.list(args) && is_fully_named_list(args)))
      if_cond(private$check_close_fun(silent = silent), return(), isFALSE)
      private$close_fun$set_args(args)

      return(invisible(self))
    },

    #' @description
    #' Set close connection UI function
    #'
    #' @param close_module (`function`)\cr
    #'  shiny module as function. Inputs specified in this `ui` are passed to server module
    #'  defined by `set_close_server` method.
    #'
    #' @return (`self`) invisibly for chaining.
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
    #' specify some dynamic `ui` as `server` function is executed after hitting submit
    #' button.
    #'
    #' @param close_module (`function`)\cr
    #'  A shiny module server function that should load data from all connectors
    #'
    #' @return (`self`) invisibly for chaining.
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
  ## __Private Fields ====
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
    preopen_server = NULL,
    open_server = NULL,
    close_server = NULL,
    ping_server = NULL,

    opened = FALSE,

    ## __Private Methods ====
    # need to have a custom deep_clone because one of the key fields are reference-type object
    # in particular: open_fun is a R6 object that wouldn't be cloned using default clone(deep = T)
    deep_clone = function(name, value) {
      deep_clone_r6(name, value)
    },

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
    # @description
    # Set close connection function
    #
    # @param fun (`Callable`) function to close connection
    #
    # @return (`self`) invisibly for chaining.
    set_close_fun = function(fun) {
      stopifnot(is(fun, "Callable"))
      private$close_fun <- fun
      return(invisible(self))
    },
    # @description
    # Set open connection function
    #
    # @param fun (`Callable`) function to open connection
    #
    # @return (`self`) invisibly for chaining.
    set_open_fun = function(fun) {
      stopifnot(is(fun, "Callable"))
      private$open_fun <- fun
      return(invisible(self))
    },
    # @description
    # Set a ping function
    #
    # @param fun (`Callable`) function to ping connection
    #
    # @return (`self`) invisibly for chaining.
    set_ping_fun = function(fun) {
      stopifnot(is(fun, "Callable"))
      private$ping_fun <- fun
      return(invisible(self))
    },
    # @description
    # Ping the connection.
    #
    # @return (`logical`)
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
#' Open connection to `random.cdisc.data`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param open_args optional, named (`list`) of additional parameters for \code{\link{library}} open
#'   function such as `quietly.` Please note that the `package` argument will be overwritten
#'   with `random.cdisc.data`.
#'
#' @return (`DataConnection`) type of object.
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

  # open connection
  x$set_open_server(
    function(input, output, session, connection) {
      connection$open(try = TRUE)

      if (connection$is_open_failed()) {
        shinyjs::alert(
          paste(
            "Error opening rcd connection\nError message: ",
            connection$get_open_error_message()
          )
        )
      }

      # we want the connection closed (if it's opened) when the user shiny session ends
      session$onSessionEnded(function() {
        suppressWarnings(connection$close(silent = TRUE, try = TRUE))
      })

      return(invisible(connection))
    }
  )

  return(x)
}

#' Open connection to `rice`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param open_args optional, named (`list`) of additional parameters for the connection's
#'   \code{rice_session_open} open function. Please note that the `password` argument will be
#'   overwritten with `askpass::askpass`.
#' @param close_args optional, named (`list`) of additional parameters for the connection's
#'   \code{rice_session_close} close function. Please note that the `message` argument
#'   will be overwritten with `FALSE`.
#' @param ping_args optional, named (`list`) of additional parameters for the connection's
#'   \code{rice_session_active} ping function.
#'
#' @return (`DataConnection`) type of object
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

  x <- DataConnection$new(open_fun = open_fun, close_fun = close_fun, ping_fun = ping_fun)

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
      connection$open(args = list(username = input$username, password = input$password), try = TRUE)

      if (connection$is_open_failed()) {
        shinyjs::alert(
          paste(
            "Error opening connection\nError message: ",
            connection$get_open_error_message()
          )
        )
      }

      session$onSessionEnded(function() {
        suppressWarnings(connection$close(silent = TRUE, try = TRUE))
      })

      return(invisible(connection))
    }
  )

  # close connection
  x$set_close_server(
    function(input, output, session, connection) {
      connection$close(try = TRUE)

      if (connection$is_close_failed()) {
        shinyjs::alert(
          paste(
            "Error closing connection\nError message: ",
            connection$get_close_error_message()
          )
        )
      }
      return(invisible(connection))
    }
  )

  return(x)
}

#' Open connection to `Teradata`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param open_args optional, named (`list`) of additional parameters for the connection's
#'   `RocheTeradata::connect_teradata` open function. Please note that the `type`
#'   argument will be overwritten with `ODBC`.
#' @param close_args optional, named (`list`) of additional parameters for the connection's
#'   \code{\link[DBI]{dbDisconnect}} close function.
#' @param ping_args optional, named (`list`) of additional parameters for the connection's
#'   \code{\link[DBI]{dbIsValid}} ping function.
#'
#' @return (`DataConnection`) type of object
#'
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

  x <- DataConnection$new(open_fun = open_fun, close_fun = close_fun, ping_fun = ping_fun, if_conn_obj = TRUE)

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
      connection$open(args = list(uid = input$username, pwd = input$password), try = TRUE)

      if (connection$is_open_failed()) {
        shinyjs::alert(
          paste(
            "Error opening connection\nError message: ",
            connection$get_open_error_message()
          )
        )
      }

      session$onSessionEnded(function() {
        suppressWarnings(connection$close(silent = TRUE, try = TRUE))
      })

      return(invisible(connection))
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
            connection$get_close_error_message()
          )
        )
      }
      return(invisible(connection))
    }
  )

  return(x)
}

#' Helper function to connect to `Snowflake`
#'
#' This is used by \code{snowflake_connection} and does not need to be called directly
#' @param username the username used to collect the auth token to connect to snowflake.
#' @param password the password used to collect the auth token to connect to snowflake
#' @param role the user role used to connect to `Snowflake`.
#' @param database the `Snowflake` database to connect to.
#' @param schema the `Snowflake` schema to connect to.
#' @param warehouse the `Snowflake` warehouse to connect to.
#' @param server the `Snowflake`server to connect to.
#' @param port the port to connect to the `Snowflake` instance.
#' @param driver the driver to use to connect to the `Snowflake` instance.
#' @export
snowflake_connection_function <- function(username = askpass::askpass("Please enter your username"),
                                          password = askpass::askpass("Please enter your password"),
                                          role,
                                          database,
                                          schema,
                                          warehouse,
                                          server = "roche_pd.eu-central-1.snowflakecomputing.com",
                                          port = 443,
                                          driver = "SnowflakeDSIIDriver") {

  res <- httr::POST("https://wam.roche.com/as/token.oauth2",
    body = list(
      client_id = "snowflake",
      grant_type = "password",
      username = username,
      password = password,
      client_secret = "snowflake",
      scope = paste0("session:role:", role)
    ),
    encode = "form"
  )

  if (httr::status_code(res) >= 300) {
    stop("Could not create authorization token.
         Is your username and password correct?
         If so please contact the app author", call. = FALSE)
  }

  # we do not need to store token as (for now) it expires after
  # long enough to pull the data in
  token <- httr::content(res)$access_token

  tryCatch(
    con <- eval(parse(text = "DBI::dbConnect(
      odbc::odbc(),
      Server = server,
      port = port,
      Driver = driver,
      database = database,
      schema = schema,
      Warehouse = warehouse,
      role = role,
      authenticator = \"oauth\",
      token = token")),
    error = function(cond){
      stop(paste("Unable to connect to snowflake. Error message:", cond$message), call. = FALSE)
    }
  )
  return(con)
}

#' Open connection to `Snowflake`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param open_args optional, named (`list`) of additional parameters for the connection's
#'   `teal::snowflake_connection_function` open function.
#' @param close_args optional, named (`list`) of additional parameters for the connection's
#'   \code{\link[DBI]{dbDisconnect}} close function.
#' @param ping_args optional, named (`list`) of additional parameters for the connection's
#'   \code{\link[DBI]{dbIsValid}} ping function.
#'
#' @return (`DataConnection`) type of object
#'
#' @export
snowflake_connection <- function(open_args = list(), close_args = list(), ping_args = list()) {
  check_pkg_quietly(
    "httr",
    "Connection to Snowflake was requested, but httr package is not available."
  )
  check_pkg_quietly(
    "DBI",
    "Connection to Snowflake was requested, but DBI package is not available."
  )
  check_pkg_quietly(
    "odbc",
    "Connection to Snowflake was requested, but odbc package is not available."
  )

  stopifnot(is_fully_named_list(open_args))
  stopifnot(is_fully_named_list(close_args))
  stopifnot(is_fully_named_list(ping_args))

  open_fun <- callable_function("teal::snowflake_connection_function")
  open_fun$set_args(open_args)

  close_fun <- callable_function("DBI::dbDisconnect")
  close_args$conn <- as.name("conn")
  close_fun$set_args(close_args)

  ping_fun <- callable_function("DBI::dbIsValid")
  ping_args$dbObj <- as.name("conn") # nolint
  ping_fun$set_args(ping_args)

  x <- DataConnection$new(open_fun = open_fun, close_fun = close_fun, ping_fun = ping_fun, if_conn_obj = TRUE)

  # open connection
  x$set_open_ui(
    function(id) {
      ns <- NS(id)
      div(
        textInput(ns("username"), "username"),
        passwordInput(ns("password"), "password")
      )
    }
  )

  x$set_open_server(
    function(input, output, session, connection) {
      connection$open(args = list(username = input$username, password = input$password), try = TRUE)

      if (connection$is_open_failed()) {
        shinyjs::alert(
          paste(
            "Error opening connection\nError message: ",
            connection$get_open_error_message()
          )
        )
      }

      session$onSessionEnded(function() {
        suppressWarnings(connection$close(silent = TRUE, try = TRUE))
      })

      return(invisible(connection))
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
            connection$get_close_error_message()
          )
        )
      }
      return(invisible(connection))
    }
  )

  return(x)
}

#' Open connection to `CDSE`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param env optional, `CDSE` environment name.
#'
#' @return (`DataConnection`) type of object
#'
#' @export
cdse_connection <- function(env = "prod") {
  check_pkg_quietly(
    "CDSE",
    "Connection to CDSE was requested, but CDSE package is not available."
  )

  open_call <- callable_code(sprintf("
  if (require(\"shiny\") && is.null(shiny::getDefaultReactiveDomain())) {
    CDSE::cdse_login()
    CDSE::cdse_set_environment(\"%s\")
    Sys.wait(1)
    NULL
  }", env))

  x <- DataConnection$new(open_fun = open_call, if_conn_obj = TRUE)

  # ugly hack to silent CDSE dependency note
  x$.__enclos_env__$private$conn <- eval(parse(text = "CDSE::cdse_connection$new()"))

  # open connection
  x$set_open_ui(
    function(id) {
      ns <- NS(id)
      div(
        eval(parse(text = "CDSE::useCDSElogin()")),
        "Please login to CDSE first before submitting!",
        br(),
        eval(parse(text = "CDSE::cdse_login_button(id = ns(\"cdse_login\"), label = \"Login to CDSE\")"))
      )
    }
  )

  x$set_preopen_server(
    function(input, output, session, connection) {
      conn <- connection$get_conn()
      callModule(module = eval(parse(text = "CDSE::cdse_login_shiny")), id = "cdse_login", env = env, con = conn)

      observeEvent(conn$reactive()(), {
        if (!conn$is_valid()) {
          connection$open()
        }
      })

      session$onSessionEnded(function() {
        suppressWarnings(connection$close(silent = TRUE, try = TRUE))
      })

      return(invisible(connection))
    }
  )

  return(x)
}

#' Open connection to `DataSetDB`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @return (`DataConnection`) type of object
#'
#' @export
datasetdb_connection <- function() {
  check_pkg_quietly(
    "gdbauth",
    "Connection to DataSetDB via gdbauth was requested, but gdbauth package is not available."
  )
  ping_fun <- callable_function("gdbauth::is_active")

  open_fun <- callable_function("gdbauth::login")

  close_fun <- callable_function("gdbauth::logout")

  x <- DataConnection$new(open_fun = open_fun, close_fun = close_fun, ping_fun = ping_fun)

  # open connection
  x$set_open_ui(
    function(id) {
      ns <- NS(id)
      div(
        textInput(ns("user"), "User"),
        passwordInput(ns("password"), "Password")
      )
    }
  )

  x$set_open_server(
    function(input, output, session, connection) {
      connection$open(args = list(user = input$user, password = input$password), try = TRUE)

      if (connection$is_open_failed()) {
        shinyjs::alert(
          paste(
            "Error opening connection\nError message: ",
            connection$get_open_error_message()
          )
        )
      }

      session$onSessionEnded(function() {
        suppressWarnings(connection$close(silent = TRUE, try = TRUE))
      })

      return(invisible(connection))
    }
  )

  # close connection
  x$set_close_server(
    function(input, output, session, connection) {
      connection$close(try = TRUE)

      if (connection$is_close_failed()) {
        shinyjs::alert(
          paste(
            "Error closing connection\nError message: ",
            connection$get_close_error_message()
          )
        )
      }
      return(invisible(connection))
    }
  )

  return(x)
}
