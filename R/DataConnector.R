#' @title A \code{DataConnector} is an R6 class
#' @name DataConnector
#' @description
#' A \code{DataConnector} is an R6 class to manage connection with remote data source.
#'
#' @details
#' A \code{DataConnector} is an R6 class that carrying a shiny module that can return a
#' \code{cdisc_data} object as a \code{reactiveVal}. \code{DataConnector} manages connection
#' accompanied by three other classes. Classes relation illustrated on diagram.
#' \if{html}{\figure{connector.png}{options: width=800}}
#' \if{latex}{\figure{connector.png}{options: width=0.5in}}
#'
#'
#' @importFrom shiny isRunning Progress
#' @importFrom shinyjs disable enable
DataConnector <- R6::R6Class( #nolint
  # DataConnector public ----
  "DataConnector",
  public = list(
    #' @description
    #' Create a new \code{DataConnector} object
    #'
    #' @param connection (\code{DataConnection}) connection to data source
    #' @param connectors (\code{list} of \code{DatasetConnector} elements) list with dataset connectors
    #'
    #' @return new \code{DataConnector} object
    initialize = function() {
      self$set_ui(
        function(id) {
          tags$span("empty page")
        }
      )

      self$set_server_helper()

      invisible(self)
    },
    #' @description
    #' Function to get \code{cdisc_data} object interactively, useful for debugging
    #' @param refresh (\code{logical}) should the data be downloaded?
    #' Defaults to FALSE, which returns last downloaded data.
    #' @param try (\code{logical}) whether perform function evaluation inside \code{try} clause
    #' @param con_args_fixed (\code{NULL} or named \code{list}) fixed argument to connection function
    #' @param con_args_dynamic (\code{NULL} or named \code{list}) dynamic argument to connection function
    #'   (not shown in generated code)
    #' @param con_args_replacement (\code{NULL} or named \code{list}) replacement of dynamic argument of connection
    #'   function
    #' @param fun_args_fixed (\code{NULL} or named \code{list}) fixed argument to pull function
    #' @param fun_args_dynamic (\code{NULL} or named \code{list}) dynamic argument to pull function
    #'   (not shown in generated code)
    #' @param fun_args_replacement (\code{NULL} or named \code{list}) replacement of dynamic argument of pull function
    #'
    #' @return \code{cdisc_data} object
    get_cdisc_data = function(refresh = FALSE,
                              try = FALSE,
                              con_args_fixed = NULL,
                              con_args_dynamic = NULL,
                              con_args_replacement = NULL,
                              fun_args_fixed = NULL,
                              fun_args_dynamic = NULL,
                              fun_args_replacement = NULL) {
      stopifnot(is_logical_single(refresh))
      if (is.null(private$cdisc_data) || refresh) {
        private$refresh_data(try = try,
                             con_args_fixed = con_args_fixed,
                             con_args_dynamic = con_args_dynamic,
                             con_args_replacement = con_args_replacement,
                             fun_args_fixed = fun_args_fixed,
                             fun_args_dynamic = fun_args_dynamic,
                             fun_args_replacement = fun_args_replacement)
      }
      return(private$cdisc_data)
    },
    #' Get connection to data source
    #'
    #' @return connector's connection
    get_connection = function() {
      return(private$connection)
    },
    #' Get connectors
    #'
    #' @return \code{list} with all connectors
    get_connectors = function() {
      return(private$connectors)
    },
    #' @description
    #' Function to get data interactively, useful for debugging
    #' @param refresh (\code{logical}) should the data be downloaded?
    #' Defaults to FALSE, which returns last downloaded data.
    #' @param try (\code{logical}) whether perform function evaluation inside \code{try} clause
    #' @param con_args_fixed (\code{NULL} or named \code{list}) fixed argument to connection function
    #' @param con_args_dynamic (\code{NULL} or named \code{list}) dynamic argument to connection function
    #'   (not shown in generated code)
    #' @param con_args_replacement (\code{NULL} or named \code{list}) replacement of dynamic argument of connection
    #'   function
    #' @param fun_args_fixed (\code{NULL} or named \code{list}) fixed argument to pull function
    #' @param fun_args_dynamic (\code{NULL} or named \code{list}) dynamic argument to pull function
    #'   (not shown in generated code)
    #' @param fun_args_replacement (\code{NULL} or named \code{list}) replacement of dynamic argument of pull function
    #'
    #' @return List of datasets
    get_data = function(refresh = FALSE,
                        try = FALSE,
                        con_args_fixed = NULL,
                        con_args_dynamic = NULL,
                        con_args_replacement = NULL,
                        fun_args_fixed = NULL,
                        fun_args_dynamic = NULL,
                        fun_args_replacement = NULL) {
      stopifnot(is_logical_single(refresh))
      if (is.null(private$data) || refresh) {
        private$refresh_data(try = try,
                             con_args_fixed = con_args_fixed,
                             con_args_dynamic = con_args_dynamic,
                             con_args_replacement = con_args_replacement,
                             fun_args_fixed = fun_args_fixed,
                             fun_args_dynamic = fun_args_dynamic,
                             fun_args_replacement = fun_args_replacement)
      }
      return(private$data)
    },
    #' @description
    #'
    #' @return the \code{server} function of the DataConnector
    get_server = function() {
      return(private$server)
    },
    #' @description
    #'
    #' @param id \code{character} shiny element id
    #'
    #' @return the \code{ui} function of the DataConnector
    get_ui = function(id) {
      return(private$ui(id))
    },
    #' @description
    #' Run simple application that uses its \code{ui} and \code{server} fields
    #'
    #' Useful for debugging
    #'
    #' @return An object that represents the app
    launch = function() {
      shinyApp(
        ui = fluidPage(private$ui(id = "main_app"),
                       br(),
                       uiOutput("result")),
        server = function(input, output, session) {
          dat <- callModule(private$server, id = "main_app")
          output$result <- renderUI({
            if (is(dat(), "cdisc_data")) {
              return(h3("Data successfully loaded!"))
            }
          })
        }
      )
    },
    #' @description
    #' Set preprocessing code
    #'
    #' @param code (\code{character}) preprocessing code
    #'
    #' @return nothing
    set_code = function(code = character(0)) {
      stopifnot(is.character(code))
      private$code <- code
      return(invisible(NULL))
    },
    #' @description
    #' Set reproducibility check
    #'
    #' @param check (\code{logical}) whether to perform reproducibility check
    #'
    #' @return nothing
    set_check = function(check = FALSE) {
      stopifnot(is_logical_single(check))
      private$check <- check
      return(invisible(NULL))
    },
    #' @description
    #' Set connector UI function
    #'
    #' @param ui (\code{function}) ui function of a shiny module
    #'
    #' @return nothing
    set_ui = function(ui) {
      stopifnot(is(ui, "function"))
      stopifnot(names(formals(ui)) == "id")
      private$ui <- ui
      return(invisible(NULL))
    },
    #' @description
    #' Set connector server function
    #'
    #' Please also consider using \code{set_server_helper} method
    #'
    #' @param server (\code{function}) A shiny module server function
    #'   that should return reactive value of type \code{cdisc_data}
    #'
    #' @return nothing
    set_server = function(server) {
      stopifnot(is(server, "function"))
      stopifnot(all(c("input", "output", "session") %in% names(formals(server))))
      private$server <- server
      return(invisible(NULL))
    },
    #' @description
    #' Helper function to set connector server function
    #'
    #' @param submit_id (\code{character}) id of the submit button
    #' @param con_args_fixed (\code{NULL} or named \code{list}) fixed argument to connection function
    #' @param con_args_dynamic (\code{NULL} or named \code{list}) dynamic argument to connection function
    #'   (not shown in generated code)
    #' @param con_args_replacement (\code{NULL} or named \code{list}) replacement of dynamic argument of connection
    #'   function
    #' @param fun_args_fixed (\code{NULL} or named \code{list}) fixed argument to pull function
    #' @param fun_args_dynamic (\code{NULL} or named \code{list}) dynamic argument to pull function
    #'   (not shown in generated code)
    #' @param fun_args_replacement (\code{NULL} or named \code{list}) replacement of dynamic argument of pull function
    #'
    #' @return nothing
    set_server_helper = function(submit_id = "submit",
                                 con_args_fixed = NULL,
                                 con_args_dynamic = NULL,
                                 con_args_replacement = NULL,
                                 fun_args_fixed = NULL,
                                 fun_args_dynamic = NULL,
                                 fun_args_replacement = NULL) {
      stopifnot(is_character_single(submit_id))
      stopifnot(is.null(con_args_fixed) || is.list(con_args_fixed))
      stopifnot(is.null(con_args_dynamic) || is.list(con_args_dynamic))
      stopifnot(is.null(con_args_replacement) ||
                  (is.list(con_args_replacement) && identical(names(con_args_dynamic), names(con_args_replacement))))
      stopifnot(is.null(fun_args_fixed) || is.list(fun_args_fixed))
      stopifnot(is.null(fun_args_dynamic) || is.list(fun_args_dynamic))
      stopifnot(is.null(fun_args_replacement) ||
                  (is.list(fun_args_replacement) && identical(names(fun_args_dynamic), names(fun_args_replacement))))

      self$set_server(
        function(input, output, session) {
          rv <- reactiveVal(NULL)
          observeEvent(input[[submit_id]], {
            private$refresh_data(
              input = input,
              submit_id = submit_id,
              session = session,
              try = TRUE,
              con_args_fixed = con_args_fixed,
              con_args_dynamic = con_args_dynamic,
              con_args_replacement = con_args_replacement,
              fun_args_fixed = fun_args_fixed,
              fun_args_dynamic = fun_args_dynamic,
              fun_args_replacement = fun_args_replacement
            )

            res <- self$get_cdisc_data()

            rv(res)
          })
          return(rv)
        }
      )

      return(invisible(NULL))
    },
    #' Set data connection
    #'
    #' @param connection (\code{DataConnection}) data connection
    #'
    #' @return nothing
    set_connection = function(connection) {
      stopifnot(is(connection, "DataConnection"))
      private$connection <- connection
      return(invisible(NULL))
    },
    #' Set dataset connectors
    #'
    #' @param connectors (\code{list} of \code{DatasetConnector} elements) data connectors
    #'
    #' @return nothing
    set_connectors = function(connectors) {
      stopifnot(is_class_list("DatasetConnector")(connectors))
      private$connectors <- connectors
      return(invisible(NULL))
    }
  ),
  private = list(
    # DataConnector private ----
    server = NULL,
    ui = NULL,
    cdisc_data = NULL,
    connection = NULL,
    connectors = NULL,
    code = character(0),
    check = FALSE,
    data = NULL,
    stop_on_error = function(x, submit_id = character(0), progress = NULL) {
      if (is(x, "try-error")) {
        private$connection$close(silent = TRUE)
        if (shiny::isRunning()) {
          shinyjs::enable(submit_id)
          progress$close()
        }
        error_dialog(x)
      } else {
        x
      }
    },
    refresh_data = function(code = private$code,
                            input = NULL,
                            submit_id = character(0),
                            session = NULL,
                            try = shiny::isRunning(),
                            con_args_fixed = NULL,
                            con_args_dynamic = NULL,
                            con_args_replacement = NULL,
                            fun_args_fixed = NULL,
                            fun_args_dynamic = NULL,
                            fun_args_replacement = NULL) {

      `if`(shiny::isRunning(), shinyjs::disable(submit_id))

      progress <- NULL
      if (shiny::isRunning()) {
        progress <- shiny::Progress$new(session)
        progress$set(0.1, message = "Setting up connection ...")
      }

      optional_eval <- function(x, envir = parent.frame(1L)) {
        if (is.null(x)) {
          return(x)
        } else {
          lapply(
            x,
            function(el) {
              if (is.call(el)) {
                eval(el, envir = envir)
              } else {
                el
              }
            }
          )
        }
      }
      con_args_fixed <- optional_eval(con_args_fixed)
      con_args_dynamic <- optional_eval(con_args_dynamic)
      fun_args_fixed <- optional_eval(fun_args_fixed, parent.frame())
      fun_args_dynamic <- optional_eval(fun_args_dynamic)

      datanames <- vapply(private$connectors, function(x) x$get_dataname(), character(1))

      if (!is.null(private$connection)) {
        private$connection$set_open_args(args = con_args_fixed)
        private$stop_on_error(
          private$connection$open(args = con_args_dynamic, try = try),
          submit_id, progress
        )
      }

      env_data <- new.env()
      for (i in seq_along(private$connectors)) {
        if_not_null(progress,
                    progress$set(0.2 + 0.4 * (i - 1) / length(private$connectors), message = "Loading data ..."))
        private$connectors[[i]]$set_pull_args(args = fun_args_fixed)

        data <- private$stop_on_error(
          private$connectors[[i]]$get_data(args = fun_args_dynamic, try = try),
          submit_id, progress
        )

        assign(
          datanames[[i]],
          data,
          envir = env_data
        )
      }

      if_not_null(private$connection, private$connection$close(silent = TRUE))

      if (!is_character_empty(code)) {
        eval(parse(text = code), env_data)
      }

      list_data <- lapply(
        datanames,
        function(x) {
          get(x, env_data)
        }
      )
      names(list_data) <- datanames
      rm(env_data)

      private$data <- list_data
      rm(list_data)

      args <- lapply(
        seq_along(private$connectors),
        function(i) {
          private$stop_on_error(
            try(cdisc_dataset(
                  dataname = private$connectors[[i]]$get_dataname(),
                  data = private$data[[i]],
                  keys = private$connectors[[i]]$get_keys()
            )),
            submit_id = submit_id,
            progress = progress
          )
        }
      )

      full_code <- c(
        if_not_null(private$connection, private$connection$get_open_call(deparse = TRUE, args = con_args_replacement)),
        vapply(private$connectors, function(x) x$get_call(deparse = TRUE, args = fun_args_replacement), character(1)),
        if_not_null(private$connection, private$connection$get_close_call(deparse = TRUE, silent = TRUE)),
        "\n",
        code
      )

      if_not_null(progress, progress$set(0.7, message = "Preprocessing data ..."))

      args <- append(args,
                     list(code = paste(full_code, collapse = "\n"),
                          check = private$check))
      private$cdisc_data <- do.call(cdisc_data, args)

      if_not_null(progress, progress$set(1, message = "Loading complete!"))

      if_not_null(progress, progress$close())

      return(invisible(NULL))
    }
  )
)


# DataConnector wrappers ----
#' Data connector for \code{random.cdisc.data}
#'
#' Build data connector for \code{random.cdisc.data} functions or datasets
#'
#' @export
#'
#' @param ... (\code{DatasetConnector}) dataset connectors created using \link{rcd_dataset}
#' @param code optional, (\code{character}) preprocessing code
#' @param check optional, (\code{logical}) whether perform reproducibility check
#'
#' @return An object of class \code{DataConnector}
#'
#' @examples
#' \dontrun{
#' library(random.cdisc.data)
#' x <- rcd_cdisc_data(
#'   rcd_dataset("ADSL", radsl, cached = TRUE),
#'   rcd_dataset("ADLB", radlb, cached = TRUE)
#' )
#' app <- init(
#'   data = x,
#'   modules = root_modules(
#'     module(
#'       "ADSL AGE histogram",
#'       server = function(input, output, session, datasets) {
#'         output$hist <- renderPlot(
#'           hist(datasets$get_data("ADSL", filtered = TRUE)$AGE)
#'         )
#'       },
#'       ui = function(id, ...) {ns <- NS(id); plotOutput(ns('hist'))},
#'       filters = "ADSL"
#'     )
#'   ),
#'   filter = NULL,
#'   header = tags$h1("Sample App")
#' )
#' shinyApp(app$ui, app$server)
#' }
rcd_cdisc_data <- function(..., code = character(0), check = TRUE) {
  connectors <- list(...)
  stopifnot(is_class_list("DatasetConnector")(connectors))

  con <- rcd_connection() # nolint

  x <- DataConnector$new()
  x$set_connection(con)
  x$set_connectors(connectors)
  x$set_code(code)
  x$set_check(check)
  x$set_ui(
    function(id) {
      ns <- NS(id)
      tagList(
        numericInput(ns("seed"), "Choose seed", min = 1, max = 1000, value = 1),
        actionButton(ns("submit"), "Submit")
      )
    }
  )
  x$set_server_helper(
    submit_id = "submit",
    fun_args_fixed = list(seed = quote(input$seed))
  )

  return(x)
}

#' Data connector for \code{RICE}
#'
#' Build data connector for \code{RICE} datasets
#'
#' @importFrom askpass askpass
#'
#' @export
#'
#' @param ... (\code{DatasetConnector}) dataset connectors created using \link{rice_dataset}
#' @param additional_ui \code{shiny.tag} additional user interface to be visible over login panel
#' @inheritParams rcd_cdisc_data
#'
#' @return An object of class \code{DataConnector}
#'
#' @examples
#' \dontrun{
#' x <- rice_cdisc_data(
#'   rice_dataset("ADSL", "/path/to/ADSL"),
#'   rice_dataset("ADLB", "/path/to/ADLB")
#' )
#' app <- init(
#'   data = x,
#'   modules = root_modules(
#'     module(
#'       "ADSL AGE histogram",
#'       server = function(input, output, session, datasets) {
#'         output$hist <- renderPlot({
#'           hist(datasets$get_data("ADSL", filtered = TRUE)$AGE)
#'         })
#'       },
#'       ui = function(id, ...) {ns <- NS(id); plotOutput(ns('hist'))},
#'       filters = "ADSL"
#'     )
#'   ),
#'   filter = NULL,
#'   header = tags$h1("Sample App")
#' )
#' shinyApp(app$ui, app$server)
#' }
rice_cdisc_data <- function(..., code = character(0), additional_ui = NULL) {
  connectors <- list(...)
  stopifnot(is_class_list("DatasetConnector")(connectors))

  con <- rice_connection() # nolint

  x <- DataConnector$new()
  x$set_connection(con)
  x$set_connectors(connectors)
  x$set_code(code)
  x$set_check(`attributes<-`(FALSE, list(quiet = TRUE)))
  x$set_ui(
    function(id) {
      ns <- NS(id)
      shinyjs::useShinyjs()
      fluidPage(
        fluidRow(
          column(
            8,
            offset = 2,
            ui_connectors("rice", connectors),
            br(),
            additional_ui,
            br(),
            textInput(ns("login"), "Login"),
            passwordInput(ns("pass"), "Password"),
            actionButton(ns("submit"), "Submit")
          )
        )
      )
    }
  )
  x$set_server_helper(
    submit_id = "submit",
    con_args_fixed = list(username = quote(input$login)),
    con_args_dynamic = list(password = quote(input$pass)),
    con_args_replacement = list(password = quote(askpass::askpass()))
  )

  return(x)
}

#' Data connector for \code{.rds} files
#'
#' Build data connector for RDS file connections
#'
#' @export
#'
#' @param ... (\code{DatasetConnector}) dataset connectors created using \link{rds_cdisc_dataset}
#' @param code optional, (\code{character}) preprocessing code
#' @param check optional, (\code{logical}) whether perform reproducibility check
#'
#' @return An object of class \code{DataConnector}
#'
#' @examples
#' \dontrun{
#' x <- rds_cdisc_dataset("ADSL", "/path/to/file.rds")
#' app <- init(
#'   data = rds_cdisc_data(x, check = TRUE),
#'   modules = root_modules(
#'     module(
#'       "ADSL AGE histogram",
#'       server = function(input, output, session, datasets) {
#'         output$hist <- renderPlot(
#'           hist(datasets$get_data("ADSL", filtered = TRUE)$AGE)
#'         )
#'       },
#'       ui = function(id, ...) {ns <- NS(id); plotOutput(ns('hist'))},
#'       filters = "ADSL"
#'     )
#'   ),
#'   filter = NULL,
#'   header = tags$h1("Sample App")
#' )
#' shinyApp(app$ui, app$server)
#' }
rds_cdisc_data <- function(..., code = character(0), check = TRUE) {
  connectors <- list(...)
  stopifnot(is_class_list("DatasetConnector")(connectors))


  x <- DataConnector$new()
  x$set_connectors(connectors)
  x$set_code(code)
  x$set_check(check)
  x$set_ui(
      function(id) {
        ns <- NS(id)
        tagList(
            h2("Loaded datasets:"),
            do.call(
              tagList,
              lapply(x$get_connectors(), function(con) tags$p(paste(con$get_dataname(), ":", con$get_path())))
            ),
            actionButton(ns("start"), "Start")
        )
      }
  )
  x$set_server_helper(
      submit_id = "start",
      fun_args_fixed = NULL
  )

  return(x)
}

#' Creates UI from \code{DatasetConnector} objects
#'
#' @param type \code{character} giving the type of connection.
#' @param connectors \code{list} of \code{DatasetConnector} objects.
#'
#' @return \code{shiny.tag} UI describing the connectors
#'
ui_connectors <- function(type, connectors) {

  stopifnot(is_character_single(type))
  stopifnot(is_class_list("DatasetConnector")(connectors))

  out <- div(
    h1("TEAL - Access data on entimICE using", toupper(type)),
    br(),
    h5("Data access requested for:"),
    fluidRow(
      column(
        11,
        offset = 1,
        lapply(seq_along(connectors), function(i) {
          tags$li(paste0(connectors[[i]]$get_dataname(),
                         ": ",
                         connectors[[i]]$get_path()))
        })
      )
    )
  )

  return(out)
}
