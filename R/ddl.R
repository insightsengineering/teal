#' DDL object
#'
#' Object to execute custom DDL code in the shiny session
#'
#' @param expr (`expression`)\cr
#'  Syntatically valid R code to be executed in the shiny session.
#'  shouldn't be specified when `code` is specified.
#'
#' @param code (`character`, `language`)\cr
#'  Object containing code to be evaluated to load data. Shouldn't be specified when `expr`
#'  is specified.
#'
#'
#' @param ui (`shiny.tag`)\cr
#'   `shiny` ui module containing inputs which `id` correspond to the
#'   args in the `code`.
#'
#' @param server (`function`)\cr
#'   `shiny` server module returning data. This server should execute
#'   `code` and return a reactive data containing necessary data. To handle
#'   evaluation and code masking process it is recommended to use `ddl_run`.
#'   Package provides universal `username_password_server` which
#'   runs [ddl_run] function, which returns `teal_data` object.
#'   Details in the the example
#'
#' @param input_mask (`list` named)\cr
#'   arguments to be substituted in the `code`. These
#'   argument are going to replace arguments set through
#'   `ui` and `server`. Example use case is when app user
#'   is asked to input a password and we'd like to skip this
#'   input in the reproducible code. Typically users password
#'   is substituted with `askpass::askpass()` call, so the
#'   returned code is still executable but secure.
#'
#' @param datanames (`character`)\cr
#'   Names of the objects to be created from the code evaluation.
#'   If not specified (`character(0)`), all objects will be used to `teal_data` function
#'   (via `env_list` in `postprocess_fun`).
#'
#' @inheritParams teal.data::teal_data
#'
#'
#' @export
ddl <- function(expr,
                code,
                ui = submit_button_ui,
                input_mask = list(),
                server = submit_button_server,
                join_keys = teal.data::join_keys(),
                datanames) {
  if (!missing(expr) && !missing(code)) {
    stop("Only one of `expr` or `code` should be specified")
  }
  if (!missing(expr)) {
    code <- substitute(expr)
  }
  if (is.character(code)) {
    code <- parse(text = code)
  }

  if (missing(datanames)) {
    stop("`dataname` argument is required")
  }


  # function creates  object from the code, input and input_mask
  # function defined here to have access to the arguments
  ddl_run <- function(input = list()) {
    checkmate::assert_list(input)
    if (inherits(input, "reactivevalues")) {
      input <- shiny::reactiveValuesToList(input)
    }
    env <- list2env(list(input = input))
    # substitute by online args and evaluate
    eval(code, envir = env)

    if (identical(ls(env), character(0))) {
      warning("DDL code returned NULL. Returning empty  object")
    }

    # don't keep input further we don't want to keep input in the @env of teal_data
    # but we want to keep other non-dataset objects created in the code
    env_list <- as.list(env)
    env_list <- env_list[names(env_list) != "input"]

    # substitute by offline args
    input <- modifyList(input, input_mask)
    code <- .substitute_inputs(code, args = input)

    # create  object
    obj <- teal.data::new_teal_data(
      data = env_list,
      code = as.expression(code),
      keys = join_keys,
      datanames = datanames
    )

    if (!inherits(obj, "teal_data")) {
      stop("postprocess_fun should return `teal_data` object")
    }

    obj
  }

  # changing enclosing environment of the server to have access to ddl_fun function
  # Thanks to this ddl object contains only ui and server functions
  #  and server function can be run just by calling ddl$server("<id>")!
  environment(server) <- environment()

  structure(
    list(ui = ui, server = server),
    datanames = datanames,
    join_keys = join_keys,
    class = "ddl"
  )
}

#' @name submit_button_module
#'
#' @inheritParams ddl
#' @param id (`character`) `shiny` module id.
NULL

#' @rdname submit_button_module
#' @export
submit_button_ui <- function(id) {
  ns <- NS(id)
  actionButton(inputId = ns("submit"), label = "Submit")
}

#' @rdname submit_button_module
#' @export
submit_button_server <- function(id, x) {
  moduleServer(id, function(input, output, session) {
    tdata <- eventReactive(input$submit, {
      ddl_run(input = input)
    })

    # would need to make sure we handle reactivity correctly here as teal::init expects not reactive teal_data...
    return(tdata)
  })
}

#' substitute inputs in the code
#'
#' Function replaces symbols in the provided code prefixed with `input$` or `input[["`
#' by values of the `args` argument.
#'
#' @param code (`language`) code to substitute
#' @param args (`list`) named list or arguments
.substitute_inputs <- function(code, args) {
  code <- if (identical(as.list(code)[[1L]], as.symbol("{"))) {
    as.list(code)[-1L]
  } else {
    code
  }

  code_strings <- vapply(code, deparse1, character(1L))
  code_strings <- gsub("(input\\$)(\\w+)", "\\.(\\2\\)", code_strings)
  code_strings <- gsub("(input\\[\\[\")(\\w+)(\"\\]\\])", "\\.(\\2\\)", code_strings)

  # Use bquote to obtain code with input values and masking values.
  lapply(code_strings, function(x) {
    bquote_call <- substitute(bquote(code), list(code = str2lang(x)))
    eval(bquote_call, envir = list2env(args))
  })
}

# todo: to remove -------------
open_conn <- function(username, password) {
  if (password != "pass") stop("Invalid credentials. 'pass' is the password") else TRUE
}
close_conn <- function(conn) {
  message("closed")
  return(NULL)
}


# methods from teal.data

#' @rdname get_dataname
#' @export
get_dataname.ddl <- function(x) {
  attr(x, "datanames")
}

#' @rdname get_join_keys
#' @export
get_join_keys.ddl <- function(data) {
  attr(data, "join_keys")
}
