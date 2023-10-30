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
#'   runs `ddl_run` function, which returns `teal_data` object.
#'   Details in the the example
#'  - `code` (`character`, `language`) code to be executed and returned in `teal_data` object.
#'  - `input_mask` (`list` named) arguments to be substituted in the `code`.
#'  - `datanames` (`character`) names of the objects to be created from the code evaluation.
#'  - `join_keys` (`JoinKeys`) object
#'  `...` can be handled automatically by [ddl_run()] but
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
                code = character(0),
                ui = submit_button_ui,
                input_mask = list(),
                server = submit_button_server,
                join_keys = teal.data::join_keys(),
                datanames = names(join_keys$get())) {
  if (!missing(expr)) {
    code <- substitute(expr)
  }
  if (is.character(code)) {
    code <- parse(text = code)
  }
  checkmate::assert_list(input_mask)
  checkmate::check_function(ui, args = "id")
  checkmate::check_function(server, args = c("id", "..."))
  checkmate::check_class(join_keys, "JoinKeys")
  checkmate::check_character(datanames, min.len = 1)

  ddl_object <- structure(
    list(ui = ui, server = server),
    code = code,
    input_mask = input_mask,
    datanames = datanames,
    join_keys = join_keys,
    class = "ddl"
  )

  ddl_object
}

#' @name ddl_module
#' @keywords internal
NULL

#' @rdname ddl_module
#' @keywords internal
ddl_server <- function(id, x) {
  attrs <- attributes(x)
  do.call(
    x$server,
    c(
      list(id = id),
      attrs[setdiff(names(attrs), c("class", "names"))]
    )
  )
}

#' @rdname ddl_module
#' @keywords internal
ddl_ui <- function(id, x) {
  x$ui(id = id)
}

#' Run code and mask inputs
#'
#' Delayed Data Loading module with login and password input.
#'
#' @name submit_button_module
#'
#'
#' @param id (`character`) `shiny` module id.
#' @param ... (`list`) arguments passed to `ddl_run` function.
#' @return `shiny` module
NULL

#' @rdname submit_button_module
#' @export
submit_button_ui <- function(id) {
  ns <- NS(id)
  actionButton(inputId = ns("submit"), label = "Submit")
}

#' @rdname submit_button_module
#' @export
submit_button_server <- function(id, ...) {
  moduleServer(id, function(input, output, session) {
    tdata <- eventReactive(input$submit, {
      ddl_run(input = input, ...)
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
  as.expression(
    lapply(code_strings, function(x) {
      do.call(bquote, list(str2lang(x), list2env(args)))
    })
  )
}

# todo: to remove -------------
#' @export
open_conn <- function(username, password) {
  if (password != "pass") stop("Invalid credentials. 'pass' is the password") else TRUE
}
#' @export
close_conn <- function(conn) {
  message("closed")
  return(NULL)
}


# methods from teal.data

#' Get datanames from `ddl`
#' @rdname get_dataname
#' @param x (`ddl`) object
#' @export
get_dataname.ddl <- function(x) {
  attr(x, "datanames")
}

#' @rdname get_join_keys
#' @export
get_join_keys.ddl <- function(data) {
  attr(data, "join_keys")
}

#' Run code and mask inputs
#'
#' Function runs the `code`, masks the `code` and creates `teal_data` object.
#' @param input (`list`) containing inputs to be used in the `code`
#' @param code (`language`) code to be executed
#' @param input_mask (`list`) containing inputs to be masked in the `code`
#' @param datanames (`character`) names of the objects to be created from the code evaluation
#' @param join_keys (`join_keys`) object
#'
#' @return `teal_data` object
#'
#' @export
ddl_run <- function(input = list(), code, input_mask, datanames, join_keys) {
  checkmate::assert_list(input)
  if (inherits(input, "reactivevalues")) {
    input <- shiny::reactiveValuesToList(input)
  }
  data <- teal_data(join_keys = join_keys)

  # evaluate code and substitute input
  data <- teal.code::eval_code(data, .substitute_inputs(code, args = input))

  if (identical(ls(data@env), character(0))) {
    warning("DDL code returned NULL. Returning empty object")
  }

  if (length(input_mask) > 0) {
    # mask dynamic inputs with mask
    input <- utils::modifyList(input, input_mask)

    # replace code of teal_data with masked code
    #  question: warnings and errors are not masked, is it ok?
    data@code <- format_expression(.substitute_inputs(code, args = input))
  }
  datanames(data) <- datanames

  data
}
