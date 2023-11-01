#' DDL object
#'
#' Object to execute custom DDL code in the shiny session.
#'
#' @section Creating reproducible data:
#' `ddl` object can be used to create reproducible data in the shiny session. `ddl$server` function
#' can execute any R code and return [`teal.data::teal_data-class`]. For reproducibility purposes,
#' we recommend to initialize empty `teal_data` object and evaluate necessary code with `eval_code` or `within`.
#' ```r
#' function(id, ...) {
#'   moduleServer(id, function(input, output, session) {
#'     eventReactive(input$submit, {
#'       data <- teal_data() |> within({
#'         # code to be run when app user presses submit
#'       })
#'     })
#'   })
#' }
#' ```
#' Obtained data is passed further in the `teal` app with `code` which can be used to recreate the objects.
#'
#' @section Code masking:
#'  `ddl` object can be used in a way that evaluated code is different than the code
#' returned in `teal_data` object. Typically occurs when app user is asked to input a
#' password and we'd like to skip this input in the reproducible code. Possibly, users password
#' could be substituted with `askpass::askpass()` call, so the returned code is still executable but secure.
#' `ddl` developer must understand that this is a security risk and should be handled with care.
#' To make sure that the code is reproducible, `ddl` object should be used with `input_mask` argument.
#' `teal` provides convenience function [ddl_run()] which handles evaluation of the code, masking
#' and creating `teal_data` object. Such `server` function could look like this:
#'
#' ```
#' server = function(id, ...) {
#'   moduleServer(id, function(input, output, session) {
#'     reactive({
#'      ddl_run(input = input, ...)
#'     })
#'   })
#' }
#' ```
#'
#' If `ddl` developer values more control, then might be interested in using `...` explicitly,
#' and create `teal_data` object manually.
#'
#' @param ui (`shiny.tag`)\cr
#'  `shiny` user-interface module containing inputs whose `id` correspond to the arguments in the `code`.
#'
#' @param server (`function`)\cr
#'  `shiny` server module [`teal.data::teal_data-class`] possibly wrapped in a [reactive()].
#'  `server` function should have `id` and `...` as formals. Where:
#'  - `id` is a `shiny` module id, and
#'  - `...` passes arguments from the `ddl` object (`code`, `input_mask`, `datanames`, `join_keys`).
#'  See section `Code masking`.
#'
#' @param expr (optional `expression`)\cr
#'  Syntactically valid R expression to be executed in the shiny session.
#'  Shouldn't be specified when `code` is specified.
#'
#' @param code (optional `character` or `language`)\cr
#'  Object containing (defused) syntactically valid R expression to be executed in the shiny session.
#'  Shouldn't be specified when `expr` is specified.
#'
#' @param input_mask (optional `named list`)\cr
#'  arguments to be substituted in the `code`. These (named) list elements are going to replace
#'  symbols in the code prefixed with `input$` or `input[["`. Typically `input_mask` is used
#'  to mask username or password with `list(password = quote(askpass::askpass()))`.
#'  See section `code masking` for more details.
#'
#' @param datanames (optional `character`)\cr
#'  Names of the datasets created by evaluation of the `code`. By default, `datanames`
#'  are obtained from the `join_keys` or from results of the `code` evaluation.
#'  If `code` evaluation creates objects which are not considered as datasets, they
#'  should be omitted from `datanames` to avoid errors.
#'
#' @inheritParams teal.data::teal_data
#'
#' @export
tm_teal_data <- function(label = "data",
                         expr,
                         code,
                         input_mask = list(),
                         ui = submit_button_ui, # or empty div?
                         server = submit_button_server) {
  checkmate::assert_list(input_mask)
  checkmate::check_function(ui, args = "id")
  checkmate::check_function(server, args = c("id", "...")) # todo: data should be passed to this module

  out <- module(
    label = label,
    ui = ui,
    server = server,
    server_args = list(input_mask = input_mask)
    # datanames = "all" by default
  )
  class(out) <- c(class(out), "teal_module_data")

  if (!missing(expr) || !missing(code)) {
    # this is intended to be used with input mask
    # but in the same time we can't forbid user to use it
    # without input_mask. Some users might prefer to use ddl_run
    # to automaticaly handle their code.
    # Q: can NEST bear responsibility for reproducibility of the masked code?
    if (!missing(expr)) {
      code <- substitute(expr)
    }
    if (is.character(code)) {
      code <- parse(text = code)
    }
    out$server_args$code <- code
  }

  out
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


# methods from teal.data ----
#  to be removed soon

#' Get data names from `ddl`
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

# todo: to remove before merge -------------
#' @export
open_conn <- function(username, password) {
  if (password != "pass") stop("Invalid credentials. 'pass' is the password") else TRUE
}
#' @export
close_conn <- function(conn) {
  message("closed")
  return(NULL)
}
