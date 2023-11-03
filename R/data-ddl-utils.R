#' Function runs the `code`, masks the `code` and creates `teal_data` object.
#' @param data (`teal_data`) object
#' @param code (`language`) code to evaluate
#' @param input (`list`) containing inputs to be used in the `code`
#' @param input_mask (`list`) containing inputs to be masked in the `code`
#'
#' @return `teal_data` object
#'
#' @export
eval_and_mask <- function(data,
                          code,
                          input = list(),
                          input_mask = list()) {
  # todo: do we need also within_and_mask?
  checkmate::assert_list(input)
  if (inherits(input, "reactivevalues")) {
    input <- shiny::reactiveValuesToList(input)
  }
  # evaluate code and substitute input
  data <- teal.code::eval_code(data, .substitute_code(code, args = input))
  if (inherits(data, "qenv.error")) {
    return(data)
  }

  if (identical(ls(data@env), character(0))) {
    warning(
      "Evaluation of `ddl` code haven't created any objects.\n",
      "Please make sure that the code is syntactically correct and creates necessary data."
    )
  }

  if (!missing(input_mask)) {
    # mask dynamic inputs with mask
    input <- utils::modifyList(input, input_mask)

    # replace last code entry with masked code
    # format_expression needed to convert expression into character(1)
    #  question: warnings and errors are not masked, is it ok?
    data@code[length(data@code)] <- format_expression(.substitute_code(code, args = input))
  }

  # todo: should it be here or in datanames(data)?
  if (length(datanames(data)) == 0) {
    datanames(data) <- ls(data@env)
  }

  data
}

#' Substitute symbols in the code
#'
#' Function replaces symbols in the provided code by values of the `args` argument.
#'
#' @param code (`language`) code to substitute
#' @param args (`list`) named list or arguments
#' @keywords internal
.substitute_code <- function(code, args) {
  do.call(
    substitute,
    list(
      expr = do.call(
        substitute,
        list(expr = code)
      ),
      env = args
    )
  )
}

#' Convenience wrapper for ddl
#' @export # todo: do we want to export this?
ddl <- function(code, input_mask, ui, server) {
  delayed_data(ui = ui, server = server, code = code, input_mask = input_mask)
}

ui_login_and_password <- function(id) {
  ns <- NS(id)
  actionButton(inputId = ns("submit"), label = "Submit")
}

srv_login_and_password <- function(id, code, input_mask) {
  moduleServer(id, function(input, output, session) {
    eventReactive(input$submit, {
      teal_data() |> eval_and_mask(code = code, input = input, input_mask = input_mask)
    })
  })
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
