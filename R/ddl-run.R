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
ddl_run <- function(input = list(),
                    code,
                    input_mask = list(),
                    join_keys = teal.data::join_keys(),
                    datanames = names(join_keys$get())) {
  checkmate::assert_list(input)
  if (inherits(input, "reactivevalues")) {
    input <- shiny::reactiveValuesToList(input)
  }
  data <- teal_data(join_keys = join_keys)

  # evaluate code and substitute input
  data <- teal.code::eval_code(data, .substitute_inputs(code, args = input))

  if (identical(ls(data@env), character(0))) {
    warning(
      "Evaluation of `ddl` code haven't created any objects.\n",
      "Please make sure that the code is syntactically correct and creates necessary data."
    )
  }

  if (!missing(input_mask)) {
    # mask dynamic inputs with mask
    input <- utils::modifyList(input, input_mask)

    # replace code of teal_data with masked code
    #  question: warnings and errors are not masked, is it ok?
    data@code <- format_expression(.substitute_inputs(code, args = input))
  }

  if (length(datanames)) {
    datanames(data) <- datanames
  }
  if (length(datanames(data)) == 0) {
    datanames(data) <- ls(data@env)
  }

  data
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
