#' Function runs the `code`, masks the `code` and creates `teal_data` object.
#' @param input (`list`) containing inputs to be used in the `code`
#' @inheritParams tm_teal_data
#'
#' @return `teal_data` object
#'
#' @export
eval_and_mask <- function(data,
                          code,
                          input = list(),
                          input_mask = list()) {
  checkmate::assert_list(input)
  if (inherits(input, "reactivevalues")) {
    input <- shiny::reactiveValuesToList(input)
  }

  # evaluate code and substitute input
  data <- teal.code::eval_code(data, .mask_code(code, args = input))

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
    data@code[length(code)] <- format_expression(.mask_code(code, args = input))
  }

  # todo: should it be here or in datanames(data)?
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
.mask_code <- function(code, args) {
  code <- if (identical(as.list(code)[[1L]], as.symbol("{"))) {
    as.list(code)[-1L]
  } else {
    code
  }

  code_strings <- vapply(code, deparse1, character(1L))
  # Replace input$ with .()
  code_strings <- gsub("input\\$(\\w+\\.?\\w*)", "\\.(\\1)", code_strings)
  code_strings <- gsub("(input\\$)(`[^`]+`)", "\\.(\\2)", code_strings)

  # Replace input[[ with .()
  code_strings <- gsub("(input\\[\\[\")(\\w+\\.?\\w*)(\"\\]\\])", "\\.(\\2\\)", code_strings)
  code_strings <- gsub("(input\\[\\[\")(\\w+\\-\\w+)\"\\]\\]", ".(`\\2`)", code_strings)

  # Use bquote to obtain code with input values and masking values.
  # todo: make sure it produces only one entry in qenv@code!!!
  as.expression(
    lapply(code_strings, function(x) {
      do.call(bquote, list(str2lang(x), list2env(args)))
    })
  )
}
