#' Modify Reactive Outputs of a `teal` Module
#'
#' @description
#' This function takes a `teal_module` object and allows modification of its
#' server's reactive outputs. It wraps the original module's server,
#' applying specified functions to one or more of the outputs returned by the
#' original server.
#'
#' The original server logic is called within a new server function.
#' The transformations are applied to the outputs of the original server
#' before they are returned by the new server.
#'
#' @param teal_module (`teal_module`)
#' @param ... Named arguments where each name corresponds to an output element
#'   (e.g., a reactive expression or a static value) returned by the
#'   `teal_module`'s server function. The value for each named argument must be a
#'   function that takes the original output's value as its single argument and
#'   returns the modified value. If the original output is a reactive, its
#'   resolved value is passed to the modifying function, and the modifying
#'   function's result is wrapped in a new reactive.
#'
#' @return A new `teal_module` object with the modified server logic and a UI
#'   that internally namespaces the original UI. The new module will have the
#'   same label, datanames, server_args, ui_args, and transformators as the
#'   original `teal_module`.
#'
#' @export
modify_reactive_output <- function(teal_module, ...) {
  checkmate::assert_class(teal_module, "teal_module")
  output_funs <- list(...)

  if (length(output_funs) == 0) {
    warning("No output transformations specified for modify_reactive_output. Returning original module.")
    return(teal_module)
  }
  if (is.null(names(output_funs)) || any(names(output_funs) == "" | duplicated(names(output_funs)))) {
    stop("All transformations supplied to modify_reactive_output must be uniquely named.")
  }
  are_functions <- vapply(output_funs, is.function, logical(1))
  if (!all(are_functions)) {
    stop("All transformations supplied to modify_reactive_output must be functions.")
  }

  original_server <- teal_module$server
  original_ui <- teal_module$ui
  new_ui <- function(id, ...) {
    ns <- NS(id)
    original_ui(ns("modified"), ...)
  }

  original_server <- teal_module$server
  new_server <- function(...) { }
  formals(new_server) <- formals(original_server)

  body(new_server) <- quote({
    arg_names <- names(formals(original_server))

    # Capture current call and evaluate each argument
    call_expr <- match.call()
    call_args <- as.list(call_expr)[-1] # remove function name
    call_args <- call_args[names(call_args) %in% arg_names]

    # Evaluate all arguments in the current environment
    eval_args <- lapply(call_args, eval, envir = parent.frame())

    moduleServer(id, function(input, output, session) {
      eval_args$id <- "modified"
      original_outputs <- do.call(original_server, eval_args)

      missing_output_names <- setdiff(names(output_funs), names(original_outputs))
      if (length(missing_output_names)) {
        stop("The provided teal_module does not return: ", toString(missing_output_names))
      }

      modified_output <- sapply(
        names(output_funs),
        function(output_name) {
          if (is.reactive(original_outputs[[output_name]])) {
            reactive({
              res <- original_outputs[[output_name]]()
              output_funs[[output_name]](res)
            })
          } else {
            res <- original_outputs[[output_name]]
            output_funs[[output_name]](res)
          }
        },
        USE.NAMES = TRUE
      )

      return(modifyList(original_outputs, modified_output))
    })
  })

  module(
    label = teal_module$label,
    server = new_server,
    ui = new_ui,
    datanames = teal_module$datanames,
    server_args = teal_module$server_args,
    ui_args = teal_module$ui_args,
    transformators = teal_module$transformators
  )
}

modify_report_card <- function(teal_module, modify_fun) {
  modify_reactive_output(teal_module, report_card = modify_fun)
}

disable_report <- function(teal_module) {
  modify_reactive_output(teal_module, report_card = function(report_card) NULL)
}


############################################################## EXAMPLE
#
# take_3_elements <- function(x){
#   teal.reporter::edit_report_document(x, modify = 1:3)
# }
#
# devtools::load_all('../teal.reporter')
# devtools::load_all('../teal.widgets')
# devtools::load_all('../teal.code')
# devtools::load_all('.')
# devtools::load_all('../teal.modules.general')
#
# data <- teal_data()
# data <- within(data, {
#   require(nestcolor)
#   CO2 <- CO2
# })
#
# app <- init(
#   data = data,
#   modules = modules(
#     tm_a_regression(
#       label = "Regression",
#       response = data_extract_spec(
#         dataname = "CO2",
#         select = select_spec(
#           label = "Select variable:",
#           choices = "uptake",
#           selected = "uptake",
#           multiple = FALSE,
#           fixed = TRUE
#         )
#       ),
#       regressor = data_extract_spec(
#         dataname = "CO2",
#         select = select_spec(
#           label = "Select variables:",
#           choices = variable_choices(data[["CO2"]], c("conc", "Treatment")),
#           selected = "conc",
#           multiple = TRUE,
#           fixed = FALSE
#         )
#       )
#     ) |> modify_teal_module_report_card(take_3_elements)
#   )
# )
#
# if (interactive()) {
#   shinyApp(app$ui, app$server)
# }
