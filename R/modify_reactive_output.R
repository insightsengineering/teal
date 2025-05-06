# This function is just here, so it is easier to understand what's happening in
# modify_reactive_output
# We want to have a match in parameter names in new_server and original_server
# assuming we don't know names of parameters in original_server.
# modify_reactive_output is the final implementation that shows how to do it without knowing parameters.
modify_reactive_output <- function(teal_module, ...) {
  checkmate::assert_class(teal_module, "teal_module")
  output_funs <- list(...)
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
          res <- if (is.reactive(original_outputs[[output_name]])) {
            original_outputs[[output_name]]()
          } else {
            original_outputs[[output_name]]
          }
          output_funs[[output_name]](res)
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
