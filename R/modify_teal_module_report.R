
# This function is just here, so it is easier to understand what's happening in
# modify_teal_module_output.
# We want to have a match in parameter names in new_server and original_server
# assuming we don't know names of parameters in original_server.
# modify_teal_module_output is the final implementation that shows how to do it without knowing parameters.
# modify_teal_module_output_named_params is just here to show how would that look like for a custom and stable
# set of parameter names.
modify_teal_module_output_named_params <- function(teal_module, modify_fun, output_name) {
  stopifnot(inherits(teal_module, "teal_module"))

  original_server <- teal_module$server
  original_ui <- teal_module$ui
  new_ui <- function(id, ...){
    ns <- NS(id)
    original_ui(ns('modified'), ...)
  }

  new_server <- function(id,
                         data,
                         filter_panel_api,
                         response,
                         regressor,
                         reporter,
                         plot_height,
                         plot_width,
                         ggplot2_args,
                         default_outlier_label,
                         decorators)

    moduleServer(id, function(input, output, session) {

      original_outputs <- original_server(
        'modified',
        data,
        filter_panel_api,
        response = response,
        regressor = regressor,
        reporter,
        plot_height,
        plot_width,
        ggplot2_args,
        default_outlier_label,
        decorators
      )
      # work on original_outputs
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


modify_teal_module_output <- function(teal_module, modify_fun, output_name) {
  stopifnot(inherits(teal_module, "teal_module"))

  original_server <- teal_module$server
  original_ui <- teal_module$ui
  new_ui <- function(id, ...){
    ns <- NS(id)
    original_ui(ns('modified'), ...)
  }

  original_server <- teal_module$server
  new_server <- function(...) { }
  formals(new_server) <- formals(original_server)

  body(new_server) <- quote({

    arg_names <- names(formals(original_server))

    # Capture current call and evaluate each argument
    call_expr <- match.call()
    call_args <- as.list(call_expr)[-1]  # remove function name
    call_args <- call_args[names(call_args) %in% arg_names]

    # Evaluate all arguments in the current environment
    eval_args <- lapply(call_args, eval, envir = parent.frame())

    moduleServer(id, function(input, output, session) {
      eval_args$id <- 'modified'
      original_outputs <- do.call(original_server, eval_args)
      if (!output_name %in% names(original_outputs)) {
        stop(paste0("The provided teal_module does not return ", output_name))
      }
      stopifnot(is.reactive(original_outputs[[output_name]]))

      modified_output <- reactive({
        modify_fun(original_outputs[[output_name]]())
      })

      named_modified_output <- setNames(list(modified_output), output_name)
      return(modifyList(original_outputs, named_modified_output))

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

modify_teal_module_report_card <- function(teal_module, modify_fun) {
  modify_teal_module_output(teal_module, modify_fun, output_name = 'report_card')
}

nullify_teal_module_report_card <- function(teal_module) {
  modify_teal_module_output(teal_module, modify_fun = function() NULL, output_name = 'report_card')
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

