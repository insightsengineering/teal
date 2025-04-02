modify_teal_module_report <- function(teal_module, modify_fun) {
  stopifnot(inherits(teal_module, "teal_module"))

  original_server <- teal_module$server

  new_server <- function(id, data, reporter, ...) {
    moduleServer(id, function(input, output, session) {
      original_outputs <- original_server(id, data, ...)

      if (!"report_card" %in% names(original_outputs)) {
        stop("The provided teal module does not return a 'report_card'.")
      }

      modified_report_card <- reactive({
        modify_fun(original_outputs$report_card())
      })

      return(modifyList(original_outputs, list(report_card = modified_report_card)))
    })
  }

  new_module <- module(
    label = paste0(teal_module$label, " (Modified)"),
    server = new_server,
    ui = teal_module$ui,
    datanames = teal_module$datanames,
    server_args = teal_module$server_args,
    ui_args = teal_module$ui_args,
    transformators = teal_module$transformators
  )

  return(new_module)
}
#
#
# take_3_elements <- function(x){
#   teal.reporter::edit_report_document(x, modify = 1:3)
# }
#
#
#
# devtools::load_all('../teal.reporter')
# devtools::load_all('../teal.widgets')
# devtools::load_all('../teal.code')
# devtools::load_all('../teal.modules.general')
# devtools::load_all('.')
#
# # general data example
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
#     ) |>
#       modify_teal_module_report(modify_fun = take_3_elements)
#   )
# )
#
# if (interactive()) {
#   shinyApp(app$ui, app$server)
# }
