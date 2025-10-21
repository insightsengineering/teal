#' @export
teal_transform_filter <- function(x, label = "Filter") {
  checkmate::assert_class(x, "picks")
  checkmate::assert_true("values" %in% names(x))

  teal_transform_module(
    label = label,
    ui <- function(id) {
      ns <- NS(id)
      teal.transform::picks_ui(ns("transformer"), spec = x, container = div)
    },
    server <- function(id, data) {
      moduleServer(id, function(input, output, session) {
        selector <- teal.transform::picks_srv("transformer", spec = x, data = data)
        reactive({
          req(data(), selector())
          # todo: make sure filter call is not executed when setequal(selected, all_possible_choices)
          filter_call <- .make_filter_call(
            datasets = selector()$datasets$selected,
            variables = selector()$variables$selected,
            values = selector()$values$selected
          )
          teal.code::eval_code(data(), filter_call)
        })
      })
    }
  )
}

.make_filter_call <- function(datasets, variables, values) {
  checkmate::assert_character(datasets)
  checkmate::assert_character(variables)
  checkmate::assert_character(values)
  substitute(
    dataname <- dplyr::filter(dataname, varname %in% values),
    list(
      dataname = as.name(datasets),
      varname = if (length(variables) == 1) {
        as.name(variables)
      } else {
        as.call(
          c(
            quote(paste),
            lapply(variables, as.name),
            list(sep = ", ")
          )
        )
      },
      values = values
    )
  )
}
