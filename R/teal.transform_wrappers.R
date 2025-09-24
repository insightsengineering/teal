#' @export
teal_transform_filter <- function(x, label = "Filter") {
  checkmate::assert_class(x, "picks")
  checkmate::assert_true("values" %in% names(x))

  teal_transform_module(
    label = label,
    ui <- function(id) {
      ns <- NS(id)
      teal.transform::module_input_ui(ns("transformer"), spec = x)
    },
    server <- function(id, data) {
      moduleServer(id, function(input, output, session) {
        selector <- teal.transform::module_input_srv("transformer", spec = x, data = data)
        reactive({
          req(data(), selector())
          teal.code::eval_code(data(), .make_filter_call(selector()))
        })
      })
    }
  )
}

.make_filter_call <- function(x) {
  checkmate::assert_class(x, "picks")
  substitute(
    dataname <- dplyr::filter(dataname, varname %in% values),
    list(
      dataname = str2lang(x$datasets$selected),
      varname = str2lang(x$variables$selected),
      values = x$values$selected
    )
  )
}
