#' Evaluate Expression on `teal_data_module`
#'
#' @details
#' `within` is a convenience function for evaluating inline code inside the environment of a `teal_data_module`.
#'
#' @param data (`teal_data_module`) object
#' @param expr (`expression`) to evaluate. Must be inline code. See
#' @param ... See `Details`.
#'
#' @return
#' `within` returns a `teal_data_module` object with a delayed evaluation of `expr` when the module is run.
#'
#' @seealso [`base::within()`], [`teal.code::within.qenv()`]
#'
#' @examples
#' tdm <- teal_data_module(
#'   ui = function(id) div(id = shiny::NS(id)("div_id")),
#'   server = function(id) {
#'     shiny::moduleServer(id, function(input, output, session) {
#'       shiny::reactive(teal_data(IRIS = iris))
#'     })
#'   }
#' )
#' within(tdm, IRIS <- subset(IRIS, Species == "virginica"))
#'
#' @include teal_data_module.R
#' @name within
#' @rdname teal_data_module
#'
#' @export
#'
within.teal_data_module <- function(data, expr, ...) {
  expr <- substitute(expr)
  extras <- list(...)

  # Add braces for consistency.
  if (!identical(as.list(expr)[[1L]], as.symbol("{"))) {
    expr <- call("{", expr)
  }

  calls <- as.list(expr)[-1]

  # Inject extra values into expressions.
  calls <- lapply(calls, function(x) do.call(substitute, list(x, env = extras)))

  eval_code(object = data, code = as.expression(calls))
}
