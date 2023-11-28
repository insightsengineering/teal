setOldClass("teal_data_module")

#' @name eval_code
#' @description
#' Given code is evaluated in the `qenv` environment of `teal_data` reactive defined in `teal_data_module`.
#'
#' @param object (`teal_data_module`)
#' @inherit teal.code::eval_code
#' @importMethodsFrom teal.code eval_code
#' @export
#' @examples
#' tdm <- teal_data_module(
#'   ui = function(id) div(id = shiny::NS(id)("div_id")),
#'   server = function(id) {
#'     shiny::moduleServer(id, function(input, output, session) {
#'       shiny::reactive(teal_data(IRIS = iris))
#'     })
#'   }
#' )
#' \dontrun{
#' eval_code(tdm, "IRIS <- subset(IRIS, Species == 'virginica')")
#' }
setMethod("eval_code", signature = c("teal_data_module", "character"), function(object, code) {
  tdm <- teal_data_module(
    ui = function(id) {
      ns <- NS(id)
      object$ui(ns("mutate_inner"))
    },
    server = function(id) {
      moduleServer(id, function(input, output, session) {
        data <- object$server("mutate_inner")
        reactive(eval_code(data(), code))
      })
    }
  )

  attr(tdm, "code") <- paste(c(attr(object, "code"), code), collapse = "\n")
  tdm
})

#' @rdname eval_code
#' @export
setMethod("eval_code", signature = c("teal_data_module", "language"), function(object, code) {
  eval_code(object, code = format_expression(code))
})

#' @rdname eval_code
#' @export
setMethod("eval_code", signature = c("teal_data_module", "expression"), function(object, code) {
  eval_code(object, code = format_expression(code))
})

#' @inherit teal.code::within.qenv params title details
#' @description
#' Convenience function for evaluating inline code inside the environment of a
#' `teal_data_module`
#'
#' @param data (`teal_data_module`) object
#' @return Returns a `teal_data_module` object with a delayed evaluation of `expr`
#' when module.
#' @export
#' @seealso [base::within()], [teal_data_module()]
#' @examples
#' tdm <- teal_data_module(
#'   ui = function(id) div(id = shiny::NS(id)("div_id")),
#'   server = function(id) {
#'     shiny::moduleServer(id, function(input, output, session) {
#'       shiny::reactive(teal_data(IRIS = iris))
#'     })
#'   }
#' )
#' \dontrun{
#' within(tdm, IRIS <- subset(IRIS, Species == "virginica"))
#' }
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
