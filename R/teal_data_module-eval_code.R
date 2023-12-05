setOldClass("teal_data_module")

#' Evaluate the code in the qenv environment
#' @name eval_code
#' @description
#' Given code is evaluated in the `qenv` environment of `teal_data` reactive defined in `teal_data_module`.
#' @param object (`teal_data_module`)
#' @inheritParams teal.code::eval_code
#' @return Returns a `teal_data_module` object.
#' @importMethodsFrom teal.code eval_code
#' @importFrom methods setMethod
NULL

#' @rdname eval_code
#' @export
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
#' eval_code(tdm, "IRIS <- subset(IRIS, Species == 'virginica')")
setMethod("eval_code", signature = c("teal_data_module", "character"), function(object, code) {
  teal_data_module(
    ui = function(id) {
      ns <- NS(id)
      object$ui(ns("mutate_inner"))
    },
    server = function(id) {
      moduleServer(id, function(input, output, session) {
        teal_data_rv <- object$server("mutate_inner")

        if (!is.reactive(teal_data_rv)) {
          stop("The `teal_data_module` must return a reactive expression.", call. = FALSE)
        }

        td <- eventReactive(teal_data_rv(),
          {
            if (inherits(teal_data_rv(), c("teal_data", "qenv.error"))) {
              eval_code(teal_data_rv(), code)
            } else {
              teal_data_rv()
            }
          },
          ignoreNULL = FALSE
        )
        td
      })
    }
  )
})

#' @rdname eval_code
#' @export
setMethod("eval_code", signature = c("teal_data_module", "language"), function(object, code) {
  eval_code(object, code = paste(lang2calls(code), collapse = "\n"))
})

#' @rdname eval_code
#' @export
setMethod("eval_code", signature = c("teal_data_module", "expression"), function(object, code) {
  eval_code(object, code = paste(lang2calls(code), collapse = "\n"))
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
#' within(tdm, IRIS <- subset(IRIS, Species == "virginica"))
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
