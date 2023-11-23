#' Data module for `teal` applications
#'
#' Creates `teal_data_module` object - a `shiny` module to supply or modify data in a `teal` application.
#'
#' This function creates a `shiny` module that allows for running data pre-processing code after the app starts.
#' The body of the server function will be run in the app rather than in the global environment.
#' This means it will be run every time the app starts, so use sparingly.
#'
#' Pass this module instead of a `teal_data` object in a call to [init()].
#'
#' See vignette \code{vignette("data-as-shiny-module", package = "teal")} for more details.
#'
#' @param ui (`function(id)`)\cr
#'  `shiny` module `ui` function; must only take `id` argument
#' @param server (`function(id)`)\cr
#'  `shiny` module `ui` function; must only take `id` argument;
#'  must return reactive expression containing `teal_data` object
#'
#' @return Object of class `teal_data_module`.
#'
#' @examples
#' data <- teal_data_module(
#'   ui = function(id) {
#'     ns <- NS(id)
#'     actionButton(ns("submit"), label = "Load data")
#'   },
#'   server = function(id) {
#'     moduleServer(id, function(input, output, session) {
#'       eventReactive(input$submit, {
#'         data <- within(
#'           teal_data(),
#'           {
#'             dataset1 <- iris
#'             dataset2 <- mtcars
#'           }
#'         )
#'         datanames(data) <- c("iris", "mtcars")
#'
#'         data
#'       })
#'     })
#'   }
#' )
#' @export
teal_data_module <- function(ui, server) {
  checkmate::assert_function(ui, args = "id", nargs = 1)
  checkmate::assert_function(server, args = "id", nargs = 1)
  structure(
    list(ui = ui, server = server),
    class = "teal_data_module"
  )
}

setOldClass("teal_data_module")

#' @name eval_code
#' @inherit teal.code::eval_code
#' @importMethodsFrom teal.code eval_code
#' @export
setMethod("eval_code", signature = c("teal_data_module", "character"), function(object, code) {
  teal_data_module(
    ui = function(id) {
      ns <- NS(id)
      object$ui(ns("mutate_inner"))
    },
    server = function(id) {
      moduleServer(id, function(input, output, session) {
        data <- object$server("mutate_inner")
        eventReactive(data(), {
          eval_code(data(), code)
        })
      })
    }
  )
})

#' @rdname eval_code
#' @export
setMethod("eval_code", signature = c("teal_data_module", "language"), function(object, code) {
  eval_code(object, code = teal.code:::format_expression(code))
})

#' @rdname eval_code
#' @export
setMethod("eval_code", signature = c("teal_data_module", "expression"), function(object, code) {
  eval_code(object, code = teal.code:::format_expression(code))
})

#' @inherit teal.code::within.qenv
#' @export
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
