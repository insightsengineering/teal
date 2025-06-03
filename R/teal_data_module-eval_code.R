setOldClass("teal_data_module")

#' Evaluate code on `teal_data_module`
#'
#' @details
#' `eval_code` evaluates given code in the environment of the `teal_data` object created by the `teal_data_module`.
#' The code is added to the `@code` slot of the `teal_data`.
#'
#' @param object (`teal_data_module`)
#' @inheritParams teal.code::eval_code
#'
#' @return
#' `eval_code` returns a `teal_data_module` object with a delayed evaluation of `code` when the module is run.
#'
#' @examples
#' eval_code(tdm, "dataset1 <- subset(dataset1, Species == 'virginica')")
#'
#' @include teal_data_module.R
#' @name eval_code
#' @rdname teal_data_module
#' @aliases eval_code,teal_data_module
#'
#' @importFrom methods setMethod
#' @importMethodsFrom teal.code eval_code
#'
setMethod("eval_code", signature = c(object = "teal_data_module"), function(object, code) {
  teal_data_module(
    ui = function(id) {
      ns <- NS(id)
      object$ui(ns("mutate_inner"))
    },
    server = function(id) {
      moduleServer(id, function(input, output, session) {
        data <- object$server("mutate_inner")
        td <- eventReactive(data(),
          {
            if (inherits(data(), c("teal_data", "qenv.error"))) {
              eval_code(data(), code)
            } else {
              data()
            }
          },
          ignoreNULL = FALSE
        )
        td
      })
    }
  )
})
