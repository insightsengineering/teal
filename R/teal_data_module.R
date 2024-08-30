#' Data module for `teal` applications
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Create a `teal_data_module` object and evaluate code on it with history tracking.
#'
#' @details
#' `teal_data_module` creates a `shiny` module to interactively supply or modify data in a `teal` application.
#' The module allows for running any code (creation _and_ some modification) after the app starts or reloads.
#' The body of the server function will be run in the app rather than in the global environment.
#' This means it will be run every time the app starts, so use sparingly.
#'
#' Pass this module instead of a `teal_data` object in a call to [init()].
#' Note that the server function must always return a `teal_data` object wrapped in a reactive expression.
#'
#' See vignette `vignette("data-as-shiny-module", package = "teal")` for more details.
#'
#' @param ui (`function(id)`)
#'  `shiny` module UI function; must only take `id` argument
#' @param server (`function(id)`)
#'  `shiny` module server function; must only take `id` argument;
#'  must return reactive expression containing `teal_data` object
#' @param label (`character(1)`) Label of the module.
#' @param once (`logical(1)`)
#'  If `TRUE`, the data module will be shown only once and will disappear after successful data loading.
#'  App user will no longer be able to interact with this module anymore.
#'  If `FALSE`, the data module can be reused multiple times.
#'  App user will be able to interact and change the data output from the module multiple times.
#'
#' @return
#' `teal_data_module` returns a list of class `teal_data_module` containing two elements, `ui` and
#' `server` provided via arguments.
#'
#' @examples
#' tdm <- teal_data_module(
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
#'         datanames(data) <- c("dataset1", "dataset2")
#'
#'         data
#'       })
#'     })
#'   }
#' )
#'
#' @name teal_data_module
#' @seealso [`teal.data::teal_data-class`], [teal.code::qenv()]
#'
#' @export
teal_data_module <- function(ui, server, label = "data module", once = TRUE) {
  checkmate::assert_function(ui, args = "id", nargs = 1)
  checkmate::assert_function(server, args = "id", nargs = 1)
  checkmate::assert_string(label)
  checkmate::assert_flag(once)
  structure(
    list(
      ui = ui,
      server = function(id) {
        data_out <- server(id)
        decorate_err_msg(
          assert_reactive(data_out),
          pre = sprintf("From: 'teal_data_module()':\nA 'teal_data_module' with \"%s\" label:", label),
          post = "Please make sure that this module returns a 'reactive` object containing 'teal_data' class of object." # nolint: line_length_linter.
        )
        reactive({
          new_data <- data_out()
          if (inherits(new_data, "teal_data") && !length(teal.data::datanames(new_data))) {
            teal.data::datanames(new_data) <- .teal_data_ls(new_data)
          }
          new_data
        })
      }
    ),
    label = label,
    class = "teal_data_module",
    once = once
  )
}

#' Data module for `teal` transformers.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Create a `teal_data_module` object for custom transformation of data for pre-processing
#' before passing the data into the module.
#'
#' @details
#' `teal_transform_module` creates a [`teal_data_module`] object to transform data in a `teal`
#' application. This transformation happens after the data has passed through the filtering activity
#' in teal. The transformed data is then sent to the server of the [teal_module()].
#'
#' See vignette `vignette("data-transform-as-shiny-module", package = "teal")` for more details.
#'
#'
#' @inheritParams teal_data_module
#' @param server (`function(id, data)`)
#' `shiny` module server function; that takes `id` and `data` argument,
#' where the `id` is the module id and `data` is the reactive `teal_data` input.
#' The server function must return reactive expression containing `teal_data` object.
#' @param datanames (`character`)
#'  Names of the datasets that are relevant for the item. The filter panel will only display filters
#'  for specified `datanames`. The keyword `"all"` will show filters of all datasets. `datanames`
#'  will be automatically appended to the [modules()] `datanames`.
#' @examples
#' my_transformers <- list(
#'   teal_transform_module(
#'     label = "Custom transform for iris",
#'     datanames = "iris",
#'     ui = function(id) {
#'       ns <- NS(id)
#'       tags$div(
#'         numericInput(ns("n_rows"), "Subset n rows", value = 6, min = 1, max = 150, step = 1)
#'       )
#'     },
#'     server = function(id, data) {
#'       moduleServer(id, function(input, output, session) {
#'         reactive({
#'           within(data(),
#'             {
#'               iris <- head(iris, num_rows)
#'             },
#'             num_rows = input$n_rows
#'           )
#'         })
#'       })
#'     }
#'   )
#' )
#'
#' @name teal_transform_module
#'
#' @export
teal_transform_module <- function(ui, server, label = "transform module", datanames = character(0)) {
  checkmate::assert_function(ui, args = "id", nargs = 1)
  checkmate::assert_function(server, args = c("id", "data"), nargs = 2)
  checkmate::assert_string(label)
  checkmate::assert_character(datanames)
  structure(
    list(
      ui = ui,
      server = function(id, data) {
        data_out <- server(id, data)
        decorate_err_msg(
          assert_reactive(data_out),
          pre = sprintf("From: 'teal_transform_module()':\nA 'teal_transform_module' with \"%s\" label:", label),
          post = "Please make sure that this module returns a 'reactive` object containing 'teal_data' class of object." # nolint: line_length_linter.
        )
      }
    ),
    label = label,
    datanames = datanames,
    class = c("teal_transform_module", "teal_data_module")
  )
}


#' Extract all `transformers` from `modules`.
#'
#' @param modules `teal_modules` or `teal_module`
#' @return A list of `teal_transform_module` nested in the same way as input `modules`.
#' @keywords internal
extract_transformers <- function(modules) {
  if (inherits(modules, "teal_module")) {
    modules$transformers
  } else if (inherits(modules, "teal_modules")) {
    lapply(modules$children, extract_transformers)
  }
}
