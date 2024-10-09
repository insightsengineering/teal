#' Data module for `teal` transformers.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `teal_transform_module` creates a shiny-module to transform data in a `teal` application.
#'
#' # Transforming `teal` module's input
#'
#' This transformation happens after the data has passed through the filtering activity in teal. The
#' transformed data is then sent to the server of the [teal_module()]. Process is handled by `teal`
#' internals.
#'
#' See vignette `vignette("data-transform-as-shiny-module", package = "teal")` for more details.
#'
#' # Decorating `teal` module's output
#'
#' `teal_transform_module`'s purpose is to modify any object created in [`teal.data::teal_data`]. It means that an
#' app-developer can use `teal_transform_module` to modify data but also outputted tables, listings and graphs.
#' Some [`teal_modules`] enables app developer to inject custom shiny module to modify displayed output.
#' To handle these `decorators` inside of your module use [ui_teal_transform_module()] and [srv_teal_transform_module].
#' (todo: write more about how to handle decorators: they need to go through ui_args/srv_args and then be consumed by
#' ui/srv_teal_transform_module()... . Alternatively, decorators could be a [module()]'s argument)
#'
#' # `server` as a language
#'
#' Server function in `teal_transform_module` must return `reactive` containing [teal.data::teal_data] object.
#' Consider sinmple transformer which doesn't require any advanced reactivity, example `server` might have a
#' following form:
#'
#' ```
#' function(id, data) {
#'   moduleServer(id, function(input, output, session) {
#'     reactive({
#'       within(
#'         data(),
#'         expr = x <- subset(x, col == level),
#'         level = input$level
#'       )
#'     })
#'   })
#' }
#' ```
#'
#' Above can be simplified to presented below, where `level` will be automatically substituted with
#' respective input matched by its name.
#'
#' ```
#' make_teal_transform_module(expr = expression(x <- subset(x, col == level)))
#' ```
#' @inheritParams teal_data_module
#' @param server (`function(id, data)` or `language`)
#' `shiny` module server function; that takes `id` and `data` argument,
#' where the `id` is the module id and `data` is the reactive `teal_data` input.
#' The server function must return reactive expression containing `teal_data` object.
#' To simplify use [make_teal_transform_server()].
#' @param datanames (`character`)
#' Names of the datasets that are relevant for the module. The
#' filter panel will only display filters for specified `datanames`. The keyword `"all"` will show
#' filters of all datasets. `datanames` will be automatically appended to the [modules()] `datanames`.
#'
#'
#' @examples
#' my_transformers <- list(
#'   teal_transform_module(
#'     label = "Static transform for iris",
#'     datanames = "iris",
#'     server = function(id, data) {
#'       moduleServer(id, function(input, output, session) {
#'         reactive({
#'           within(data(), {
#'             iris <- head(iris, 5)
#'           })
#'         })
#'       })
#'     }
#'   ),
#'   teal_transform_module(
#'     label = "Interactive transform for iris",
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
teal_transform_module <- function(ui = NULL,
                                  server = function(id, data) data,
                                  label = "transform module",
                                  datanames = "all") {
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

#' Make teal_transform_module's server
#'
#' A factory function to simplify creation of a [`teal_transform_module`]'s server. Specified `expr`
#' is wrapped in a shiny module function and output can be passed to the `server` argument in
#' [teal_transform_module()] call. Such server function can be linked with ui and values from the
#' inputs can be used in the expression. Object names specified in the expression will be substituted
#' with the value of the respective input (matched by the name) - for example in
#' `expression(graph <- graph + ggtitle(title))` object `title` will be replaced with the value of
#' `input$title`.
#' @param expr (`language`)
#'  An R call which will be evaluated within [`teal.data::teal_data`] environment.
#' @return `function(id, data)` returning `shiny` module
#' @examples
#'
#' teal_transform_module(
#'   label = "Simplified interactive transform for iris",
#'   datanames = "iris",
#'   ui = function(id) {
#'     ns <- NS(id)
#'     numericInput(ns("n_rows"), "Subset n rows", value = 6, min = 1, max = 150, step = 1)
#'   },
#'   server = make_teal_transform_server(expression(iris <- head(iris, n_rows)))
#' )
#'
#' @export
make_teal_transform_server <- function(expr) {
  function(id, data) {
    moduleServer(id, function(input, output, session) {
      reactive({
        call_with_inputs <- lapply(expr, function(x) {
          do.call(
            what = substitute,
            args = list(expr = x, env = reactiveValuesToList(input))
          )
        })
        eval_code(object = data(), code = as.expression(call_with_inputs))
      })
    })
  }
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
