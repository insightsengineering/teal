# This is the main function from teal to be used by the end-users. Although it delegates
# directly to `module_teal_with_splash.R`, we keep it in a separate file because its doc is quite large
# and it is very end-user oriented. It may also perform more argument checking with more informative
# error messages.


#' Create the Server and UI Function For the Shiny App
#'
#' @description `r lifecycle::badge("stable")`
#' End-users: This is the most important function for you to start a
#' teal app that is composed out of teal modules.
#'
#' **Notes for developers**:
#' This is a wrapper function around the `module_teal.R` functions. Unless you are
#' an end-user, don't use this function, but instead this module.
#'
#' @param data (`TealData` or `TealDataset` or `TealDatasetConnector` or `list` or `data.frame`
#' or `MultiAssayExperiment`)\cr
#' `R6` object as returned by [teal.data::cdisc_data()], [teal.data::teal_data()], [teal.data::cdisc_dataset()], [teal.data::dataset()],
#' [teal.data::dataset_connector()] or [teal.data::cdisc_dataset_connector()] or a single `data.frame` or a `MultiAssayExperiment`
#' or a list of the previous objects or function returning a named list.
#' NOTE: teal does not guarantee reproducibility of the code when names of the list elements
#' do not match the original object names. To ensure reproducibility please use [teal.data::teal_data()]
#' or [teal.data::cdisc_data()] with `check = TRUE` enabled.
#' @param modules (`list` or `teal_modules`)\cr
#'   nested list of `teal_modules` or `module` objects. See [modules()] and [module()] for
#'   more details.
#' @param title (`NULL` or `character`)\cr
#'   The browser window title (defaults to the host URL of the page).
#' @param filter (`list`)\cr
#'   You can define filters that show when the app starts. List names should be
#'   named according to datanames passed to the `data` argument.
#'   In case of  data.frame` the list should be composed as follows:
#'   ```
#'   list(<dataname1> = list(<varname1> = ..., <varname2> = ...),
#'        <dataname2> = list(...),
#'        ...)
#'
#'   ```
#'
#'   For example, filters for variable `Sepal.Length` in `iris` can be specified as
#'   follows:
#'   ```
#'   list(iris = list(Sepal.Length = list(selected = c(5.0, 7.0))))
#'   # or
#'   list(iris = list(Sepal.Length = c(5.0, 7.0)))
#'   ```
#'
#'   In case developer would like to include `NA` and `Inf` values in  the
#'   filtered dataset.
#'   ```
#'   list(Species = list(selected = c(5.0, 7.0), keep_na = TRUE, keep_inf = TRUE))
#'   list(Species = c(c(5.0, 7.0), NA, Inf))
#'   ```
#'
#'   To initialize with specific variable filter with all values on start, one
#'   can use
#'   ```
#'   list(Species = list())
#'   ```
#'   `filter` should be set with respect to the class of the column:
#'   * `numeric`: `selected` should be a two elements vector defining the range
#'   of the filter.
#'   * `Date`: `selected` should be a two elements vector defining the date-range
#'   of the filter
#'   * `POSIXct`: `selected` should be a two elements vector defining the
#'   `datetime` range of the filter
#'   * `character` and `factor`: `selected` should be a vector of any length
#'   defining initial values selected to filter.
#'   \cr
#'   `filter` for `MultiAssayExperiment` objects should be specified in slightly
#'   different way. Since it contains patient data with list of experiments,
#'   `filter` list should be created as follows:
#'   \cr
#'
#'   ```
#'   list(
#'     <MAE dataname> = list(
#'       subjects = list(<column in colData> = ..., <column in colData> = ...),
#'       <experiment name> = list(
#'         subset = list(<column in rowData of experiment> = ...,
#'                       <column in rowData of experiment> = ...),
#'         select = list(<column in colData of experiment> = ...,
#'                       <column in colData of experiment> = ...)
#'       )
#'     )
#'   )
#'   ```
#' @param header (`character` or `shiny.tag`) \cr
#'   the header of the app. Note shiny code placed here (and in the footer
#'   argument) will be placed in the app's `ui` function so code which needs to be placed in the `ui` function
#'   (such as loading css via [htmltools::htmlDependency()]) should be included here.
#' @param footer (`character` or `shiny.tag`)\cr
#'   the footer of the app
#' @param id (`character`)\cr
#'   module id to embed it, if provided,
#'   the server function must be called with [shiny::moduleServer()];
#'   See the vignette for an example. However, [ui_teal_with_splash()]
#'   is then preferred to this function.
#'
#' @return named list with server and ui function
#'
#' @export
#'
#' @include modules.R
#'
#' @examples
#' library(scda)
#'
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     code = "ADSL <- synthetic_cdisc_data(\"latest\")$adsl"
#'   ),
#'   modules = modules(
#'     module(
#'       "data source",
#'       server = function(input, output, session, data) {},
#'       ui = function(id, ...) div(p("information about data source")),
#'       filters = "all"
#'     ),
#'     example_module(),
#'     module(
#'       "ADSL AGE histogram",
#'       server = function(input, output, session, data) {
#'         output$hist <- renderPlot(
#'           hist(data[["ADSL"]]()$AGE)
#'         )
#'       },
#'       ui = function(id, ...) {
#'         ns <- NS(id)
#'         plotOutput(ns("hist"))
#'       },
#'       filters = "ADSL"
#'     )
#'   ),
#'   title = "App title",
#'   filter = list(ADSL = list(AGE = list())),
#'   header = tags$h1("Sample App"),
#'   footer = tags$p("Copyright 2017 - 2020")
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
#' # See the vignette for an example how to embed this app as a module
#' # into a larger application
init <- function(data,
                 modules,
                 title = NULL,
                 filter = list(),
                 header = tags$p("Add Title Here"),
                 footer = tags$p("Add Footer Here"),
                 id = character(0)) {
  logger::log_trace("init initializing teal app with: data ({ class(data)[1] }).")
  data <- teal.data::to_relational_data(data = data)

  checkmate::assert_string(title, null.ok = TRUE)
  checkmate::assert_class(data, "TealData")
  checkmate::check_list(modules)
  checkmate::check_class(modules, "teal_modules")
  checkmate::assert_list(filter, min.len = 0, names = "unique")
  checkmate::assert_subset(names(filter), choices = teal.data::get_dataname(data))
  checkmate::assert_character(id, max.len = 1, any.missing = FALSE)

  teal.logger::log_system_info()

  if (is(modules, "list")) {
    modules <- do.call(teal::modules, modules)
  }

  # Note regarding case `id = character(0)`:
  # rather than using `callModule` and creating a submodule of this module, we directly modify
  # the `ui` and `server` with `id = character(0)` and calling the server function directly
  # rather than through `callModule`
  res <- list(
    ui = ui_teal_with_splash(id = id, data = data, title = title, header = header, footer = footer),
    server = function(input, output, session) {
      # copy object so that load won't be shared between the session
      data <- data$copy(deep = TRUE)
      srv_teal_with_splash(id = id, data = data, modules = modules, filter = filter)
    }
  )
  logger::log_trace("init teal app has been initialized.")
  return(res)
}


#' Make a Shiny UI function bookmarkable
#'
#' @description `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated and will be removed in a future release of `teal`.
#'
#' This is a customization of `shinyApp`.
#'
#' To be bookmarkable, the Shiny UI function must have an
#' argument `request`. This function ensures this.
#'
#' When `ui` is a function, it passes the following to `shinyApp`
#' ```
#' app <- teal::init(....)
#' ui <- app$ui
#' ui_new <- function(request) {
#'   ui() # or just `ui` when ui is already evaluated, e.g. `shiny.tag`
#' }
#' ```
#'
#' If no bookmarking is needed for teal apps, then you can also call
#' `shinyApp(ui = app$ui, server = app$server)`, where `app` is returned
#' by `init()`.
#'
#' **For Developers: **
#' The reason you cannot
#' call `shinyApp(ui = app$ui, server = app$server)` without parentheses is
#' that `app$ui` has an `id` argument with a default value which makes it
#' possible to be added into modules. `shinyApp` thinks that this is the request
#' argument which is needed for bookmarking. This avoids it.
#'
#' We guarantee that anything that can be run with `shinyApp` can be replaced
#' by a call to this function without any changes.
#'
#' @param ui `function or shiny.tag` Shiny UI; either a
#'   `shiny.tag` or a function with no argument or
#'   one argument (`request`)
#' @param server `function` Shiny server function
#' @param ... additional arguments to `shinyApp`
#' @return `shinyApp` value
#' @export
bookmarkableShinyApp <- function(ui, server, ...) { # nolint

  # Note when this function is removed code to allow bookmarking in srv_teal
  # should also be removed.
  lifecycle::deprecate_soft(
    when = "0.12.0",
    what = "bookmarkableShinyApp()",
    details = "In future releases teal will stop supporting shiny bookmarking"
  )

  # ui must be a function of request to be bookmarkable
  ui_new <- function(request) {
    # we use similar logic to `shiny:::uiHttpHandler`
    if (is.function(ui)) {
      # evaluating ui with default arguments
      ui()
    } else {
      checkmate::assert(
        checkmate::check_class(ui, "shiny.tag"),
        checkmate::check_class(ui, "shiny.tag.list"),
        checkmate::check_class(ui, "html")
      )
      ui
    }
  }
  return(shinyApp(ui = ui_new, server = server, ...))
}
