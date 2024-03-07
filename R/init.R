# This is the main function from teal to be used by the end-users. Although it delegates
# directly to `module_teal_with_splash.R`, we keep it in a separate file because its documentation is quite large
# and it is very end-user oriented. It may also perform more argument checking with more informative
# error messages.

#' Create the server and UI function for the `shiny` app
#'
#' @description `r lifecycle::badge("stable")`
#'
#' End-users: This is the most important function for you to start a
#' `teal` app that is composed of `teal` modules.
#'
#' @details
#' When initializing the `teal` app, if `datanames` are not set for the `teal_data` object,
#' defaults from the `teal_data` environment will be used.
#'
#' @param data (`teal_data` or `teal_data_module`)
#' For constructing the data object, refer to [teal_data()] and [teal_data_module()].
#' @param modules (`list` or `teal_modules` or `teal_module`)
#'   nested list of `teal_modules` or `teal_module` objects or a single
#'   `teal_modules` or `teal_module` object. These are the specific output modules which
#'   will be displayed in the `teal` application. See [modules()] and [module()] for
#'   more details.
#' @param filter (`teal_slices`)
#'   Specifies the initial filter using [teal_slices()].
#' @param title (`shiny.tag` or `character(1)`)
#'   The browser window title. Defaults to a title "teal app" with the icon of NEST.
#'   Can be created using the `build_app_title()` or
#'   by passing a valid `shiny.tag` which is a head tag with title and link tag.
#' @param header (`shiny.tag` or `character(1)`)
#'   The header of the app.
#' @param footer (`shiny.tag` or `character(1)`)
#'   The footer of the app.
#' @param id (`character`)
#'   Optional string specifying the `shiny` module id in cases it is used as a `shiny` module
#'   rather than a standalone `shiny` app. This is a legacy feature.
#'
#' @return Named list with server and UI functions.
#'
#' @export
#'
#' @include modules.R
#'
#' @examples
#' app <- init(
#'   data = teal_data(
#'     new_iris = transform(iris, id = seq_len(nrow(iris))),
#'     new_mtcars = transform(mtcars, id = seq_len(nrow(mtcars))),
#'     code = "
#'       new_iris <- transform(iris, id = seq_len(nrow(iris)))
#'       new_mtcars <- transform(mtcars, id = seq_len(nrow(mtcars)))
#'     "
#'   ),
#'   modules = modules(
#'     module(
#'       label = "data source",
#'       server = function(input, output, session, data) {},
#'       ui = function(id, ...) div(p("information about data source")),
#'       datanames = "all"
#'     ),
#'     example_module(label = "example teal module"),
#'     module(
#'       "Iris Sepal.Length histogram",
#'       server = function(input, output, session, data) {
#'         output$hist <- renderPlot(
#'           hist(data()[["new_iris"]]$Sepal.Length)
#'         )
#'       },
#'       ui = function(id, ...) {
#'         ns <- NS(id)
#'         plotOutput(ns("hist"))
#'       },
#'       datanames = "new_iris"
#'     )
#'   ),
#'   filter = teal_slices(
#'     teal_slice(dataname = "new_iris", varname = "Species"),
#'     teal_slice(dataname = "new_iris", varname = "Sepal.Length"),
#'     teal_slice(dataname = "new_mtcars", varname = "cyl"),
#'     exclude_varnames = list(new_iris = c("Sepal.Width", "Petal.Width")),
#'     module_specific = TRUE,
#'     mapping = list(
#'       `example teal module` = "new_iris Species",
#'       `Iris Sepal.Length histogram` = "new_iris Species",
#'       global_filters = "new_mtcars cyl"
#'     )
#'   ),
#'   title = "App title",
#'   header = tags$h1("Sample App"),
#'   footer = tags$p("Copyright 2017 - 2023")
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
init <- function(data,
                 modules,
                 filter = teal_slices(),
                 title = build_app_title(),
                 header = tags$p(),
                 footer = tags$p(),
                 id = character(0)) {
  logger::log_trace("init initializing teal app with: data ('{ class(data) }').")

  # argument checking (independent)
  ## `data`
  if (inherits(data, "TealData")) {
    lifecycle::deprecate_stop(
      when = "0.15.0",
      what = "init(data)",
      paste(
        "TealData is no longer supported. Use teal_data() instead.",
        "Please follow migration instructions https://github.com/insightsengineering/teal/discussions/988."
      )
    )
  }
  checkmate::assert_multi_class(data, c("teal_data", "teal_data_module"))

  ## `modules`
  checkmate::assert(
    .var.name = "modules",
    checkmate::check_multi_class(modules, c("teal_modules", "teal_module")),
    checkmate::check_list(modules, min.len = 1, any.missing = FALSE, types = c("teal_module", "teal_modules"))
  )
  if (inherits(modules, "teal_module")) {
    modules <- list(modules)
  }
  if (checkmate::test_list(modules, min.len = 1, any.missing = FALSE, types = c("teal_module", "teal_modules"))) {
    modules <- do.call(teal::modules, modules)
  }

  ## `filter`
  checkmate::assert_class(filter, "teal_slices")

  ## all other arguments
  checkmate::assert(
    .var.name = "title",
    checkmate::check_string(title),
    checkmate::check_multi_class(title, c("shiny.tag", "shiny.tag.list", "html"))
  )
  checkmate::assert(
    .var.name = "header",
    checkmate::check_string(header),
    checkmate::check_multi_class(header, c("shiny.tag", "shiny.tag.list", "html"))
  )
  checkmate::assert(
    .var.name = "footer",
    checkmate::check_string(footer),
    checkmate::check_multi_class(footer, c("shiny.tag", "shiny.tag.list", "html"))
  )
  checkmate::assert_character(id, max.len = 1, any.missing = FALSE)

  # log
  teal.logger::log_system_info()

  # argument transformations
  ## `modules` - landing module
  landing <- extract_module(modules, "teal_module_landing")
  landing_module <- NULL
  if (length(landing) == 1L) {
    landing_module <- landing[[1L]]
    modules <- drop_module(modules, "teal_module_landing")
  } else if (length(landing) > 1L) {
    stop("Only one `landing_popup_module` can be used.")
  }

  ## `filter` - app_id attribute
  attr(filter, "app_id") <- create_app_id(data, modules)

  ## `filter` - convert teal.slice::teal_slices to teal::teal_slices
  filter <- as.teal_slices(as.list(filter))

  # argument checking (interdependent)
  ## `filter` - `modules`
  if (isTRUE(attr(filter, "module_specific"))) {
    module_names <- unlist(c(module_labels(modules), "global_filters"))
    failed_mod_names <- setdiff(names(attr(filter, "mapping")), module_names)
    if (length(failed_mod_names)) {
      stop(
        sprintf(
          "Some module names in the mapping arguments don't match module labels.\n %s not in %s",
          toString(failed_mod_names),
          toString(unique(module_names))
        )
      )
    }

    if (anyDuplicated(module_names)) {
      # In teal we are able to set nested modules with duplicated label.
      # Because mapping argument bases on the relationship between module-label and filter-id,
      # it is possible that module-label in mapping might refer to multiple teal_module (identified by the same label)
      stop(
        sprintf(
          "Module labels should be unique when teal_slices(mapping = TRUE). Duplicated labels:\n%s ",
          toString(module_names[duplicated(module_names)])
        )
      )
    }
  }

  ## `data` - `modules`
  if (inherits(data, "teal_data")) {
    if (length(teal_data_datanames(data)) == 0) {
      stop("The environment of `data` is empty.")
    }
    # in case of teal_data_module this check is postponed to the srv_teal_with_splash
    is_modules_ok <- check_modules_datanames(modules, teal_data_datanames(data))
    if (!isTRUE(is_modules_ok)) {
      logger::log_error(is_modules_ok)
      checkmate::assert(is_modules_ok, .var.name = "modules")
    }

    is_filter_ok <- check_filter_datanames(filter, teal_data_datanames(data))
    if (!isTRUE(is_filter_ok)) {
      warning(is_filter_ok)
      # we allow app to continue if applied filters are outside
      # of possible data range
    }
  }

  # Note regarding case `id = character(0)`:
  # rather than creating a submodule of this module, we directly modify
  # the UI and server with `id = character(0)` and calling the server function directly
  res <- list(
    ui = ui_teal_with_splash(id = id, data = data, title = title, header = header, footer = footer),
    server = function(input, output, session) {
      if (!is.null(landing_module)) {
        do.call(landing_module$server, c(list(id = "landing_module_shiny_id"), landing_module$server_args))
      }
      srv_teal_with_splash(id = id, data = data, modules = modules, filter = deep_copy_filter(filter))
    }
  )

  logger::log_trace("init teal app has been initialized.")

  res
}
