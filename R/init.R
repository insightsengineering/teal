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
#' @param data (`teal_data` or `teal_data_module`)
#'   For constructing the data object, refer to [teal.data::teal_data()] and [teal_data_module()].
#' @param modules (`list` or `teal_modules` or `teal_module`)
#'   Nested list of `teal_modules` or `teal_module` objects or a single
#'   `teal_modules` or `teal_module` object. These are the specific output modules which
#'   will be displayed in the `teal` application. See [modules()] and [module()] for
#'   more details.
#' @param filter (`teal_slices`) Optionally,
#'   specifies the initial filter using [teal_slices()].
#' @param title (`shiny.tag` or `character(1)`) Optionally,
#'   the browser window title. Defaults to a title "teal app" with the icon of NEST.
#'   Can be created using the `build_app_title()` or
#'   by passing a valid `shiny.tag` which is a head tag with title and link tag.
#' @param header (`shiny.tag` or `character(1)`) Optionally,
#'   the header of the app.
#' @param footer (`shiny.tag` or `character(1)`) Optionally,
#'   the footer of the app.
#' @param id (`character`) Optionally,
#'   a string specifying the `shiny` module id in cases it is used as a `shiny` module
#'   rather than a standalone `shiny` app. This is a legacy feature.
#' @param landing_popup (`teal_module_landing`) Optionally,
#'   a `landing_popup_module` to show up as soon as the teal app is initialized.
#'
#' @return Named list containing server and UI functions.
#'
#' @export
#'
#' @include modules.R
#'
#' @examples
#' app <- init(
#'   data = within(
#'     teal_data(),
#'     {
#'       new_iris <- transform(iris, id = seq_len(nrow(iris)))
#'       new_mtcars <- transform(mtcars, id = seq_len(nrow(mtcars)))
#'     }
#'   ),
#'   modules = modules(
#'     module(
#'       label = "data source",
#'       server = function(input, output, session, data) {},
#'       ui = function(id, ...) tags$div(p("information about data source")),
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
#'   footer = tags$p("Sample footer")
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
                 id = character(0),
                 landing_popup = NULL) {
  logger::log_debug("init initializing teal app with: data ('{ class(data) }').")

  # argument checking (independent)
  ## `data`
  checkmate::assert_multi_class(data, c("teal_data", "teal_data_module"))
  checkmate::assert_class(landing_popup, "teal_module_landing", null.ok = TRUE)

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
  if (length(landing) == 1L) {
    landing_popup <- landing[[1L]]
    modules <- drop_module(modules, "teal_module_landing")
    lifecycle::deprecate_soft(
      when = "0.15.3",
      what = "landing_popup_module()",
      details = paste(
        "Pass `landing_popup_module` to the `landing_popup` argument of the `init` ",
        "instead of wrapping it into `modules()` and passing to the `modules` argument"
      )
    )
  } else if (length(landing) > 1L) {
    stop("Only one `landing_popup_module` can be used.")
  }

  ## `filter` - set app_id attribute unless present (when restoring bookmark)
  if (is.null(attr(filter, "app_id", exact = TRUE))) attr(filter, "app_id") <- create_app_id(data, modules)

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
    if (length(data) == 0) {
      stop("The environment of `data` is empty.")
    }

    is_modules_ok <- check_modules_datanames(modules, names(data))
    if (!isTRUE(is_modules_ok) && length(unlist(extract_transformators(modules))) == 0) {
      warning(is_modules_ok, call. = FALSE)
    }

    is_filter_ok <- check_filter_datanames(filter, names(data))
    if (!isTRUE(is_filter_ok)) {
      warning(is_filter_ok)
      # we allow app to continue if applied filters are outside
      # of possible data range
    }
  }

  reporter <- teal.reporter::Reporter$new()$set_id(attr(filter, "app_id"))
  if (is_arg_used(modules, "reporter") && length(extract_module(modules, "teal_module_previewer")) == 0) {
    modules <- append_module(
      modules,
      reporter_previewer_module(server_args = list(previewer_buttons = c("download", "reset")))
    )
  }

  ns <- NS(id)
  # Note: UI must be a function to support bookmarking.
  res <- list(
    ui = function(request) {
      ui_teal(
        id = ns("teal"),
        modules = modules,
        title = title,
        header = header,
        footer = footer
      )
    },
    server = function(input, output, session) {
      if (!is.null(landing_popup)) {
        do.call(landing_popup$server, c(list(id = "landing_module_shiny_id"), landing_popup$server_args))
      }
      srv_teal(id = ns("teal"), data = data, modules = modules, filter = deep_copy_filter(filter))
    }
  )

  logger::log_debug("init teal app has been initialized.")

  res
}
