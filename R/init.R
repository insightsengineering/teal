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
#' `R6` object as returned by [teal.data::cdisc_data()], [teal.data::teal_data()],
#' [teal.data::cdisc_dataset()], [teal.data::dataset()], [teal.data::dataset_connector()] or
#' [teal.data::cdisc_dataset_connector()] or a single `data.frame` or a `MultiAssayExperiment`
#' or a list of the previous objects or function returning a named list.
#' NOTE: teal does not guarantee reproducibility of the code when names of the list elements
#' do not match the original object names. To ensure reproducibility please use [teal.data::teal_data()]
#' or [teal.data::cdisc_data()] with `check = TRUE` enabled.
#' @param modules (`list`, `teal_modules` or `teal_module`)\cr
#'   nested list of `teal_modules` or `teal_module` objects or a single
#'   `teal_modules` or `teal_module` object. These are the specific output modules which
#'   will be displayed in the teal application. See [modules()] and [module()] for
#'   more details.
#' @param title (`NULL` or `character`)\cr
#'   The browser window title (defaults to the host URL of the page).
#' @param filter (`teal_slices`)\cr
#'   Specification of initial filter. Filters can be specified using [teal::teal_filters()].
#'   Old way of specifying filters through a list is deprecated and will be removed in the
#'   next release. Please fix your applications to use [teal::teal_filters()].
#' @param header (`shiny.tag` or `character`) \cr
#'   the header of the app. Note shiny code placed here (and in the footer
#'   argument) will be placed in the app's `ui` function so code which needs to be placed in the `ui` function
#'   (such as loading css via [htmltools::htmlDependency()]) should be included here.
#' @param footer (`shiny.tag` or `character`)\cr
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
#'       label = "data source",
#'       server = function(input, output, session, data) {},
#'       ui = function(id, ...) div(p("information about data source")),
#'       filters = "all"
#'     ),
#'     example_module(label = "example teal module"),
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
#'   filter = teal:::teal_filters(
#'     teal.slice:::filter_var("ADSL", "AGE"),
#'     teal.slice:::filter_var("ADSL", "SEX"),
#'     teal.slice:::filter_var("ADSL", "RACE"),
#'     exclude_varnames = list(ADSL = setdiff(names(ADSL), c("AGE", "SEX", "RACE"))),
#'     mapping = list(
#'       `example teal module` = "ADSL_RACE",
#'       `ADSL AGE histogram` = "ADSL_AGE",
#'       global_filters = "ADSL_SEX"
#'     )
#'   ),
#'   header = tags$h1("Sample App"),
#'   footer = tags$p("Copyright 2017 - 2020")
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
init <- function(data,
                 modules,
                 title = NULL,
                 filter = teal_filters(),
                 header = tags$p(),
                 footer = tags$p(),
                 id = character(0)) {
  logger::log_trace("init initializing teal app with: data ({ class(data)[1] }).")
  data <- teal.data::to_relational_data(data = data)

  checkmate::assert_class(data, "TealData")
  checkmate::assert_multi_class(modules, c("teal_module", "list", "teal_modules"))
  checkmate::assert_string(title, null.ok = TRUE)
  checkmate::assert(
    checkmate::check_class(filter, "teal_slices"),
    checkmate::check_list(filter, names = "named")
  )
  if (!teal.slice:::is.teal_slices(filter)) {
    checkmate::assert_subset(names(filter), choices = teal.data::get_dataname(data))
  }
  checkmate::assert_multi_class(header, c("shiny.tag", "character"))
  checkmate::assert_multi_class(footer, c("shiny.tag", "character"))
  checkmate::assert_character(id, max.len = 1, any.missing = FALSE)

  teal.logger::log_system_info()

  if (is(modules, "teal_module")) {
    modules <- list(modules)
  }
  if (is(modules, "list")) {
    modules <- do.call(teal::modules, modules)
  }

  if (!inherits(filter, "teal_slices")) {
    filter <- teal.slice:::as.teal_slices(filter)
  }

  # check teal_slices
  for (i in seq_along(filter)) {
    dataname_i <- shiny::isolate(filter[[i]]$dataname)
    if (!dataname_i %in% teal.data::get_dataname(data)) {
      stop(
        sprintf(
          "filter[[%s]] has a different dataname than available in a 'data':\n %s not in %s",
          i,
          dataname_i,
          toString(teal.data::get_dataname(data))
        )
      )
    }
  }

  if (isFALSE(attr(filter, "global"))) {
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
      stop(
        sprintf(
          "Module labels should be unique when teal_filters(mapping = TRUE). Duplicated labels:\n%s ",
          toString(module_names[duplicated(module_names)])
        )
      )
    }
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
