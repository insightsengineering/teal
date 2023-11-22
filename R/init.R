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
#' @param data (`TealData` or `TealDataset` or `TealDatasetConnector` or `list` or `data.frame`
#' or `MultiAssayExperiment`, `teal_data`, `teal_data_module`)\cr
#' `R6` object as returned by [teal.data::cdisc_data()], [teal.data::teal_data()],
#' [teal.data::cdisc_dataset()], [teal.data::dataset()], [teal.data::dataset_connector()] or
#' [teal.data::cdisc_dataset_connector()] or [teal_data_module()] or a single `data.frame` or
#' a `MultiAssayExperiment`
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
#'   Specification of initial filter. Filters can be specified using [teal::teal_slices()].
#'   Old way of specifying filters through a list is deprecated and will be removed in the
#'   next release. Please fix your applications to use [teal::teal_slices()].
#' @param header (`shiny.tag` or `character`) \cr
#'   the header of the app. Note shiny code placed here (and in the footer
#'   argument) will be placed in the app's `ui` function so code which needs to be placed in the `ui` function
#'   (such as loading `CSS` via [htmltools::htmlDependency()]) should be included here.
#' @param footer (`shiny.tag` or `character`)\cr
#'   the footer of the app
#' @param id (`character`)\cr
#'   module id to embed it, if provided,
#'   the server function must be called with [shiny::moduleServer()];
#'   See the vignette for an example. However, [ui_teal_with_splash()]
#'   is then preferred to this function.
#'
#' @return named list with `server` and `ui` function
#'
#' @export
#'
#' @include modules.R
#'
#' @examples
#' new_iris <- transform(iris, id = seq_len(nrow(iris)))
#' new_mtcars <- transform(mtcars, id = seq_len(nrow(mtcars)))
#'
#' app <- init(
#'   data = teal_data(
#'     dataset("new_iris", new_iris),
#'     dataset("new_mtcars", new_mtcars),
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
#'           hist(data[["new_iris"]]()$Sepal.Length)
#'         )
#'       },
#'       ui = function(id, ...) {
#'         ns <- NS(id)
#'         plotOutput(ns("hist"))
#'       },
#'       datanames = "new_iris"
#'     )
#'   ),
#'   title = "App title",
#'   filter = teal_slices(
#'     teal_slice(dataname = "new_iris", varname = "Species"),
#'     teal_slice(dataname = "new_iris", varname = "Sepal.Length"),
#'     teal_slice(dataname = "new_mtcars", varname = "cyl"),
#'     exclude_varnames = list(new_iris = c("Sepal.Width", "Petal.Width")),
#'     mapping = list(
#'       `example teal module` = "new_iris Species",
#'       `Iris Sepal.Length histogram` = "new_iris Species",
#'       global_filters = "new_mtcars cyl"
#'     )
#'   ),
#'   header = tags$h1("Sample App"),
#'   footer = tags$p("Copyright 2017 - 2023")
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
init <- function(data,
                 modules,
                 title = NULL,
                 filter = teal_slices(),
                 header = tags$p(),
                 footer = tags$p(),
                 id = character(0)) {
  logger::log_trace("init initializing teal app with: data ({ class(data)[1] }).")
  if (!inherits(data, c("TealData", "teal_data", "teal_data_module"))) {
    data <- teal.data::to_relational_data(data = data)
  }

  checkmate::assert_multi_class(data, c("TealData", "teal_data", "teal_data_module"))
  checkmate::assert_multi_class(modules, c("teal_module", "list", "teal_modules"))
  checkmate::assert_string(title, null.ok = TRUE)
  checkmate::assert(
    checkmate::check_class(filter, "teal_slices"),
    checkmate::check_list(filter, names = "named")
  )
  checkmate::assert_multi_class(header, c("shiny.tag", "character"))
  checkmate::assert_multi_class(footer, c("shiny.tag", "character"))
  checkmate::assert_character(id, max.len = 1, any.missing = FALSE)

  teal.logger::log_system_info()

  if (inherits(modules, "teal_module")) {
    modules <- list(modules)
  }
  if (inherits(modules, "list")) {
    modules <- do.call(teal::modules, modules)
  }

  landing <- extract_module(modules, "teal_module_landing")
  if (length(landing) > 1L) stop("Only one `landing_popup_module` can be used.")
  modules <- drop_module(modules, "teal_module_landing")

  # Calculate app id that will be used to stamp filter state snapshots.
  # App id is a hash of the app's data and modules.
  # See "transferring snapshots" section in ?snapshot.
  hashables <- mget(c("data", "modules"))
  hashables$data <- if (inherits(hashables$data, "teal_data")) {
    as.list(hashables$data@env)
  } else if (inherits(data, "teal_data_module")) {
    body(data$server)
  } else if (hashables$data$is_pulled()) {
    sapply(get_dataname(hashables$data), simplify = FALSE, function(dn) {
      hashables$data$get_dataset(dn)$get_raw_data()
    })
  } else {
    hashables$data$get_code()
  }

  attr(filter, "app_id") <- rlang::hash(hashables)

  # convert teal.slice::teal_slices to teal::teal_slices
  filter <- as.teal_slices(as.list(filter))

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

  if (inherits(data, "teal_data")) {
    if (length(teal.data::datanames(data)) == 0) {
      stop("`data` object has no datanames. Specify `datanames(data)` and try again.")
    }

    # in case of teal_data_module this check is postponed to the srv_teal_with_splash
    is_modules_ok <- check_modules_datanames(modules, teal.data::datanames(data))
    if (!isTRUE(is_modules_ok)) {
      logger::log_error(is_modules_ok)
      checkmate::assert(is_modules_ok, .var.name = "modules")
    }


    is_filter_ok <- check_filter_datanames(filter, teal.data::datanames(data))
    if (!isTRUE(is_filter_ok)) {
      logger::log_warn(is_filter_ok)
      # we allow app to continue if applied filters are outside
      # of possible data range
    }
  }

  # Note regarding case `id = character(0)`:
  # rather than using `callModule` and creating a submodule of this module, we directly modify
  # the `ui` and `server` with `id = character(0)` and calling the server function directly
  # rather than through `callModule`
  res <- list(
    ui = ui_teal_with_splash(id = id, data = data, title = title, header = header, footer = footer),
    server = function(input, output, session) {
      if (length(landing) == 1L) {
        landing_module <- landing[[1L]]
        do.call(landing_module$server, c(list(id = "landing_module_shiny_id"), landing_module$server_args))
      }
      if (inherits(data, "TealDataAbstract")) {
        # copy TealData so that load won't be shared between the session
        data <- data$copy(deep = TRUE)
      }
      filter <- deep_copy_filter(filter)
      srv_teal_with_splash(id = id, data = data, modules = modules, filter = filter)
    }
  )
  logger::log_trace("init teal app has been initialized.")
  return(res)
}
