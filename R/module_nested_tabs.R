# Nesting teal modules in a tab UI

#' Create a UI of nested tabs of `teal_modules`
#'
#' Each `teal_modules` is translated to a `tabsetPanel` and each
#' of its children is another tab-module called recursively. The UI of a
#' `teal_module` is obtained by calling the `ui` function on it.
#'
#' The `datasets` argument is required to resolve the teal arguments in an
#' isolated context (with respect to reactivity)
#'
#' @inheritParams ui_tabs_with_filters
#' @param depth (`integer(1)`)\cr
#'  number which helps to determine depth of the modules nesting.
#' @return depending on class of `modules`:
#'   - `teal_module`: instantiated UI of the module
#'   - `teal_modules`: `tabsetPanel` with each tab corresponding to recursively
#'     calling this function on it.
#' @examples
#' mods <- teal:::get_dummy_modules()
#' datasets <- teal:::get_dummy_datasets()
#' app <- shinyApp(
#'   ui = function() {
#'     tagList(
#'       teal:::include_teal_css_js(),
#'       textOutput("info"),
#'       fluidPage( # needed for nice tabs
#'         teal:::ui_nested_tabs("dummy", modules = mods, datasets = datasets)
#'       )
#'     )
#'   },
#'   server = function(input, output, session) {
#'     active_module <- teal:::srv_nested_tabs(
#'       "dummy",
#'       datasets = datasets,
#'       modules = mods
#'     )
#'     output$info <- renderText({
#'       paste0("The currently active tab name is ", active_module()$label)
#'     })
#'   }
#' )
#' \dontrun{
#' runApp(app)
#' }
#' @keywords internal
ui_nested_tabs <- function(id, modules, datasets, depth = 0L) {
  stopifnot(inherits(datasets, "FilteredData"))
  stopifnot(inherits(depth, "integer") && length(depth) == 1)
  UseMethod("ui_nested_tabs", modules)
}

#' @rdname ui_nested_tabs
#' @export
#' @keywords internal
ui_nested_tabs.default <- function(id, modules, datasets, depth = 0L) {
  stop("Modules class not supported: ", paste(class(modules), collapse = " "))
}

#' @rdname ui_nested_tabs
#' @export
#' @keywords internal
ui_nested_tabs.teal_modules <- function(id, modules, datasets, depth = 0L) {
  ns <- NS(id)
  do.call(
    tabsetPanel,
    c(
      # by giving an id, we can reactively respond to tab changes
      list(
        id = ns("active_tab"),
        type = if (modules$label == "root") "pills" else "tabs"
      ),
      lapply(
        names(modules$children),
        function(id) {
          tabPanel(
            title = modules$children[[id]]$label,
            value = id, # when clicked this tab value changes input$<tabset panel id>
            ui_nested_tabs(id = ns(id), modules = modules$children[[id]], datasets, depth = depth + 1L)
          )
        }
      )
    )
  )
}

#' @rdname ui_nested_tabs
#' @export
#' @keywords internal
ui_nested_tabs.teal_module <- function(id, modules, datasets, depth = 0L) {
  checkmate::assert_class(datasets, "FilteredData")
  args <- isolate(teal.transform::resolve_delayed(modules$ui_args, datasets))
  args <- c(list(id = id), args)

  if (is_arg_used(modules$ui, "datasets")) {
    args <- c(args, datasets = datasets)
  }


  #TODO we want to add deprecation, you should not have access to data in ui
  # and handle active_id here (modules e.g. missing data do still use it)
  if (is_arg_used(modules$ui, "data")) {
    data <- .datasets_to_data(modules, datasets, id)
    args <- c(args, data = list(data))
  }

  tags$div(
    id = id,
    class = "teal_module",
    tagList(
      if (depth >= 2L) div(style = "mt-6"),
      do.call(modules$ui, args)
    )
  )
}

#' Server function that returns currently active module
#'
#' @inheritParams srv_tabs_with_filters
#' @details
#' This module calls recursively all elements of the `modules` returns one which
#' is currently active. \cr
#' `teal_module` returns self as a active module.
#' `teal_modules` also returns module active within self which is determined by the `input$active_tab`.
#'
#' @return `reactive` which returns the active module that corresponds to the selected tab
#' @keywords internal
srv_nested_tabs <- function(id, datasets, modules, reporter = teal.reporter::Reporter$new(), active_id) {
  stopifnot(inherits(datasets, "FilteredData"))
  stopifnot(inherits(reporter, "Reporter"))
  UseMethod("srv_nested_tabs", modules)
}

#' @rdname srv_nested_tabs
#' @export
#' @keywords internal
srv_nested_tabs.default <- function(id, datasets, modules, reporter, active_id) {
  stop("Modules class not supported: ", paste(class(modules), collapse = " "))
}

#' @rdname srv_nested_tabs
#' @export
#' @keywords internal
srv_nested_tabs.teal_modules <- function(id, datasets, modules, reporter, active_id) {
  moduleServer(id = id, module = function(input, output, session) {
    logger::log_trace(
      paste(
        "srv_nested_tabs.teal_modules initializing the module with:",
        "datasets { paste(datasets$datanames(), collapse = ' ') };",
        "module { deparse1(modules$label) }."
      )
    )

    modules_reactive <- sapply(names(modules$children), USE.NAMES = TRUE, function(id) {
      srv_nested_tabs(
        id = id,
        datasets = datasets,
        modules = modules$children[[id]],
        reporter = reporter,
        active_id
      )
    })

    get_active_module <- reactive({
      if (length(modules$children) == 1L) {
        # single tab is active by default
        active_mod <- modules_reactive[[1]]()
        structure(
          active_mod,
          name = attr(active_mod, "name"))
      } else {
        # switch to active tab
        req(input$active_tab)
        active_mod <- modules_reactive[[input$active_tab]]()
        active_name <- attr(active_mod, "name")
        structure(active_mod, name = active_name)
      }
    })

    get_active_module
  })
}

#' @rdname srv_nested_tabs
#' @export
#' @keywords internal
srv_nested_tabs.teal_module <- function(id, datasets, modules, reporter, active_id) {
  logger::log_trace(
    paste(
      "srv_nested_tabs.teal_module initializing the module with:",
      "datasets { paste(datasets$datanames(), collapse = ' ') };",
      "module { deparse1(modules$label) }."
    )
  )
  modules$server_args <- teal.transform::resolve_delayed(modules$server_args, datasets)

  args <- c(list(id = id), modules$server_args)
  if (is_arg_used(modules$server, "reporter")) {
    args <- c(args, list(reporter = reporter))
  }

  if (is_arg_used(modules$server, "datasets")) {
    args <- c(args, datasets = datasets)
  }

  if (is_arg_used(modules$server, "data")) {
    data <- .datasets_to_data(modules, datasets, id, active_id)
    args <- c(args, data = list(data))
  }

  if (is_arg_used(modules$server, "filter_panel_api")) {
    filter_panel_api <- teal.slice::FilterPanelAPI$new(datasets)
    args <- c(args, filter_panel_api = filter_panel_api)
  }

  if (is_arg_used(modules$server, "datasets") && is_arg_used(modules$server, "data")) {
    warning(
      "Module '", modules$label, "' has `data` and `datasets` arguments in the formals.",
      "\nIt's recommended to use `data` to work with filtered objects."
    )
  }

  # teal_modules do not suppose to return values as it's never passed anyway
  # it's assigned here for tests
  module_output <- if (is_arg_used(modules$server, "id")) {
    do.call(modules$server, args)
  } else {
    do.call(callModule, c(args, list(module = modules$server)))
  }
  reactive(structure(modules, name = id))
}

#' Convert `FilteredData` to reactive list of datasets of the `tdata` type.
#'
#' Converts `FilteredData` object to `tdata` object containing datasets needed for a specific module.
#' Please note that if module needs dataset which has a parent, then parent will be also returned.
#' A hash per `dataset` is calculated internally and returned in the code.
#'
#' @param module (`teal_module`) module where needed filters are taken from
#' @param datasets (`FilteredData`) object where needed data are taken from
#' @return list of reactive datasets with following attributes:
#' - `code` (`character`) containing datasets reproducible code.
#' - `join_keys` (`JoinKeys`) containing relationships between datasets.
#' - `metadata` (`list`) containing metadata of datasets.
#'
#' @keywords internal
.datasets_to_data <- function(module, datasets, id, active_id = reactive(id)) {
  datanames <- if (identical("all", module$filter) || is.null(module$filter)) {
    datasets$datanames()
  } else {
    datasets$get_filterable_datanames(module$filter) # get_filterable_datanames adds parents if present
  }

  # list of reactive filtered data
  data <- sapply(
    datanames,
    simplify = FALSE,
    function(x) {
      # TODO we could make this smarter to not re-trigger when
      # datasets$get_data(x, filtered = TRUE) invalidated but does
      # not change value, i.e. when a new filter is added but doesn't
      # change the filtered data
      reactive({
        req(active_id() == id)
        datasets$get_data(x, filtered = TRUE)
      })
    }
  )

  hashes <- calculate_hashes(datanames, datasets)
  metadata <- lapply(datanames, datasets$get_metadata)
  names(metadata) <- datanames

  new_tdata(
    data,
    reactive(
      c(
        get_rcode_str_install(),
        get_rcode_libraries(),
        get_datasets_code(datanames, datasets, hashes),
        teal.slice::get_filter_expr(datasets, datanames)
      )
    ),
    datasets$get_join_keys(),
    metadata
  )
}

#' Get the hash of a dataset
#'
#' @param datanames (`character`) names of datasets
#' @param datasets (`FilteredData`) object holding the data
#'
#' @return A list of hashes per dataset
#' @keywords internal
#'
calculate_hashes <- function(datanames, datasets) {
  sapply(
    datanames,
    simplify = FALSE,
    function(x) {
      rlang::hash(datasets$get_data(x, filtered = FALSE))
    }
  )
}
