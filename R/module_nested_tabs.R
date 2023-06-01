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
  ns <- NS(id)
  checkmate::assert_class(datasets, "FilteredData")

  datanames <- if (is.null(modules$filter)) {
  datasets$datanames()
  } else {
    datasets$get_filterable_datanames(modules$filter) # get_filterable_datanames adds parents if present
  }
  raw_data <- sapply(datanames, simplify = FALSE, FUN = function(dataname) {
    list(dataset = datasets$get_data(dataname, filtered = FALSE))
  })
  module_datasets <- init_filtered_data(x = raw_data, join_keys = datasets$get_join_keys())

  args <- isolate(teal.transform::resolve_delayed(modules$ui_args, datasets))
  args <- c(list(id = ns("module")), args)

  if (is_arg_used(modules$ui, "datasets")) {
    args <- c(args, datasets = datasets)
  }

  if (is_arg_used(modules$ui, "data")) {
    data <- .datasets_to_data(modules, datasets)
    args <- c(args, data = list(data))
  }

  teal_ui <- tags$div(
    id = id,
    class = "teal_module",
    uiOutput(ns("data_reactive"), inline = TRUE),
    tagList(
      if (depth >= 2L) div(style = "mt-6"),
      do.call(modules$ui, args)
    )
  )

  fluidRow(
    column(width = 9, teal_ui, class = "teal_primary_col"),
    column(width = 3, module_datasets$ui_filter_panel(ns("module_filter_panel")), class = "teal_secondary_col")
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
srv_nested_tabs <- function(id, datasets, modules, reporter = teal.reporter::Reporter$new()) {
  stopifnot(inherits(datasets, "FilteredData"))
  stopifnot(inherits(reporter, "Reporter"))
  UseMethod("srv_nested_tabs", modules)
}

#' @rdname srv_nested_tabs
#' @export
#' @keywords internal
srv_nested_tabs.default <- function(id, datasets, modules, reporter = teal.reporter::Reporter$new()) {
  stop("Modules class not supported: ", paste(class(modules), collapse = " "))
}

#' @rdname srv_nested_tabs
#' @export
#' @keywords internal
srv_nested_tabs.teal_modules <- function(id, datasets, modules, reporter = teal.reporter::Reporter$new()) {
  moduleServer(id = id, module = function(input, output, session) {
    logger::log_trace(
      paste(
        "srv_nested_tabs.teal_modules initializing the module with:",
        "datasets { paste(datasets$datanames(), collapse = ' ') };",
        "module { deparse1(modules$label) }."
      )
    )


    labels <- sapply(modules$children, `[[`, "label")
    filtered_data_list <- lapply(names(modules$children), function(id) {
      srv_nested_tabs(id = id, datasets = datasets, modules = modules$children[[id]], reporter = reporter)
    })
    names(filtered_data_list) <- labels
    filtered_data_list
  })
}

#' @rdname srv_nested_tabs
#' @export
#' @keywords internal
srv_nested_tabs.teal_module <- function(id, datasets, modules, reporter = teal.reporter::Reporter$new()) {
  logger::log_trace(
    paste(
      "srv_nested_tabs.teal_module initializing the module with:",
      "datasets { paste(datasets$datanames(), collapse = ' ') };",
      "module { deparse1(modules$label) }."
    )
  )
  moduleServer(id = id, module = function(input, output, session) {
    modules$server_args <- teal.transform::resolve_delayed(modules$server_args, datasets)

    datanames <- if (is.null(modules$filter)) {
      datasets$datanames()
    } else {
      datasets$get_filterable_datanames(modules$filter) # get_filterable_datanames adds parents if present
    }

    # create module-specific FilteredData
    raw_data <- sapply(datanames, simplify = FALSE, function(dataname) {
     list(dataset = datasets$get_data(dataname, filtered = FALSE))
    })
    module_datasets <- init_filtered_data(x = raw_data, join_keys = datasets$get_join_keys())
    module_datasets$srv_filter_panel("module_filter_panel")


    # Create two triggers to limit reactivity between filter-panel and modules.
    # We want to recalculate only visible modules
    # - trigger the data when the tab is selected
    # - trigger module to be called when the tab is selected for the first time
    trigger_data <- reactiveVal(1L)
    trigger_module <- reactiveVal(NULL)
    output$data_reactive <- renderUI({
      lapply(datanames, function(x) {
        module_datasets$get_data(x, filtered = TRUE)
      })
      isolate(trigger_data(trigger_data() + 1))
      isolate(trigger_module(TRUE))

      NULL
    })

    # collect arguments to run teal_module
    args <- c(list(id = "module"), modules$server_args)
    if (is_arg_used(modules$server, "reporter")) {
      args <- c(args, list(reporter = reporter))
    }

    if (is_arg_used(modules$server, "datasets")) {
      args <- c(args, datasets = module_datasets)
    }

    if (is_arg_used(modules$server, "data")) {
      data <- .datasets_to_data(modules, module_datasets, trigger_data)
      args <- c(args, data = list(data))
    }

    if (is_arg_used(modules$server, "filter_panel_api")) {
      filter_panel_api <- teal.slice::FilterPanelAPI$new(module_datasets)
      args <- c(args, filter_panel_api = filter_panel_api)
    }

    if (is_arg_used(modules$server, "datasets") && is_arg_used(modules$server, "data")) {
      warning(
        "Module '", modules$label, "' has `data` and `datasets` arguments in the formals.",
        "\nIt's recommended to use `data` to work with filtered objects."
      )
    }

    # observe the trigger_module above to induce the module once the renderUI is triggered
    observeEvent(
      ignoreNULL = TRUE,
      once = TRUE,
      eventExpr = trigger_module(),
      handlerExpr = {
        module_output <- if (is_arg_used(modules$server, "id")) {
          do.call(modules$server, args)
        } else {
          do.call(callModule, c(args, list(module = modules$server)))
        }
      }
    )
    module_datasets
  })
}

#' Convert `FilteredData` to reactive list of datasets of the `tdata` type.
#'
#' Converts `FilteredData` object to `tdata` object containing datasets needed for a specific module.
#' Please note that if module needs dataset which has a parent, then parent will be also returned.
#' A hash per `dataset` is calculated internally and returned in the code.
#'
#' @param module (`teal_module`) module where needed filters are taken from
#' @param datasets (`FilteredData`) object where needed data are taken from
#' @param trigger_data (`reactiveVal`) to trigger getting the filtered data
#' @return list of reactive datasets with following attributes:
#' - `code` (`character`) containing datasets reproducible code.
#' - `join_keys` (`JoinKeys`) containing relationships between datasets.
#' - `metadata` (`list`) containing metadata of datasets.
#'
#' @keywords internal
.datasets_to_data <- function(module, datasets, trigger_data = reactiveVal(1L)) {
  checkmate::assert_class(trigger_data, "reactiveVal")
  datanames <- if (is.null(module$filter)) {
    datasets$datanames()
  } else {
    datasets$get_filterable_datanames(module$filter) # get_filterable_datanames adds parents if present
  }

  # list of reactive filtered data
  data <- sapply(
    USE.NAMES = TRUE,
    X = datanames,
    FUN = function(x) eventReactive(trigger_data(), datasets$get_data(x, filtered = TRUE))
  )

  hashes <- calculate_hashes(datanames, datasets)
  metadata <- lapply(datanames, datasets$get_metadata)
  names(metadata) <- datanames

  new_tdata(
    data,
    eventReactive(
      trigger_data(),
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
