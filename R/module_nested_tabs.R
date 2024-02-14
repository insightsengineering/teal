#' Create a UI of nested tabs of `teal_modules`
#'
#' @section `ui_nested_tabs`:
#' Each `teal_modules` is translated to a `tabsetPanel` and each
#' of its children is another tab-module called recursively. The UI of a
#' `teal_module` is obtained by calling its UI function.
#'
#' The `datasets` argument is required to resolve the `teal` arguments in an
#' isolated context (with respect to reactivity).
#'
#' @section `srv_nested_tabs`:
#' This module recursively calls all elements of `modules` and returns currently active one.
#' - `teal_module` returns self as a active module.
#' - `teal_modules` also returns module active within self which is determined by the `input$active_tab`.
#'
#' @name module_nested_tabs
#'
#' @inheritParams module_tabs_with_filters
#'
#' @param depth (`integer(1)`)
#'  number which helps to determine depth of the modules nesting.
#' @param is_module_specific (`logical(1)`)
#'  flag determining if the filter panel is global or module-specific.
#'  When set to `TRUE`, a filter panel is called inside of each module tab.
#'
#' @return
#' Depending on the class of `modules`, `ui_nested_tabs` returns:
#'   - `teal_module`: instantiated UI of the module.
#'   - `teal_modules`: `tabsetPanel` with each tab corresponding to recursively
#'     calling this function on it.
#'
#' `srv_nested_tabs` returns a reactive which returns the active module that corresponds to the selected tab.
#'
#' @keywords internal
NULL

#' @rdname module_nested_tabs
ui_nested_tabs <- function(id, modules, datasets, depth = 0L, is_module_specific = FALSE) {
  checkmate::assert_multi_class(modules, c("teal_modules", "teal_module"))
  checkmate::assert_count(depth)
  UseMethod("ui_nested_tabs", modules)
}

#' @rdname module_nested_tabs
#' @export
ui_nested_tabs.default <- function(id, modules, datasets, depth = 0L, is_module_specific = FALSE) {
  stop("Modules class not supported: ", paste(class(modules), collapse = " "))
}

#' @rdname module_nested_tabs
#' @export
ui_nested_tabs.teal_modules <- function(id, modules, datasets, depth = 0L, is_module_specific = FALSE) {
  checkmate::assert_list(datasets, types = c("list", "FilteredData"))
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
        function(module_id) {
          module_label <- modules$children[[module_id]]$label
          tabPanel(
            title = module_label,
            value = module_id, # when clicked this tab value changes input$<tabset panel id>
            ui_nested_tabs(
              id = ns(module_id),
              modules = modules$children[[module_id]],
              datasets = datasets[[module_label]],
              depth = depth + 1L,
              is_module_specific = is_module_specific
            )
          )
        }
      )
    )
  )
}

#' @rdname module_nested_tabs
#' @export
ui_nested_tabs.teal_module <- function(id, modules, datasets, depth = 0L, is_module_specific = FALSE) {
  checkmate::assert_class(datasets, classes = "FilteredData")
  ns <- NS(id)

  args <- c(list(id = ns("module")), modules$ui_args)

  teal_ui <- tags$div(
    id = id,
    class = "teal_module",
    uiOutput(ns("data_reactive"), inline = TRUE),
    tagList(
      if (depth >= 2L) div(style = "mt-6"),
      do.call(modules$ui, args)
    )
  )

  if (!is.null(modules$datanames) && is_module_specific) {
    fluidRow(
      column(width = 9, teal_ui, class = "teal_primary_col"),
      column(
        width = 3,
        datasets$ui_filter_panel(ns("module_filter_panel")),
        class = "teal_secondary_col"
      )
    )
  } else {
    teal_ui
  }
}

#' @rdname module_nested_tabs
srv_nested_tabs <- function(id, datasets, modules, is_module_specific = FALSE,
                            reporter = teal.reporter::Reporter$new()) {
  checkmate::assert_multi_class(modules, c("teal_modules", "teal_module"))
  checkmate::assert_class(reporter, "Reporter")
  UseMethod("srv_nested_tabs", modules)
}

#' @rdname module_nested_tabs
#' @export
srv_nested_tabs.default <- function(id, datasets, modules, is_module_specific = FALSE,
                                    reporter = teal.reporter::Reporter$new()) {
  stop("Modules class not supported: ", paste(class(modules), collapse = " "))
}

#' @rdname module_nested_tabs
#' @export
srv_nested_tabs.teal_modules <- function(id, datasets, modules, is_module_specific = FALSE,
                                         reporter = teal.reporter::Reporter$new()) {
  checkmate::assert_list(datasets, types = c("list", "FilteredData"))

  moduleServer(id = id, module = function(input, output, session) {
    logger::log_trace("srv_nested_tabs.teal_modules initializing the module { deparse1(modules$label) }.")

    labels <- vapply(modules$children, `[[`, character(1), "label")
    modules_reactive <- sapply(
      names(modules$children),
      function(module_id) {
        srv_nested_tabs(
          id = module_id,
          datasets = datasets[[labels[module_id]]],
          modules = modules$children[[module_id]],
          is_module_specific = is_module_specific,
          reporter = reporter
        )
      },
      simplify = FALSE
    )

    # when not ready input$active_tab would return NULL - this would fail next reactive
    input_validated <- eventReactive(input$active_tab, input$active_tab, ignoreNULL = TRUE)
    get_active_module <- reactive({
      if (length(modules$children) == 1L) {
        # single tab is active by default
        modules_reactive[[1]]()
      } else {
        # switch to active tab
        modules_reactive[[input_validated()]]()
      }
    })

    get_active_module
  })
}

#' @rdname module_nested_tabs
#' @export
srv_nested_tabs.teal_module <- function(id, datasets, modules, is_module_specific = TRUE,
                                        reporter = teal.reporter::Reporter$new()) {
  checkmate::assert_class(datasets, "FilteredData")
  logger::log_trace("srv_nested_tabs.teal_module initializing the module: { deparse1(modules$label) }.")

  moduleServer(id = id, module = function(input, output, session) {
    if (!is.null(modules$datanames) && is_module_specific) {
      datasets$srv_filter_panel("module_filter_panel")
    }

    # Create two triggers to limit reactivity between filter-panel and modules.
    # We want to recalculate only visible modules
    # - trigger the data when the tab is selected
    # - trigger module to be called when the tab is selected for the first time
    trigger_data <- reactiveVal(1L)
    trigger_module <- reactiveVal(NULL)
    output$data_reactive <- renderUI({
      lapply(datasets$datanames(), function(x) {
        datasets$get_data(x, filtered = TRUE)
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
      args <- c(args, datasets = datasets)
    }

    if (is_arg_used(modules$server, "data")) {
      data <- eventReactive(trigger_data(), .datasets_to_data(modules, datasets))
      args <- c(args, data = list(data))
    }

    if (is_arg_used(modules$server, "filter_panel_api")) {
      filter_panel_api <- teal.slice::FilterPanelAPI$new(datasets)
      args <- c(args, filter_panel_api = filter_panel_api)
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

    reactive(modules)
  })
}

#' Convert `FilteredData` to reactive list of datasets of the `teal_data` type.
#'
#' Converts `FilteredData` object to `teal_data` object containing datasets needed for a specific module.
#' Please note that if a module needs a dataset which has a parent, then the parent will also be returned.
#' A hash per `dataset` is calculated internally and returned in the code.
#'
#' @param module (`teal_module`) module where needed filters are taken from
#' @param datasets (`FilteredData`) object where needed data are taken from
#'
#' @return A `teal_data` object.
#'
#' @keywords internal
.datasets_to_data <- function(module, datasets) {
  checkmate::assert_class(module, "teal_module")
  checkmate::assert_class(datasets, "FilteredData")

  datanames <- if (is.null(module$datanames) || identical(module$datanames, "all")) {
    datasets$datanames()
  } else {
    include_parent_datanames(
      module$datanames,
      datasets$get_join_keys()
    )
  }

  # list of reactive filtered data
  data <- sapply(datanames, function(x) datasets$get_data(x, filtered = TRUE), simplify = FALSE)

  hashes <- calculate_hashes(datanames, datasets)

  code <- c(
    get_rcode_str_install(),
    get_rcode_libraries(),
    get_datasets_code(datanames, datasets, hashes)
  )


  data <- do.call(
    teal.data::teal_data,
    args = c(data, code = list(code), join_keys = list(datasets$get_join_keys()[datanames]))
  )

  data@verified <- attr(datasets, "verification_status")
  data
}

#' Get the hash of a dataset
#'
#' @param datanames (`character`) names of datasets
#' @param datasets (`FilteredData`) object holding the data
#'
#' @return A list of hashes per dataset.
#' @keywords internal
#'
calculate_hashes <- function(datanames, datasets) {
  sapply(datanames, function(x) rlang::hash(datasets$get_data(x, filtered = FALSE)), simplify = FALSE)
}
