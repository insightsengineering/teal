#' Create a UI of nested tabs of `teal_modules`
#'
#' @section `ui_teal_module`:
#' Each `teal_modules` is translated to a `bslib::navset_tab` and each
#' of its children is another tab-module called recursively. The UI of a
#' `teal_module` is obtained by calling its UI function.
#'
#' The `datasets` argument is required to resolve the `teal` arguments in an
#' isolated context (with respect to reactivity).
#'
#' @section `srv_teal_module`:
#' This module recursively calls all elements of `modules` and returns currently active one.
#' - `teal_module` returns self as a active module.
#' - `teal_modules` also returns module active within self which is determined by the `input$active_tab`.
#'
#' @name module_teal_module
#'
#' @inheritParams module_teal
#'
#' @param data_rv (`reactive` returning `teal_data`)
#'
#' @param slices_global (`reactiveVal` returning `modules_teal_slices`)
#'   see [`module_filter_manager`]
#'
#' @param depth (`integer(1)`)
#'  number which helps to determine depth of the modules nesting.
#'
#' @param datasets (`reactive` returning `FilteredData` or `NULL`)
#'  When `datasets` is passed from the parent module (`srv_teal`) then `dataset` is a singleton
#'  which implies in filter-panel to be "global". When `NULL` then filter-panel is "module-specific".
#'
#' @return
#' Depending on the class of `modules`, `ui_teal_module` returns:
#'   - `teal_module`: instantiated UI of the module.
#'   - `teal_modules`: `bslib::navset_tab` with each tab corresponding to recursively
#'     calling this function on it.
#'
#' `srv_teal_module` returns a reactive which returns the active module that corresponds to the selected tab.
#'
#' @keywords internal
NULL

#' @rdname module_teal_module
ui_teal_module <- function(id, modules, depth = 0L) {
  checkmate::assert_multi_class(modules, c("teal_modules", "teal_module", "shiny.tag"))
  checkmate::assert_count(depth)
  UseMethod("ui_teal_module", modules)
}

#' @rdname module_teal_module
#' @export
ui_teal_module.default <- function(id, modules, depth = 0L) {
  stop("Modules class not supported: ", paste(class(modules), collapse = " "))
}

#' @rdname module_teal_module
#' @export
ui_teal_module.teal_modules <- function(id, modules, depth = 0L) {
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
          if (is.null(module_label)) {
            module_label <- icon("database")
          }
          tabPanel(
            title = module_label,
            value = module_id, # when clicked this tab value changes input$<tabset panel id>
            ui_teal_module(
              id = ns(module_id),
              modules = modules$children[[module_id]],
              depth = depth + 1L
            )
          )
        }
      )
    )
  )
}

#' @rdname module_teal_module
#' @export
ui_teal_module.shiny.tag <- function(id, modules, depth = 0L) {
  modules
}

#' @rdname module_teal_module
#' @export
ui_teal_module.teal_module <- function(id, modules, depth = 0L) {
  ns <- NS(id)
  args <- c(list(id = ns("module")), modules$ui_args)
  div(
    id = id,
    class = "teal_module",
    uiOutput(ns("data_reactive"), inline = TRUE),
    tagList(
      if (depth >= 2L) tags$div(style = "mt-6"),
      fluidRow(
        column(
          width = 9,
          div(
            class = "teal_validated",
            ui_validate_reactive_teal_data(ns("validate_datanames"))
          ),
          do.call(modules$ui, args),
          class = "teal_primary_col"
        ),
        column(
          width = 3,
          ui_data_summary(ns("data_summary")),
          ui_filter_panel(ns("filter_panel")),
          if (length(modules$transformers) > 0 && !isTRUE(attr(modules$transformers, "custom_ui"))) {
            ui_teal_data_modules(ns("data_transform"), modules$transformers, class = "well")
          },
          class = "teal_secondary_col"
        )
      )
    )
  )
}

#' @rdname module_teal_module
srv_teal_module <- function(id,
                            data_rv,
                            modules,
                            datasets = NULL,
                            slices_global,
                            reporter = teal.reporter::Reporter$new(),
                            is_active = reactive(TRUE)) {
  checkmate::assert_string(id)
  checkmate::assert_class(data_rv, "reactive")
  checkmate::assert_multi_class(modules, c("teal_modules", "teal_module"))
  checkmate::assert_class(datasets, "reactive", null.ok = TRUE)
  checkmate::assert_class(slices_global, ".slicesGlobal")
  checkmate::assert_class(reporter, "Reporter")
  UseMethod("srv_teal_module", modules)
}

#' @rdname module_teal_module
#' @export
srv_teal_module.default <- function(id,
                                    data_rv,
                                    modules,
                                    datasets = NULL,
                                    slices_global,
                                    reporter = teal.reporter::Reporter$new(),
                                    is_active = reactive(TRUE)) {
  stop("Modules class not supported: ", paste(class(modules), collapse = " "))
}

#' @rdname module_teal_module
#' @export
srv_teal_module.teal_modules <- function(id,
                                         data_rv,
                                         modules,
                                         datasets = NULL,
                                         slices_global,
                                         reporter = teal.reporter::Reporter$new(),
                                         is_active = reactive(TRUE)) {
  moduleServer(id = id, module = function(input, output, session) {
    logger::log_debug("srv_teal_module.teal_modules initializing the module { deparse1(modules$label) }.")

    modules_output <- sapply(
      names(modules$children),
      function(module_id) {
        srv_teal_module(
          id = module_id,
          data_rv = data_rv,
          modules = modules$children[[module_id]],
          datasets = datasets,
          slices_global = slices_global,
          reporter = reporter,
          is_active = reactive(is_active() && input$active_tab == module_id)
        )
      },
      simplify = FALSE
    )

    modules_output
  })
}

#' @rdname module_teal_module
#' @export
srv_teal_module.teal_module <- function(id,
                                        data_rv,
                                        modules,
                                        datasets = NULL,
                                        slices_global,
                                        reporter = teal.reporter::Reporter$new(),
                                        is_active = reactive(TRUE)) {
  logger::log_debug("srv_teal_module.teal_module initializing the module: { deparse1(modules$label) }.")
  moduleServer(id = id, module = function(input, output, session) {
    active_datanames <- reactive({
      stopifnot("data_rv must be teal_data object." = inherits(data_rv(), "teal_data"))
      if (is.null(modules$datanames) || identical(modules$datanames, "all")) {
        teal_data_datanames(data_rv())
      } else {
        # Remove datanames that are not **YET** in the data (may be added with teal_data_module transforms)
        # Check is deferred when module is called
        intersect(
          include_parent_datanames(
            modules$datanames,
            teal.data::join_keys(data_rv())
          ),
          teal_data_datanames(data_rv())
        )
      }
    })
    if (is.null(datasets)) {
      datasets <- eventReactive(data_rv(), {
        if (!inherits(data_rv(), "teal_data")) {
          stop("data_rv must be teal_data object.")
        }
        logger::log_debug("srv_teal_module@1 initializing module-specific FilteredData")
        teal_data_to_filtered_data(data_rv(), datanames = active_datanames())
      })
    }

    # manage module filters on the module level
    # important:
    #   filter_manager_module_srv needs to be called before filter_panel_srv
    #   Because available_teal_slices is used in FilteredData$srv_available_slices (via srv_filter_panel)
    #   and if it is not set, then it won't be available in the srv_filter_panel
    srv_module_filter_manager(modules$label, module_fd = datasets, slices_global = slices_global)
    filtered_teal_data <- srv_filter_panel(
      "filter_panel",
      datasets = datasets,
      active_datanames = active_datanames,
      data_rv = data_rv,
      is_active = is_active
    )

    transformed_teal_data <- srv_teal_data_modules(
      "data_transform",
      data = filtered_teal_data,
      transformers = modules$transformers,
      modules = modules
    )

    summary_table <- srv_data_summary("data_summary", transformed_teal_data)

    # todo: Datasets in teal_data handed over to module should be limited to module$datanames
    #       Summary shouldn't display datanames that are not in module$datanames
    #       During ddl and transform we should keep all the datasets which might be needed in transform
    # so:
    #  - keep all datasets in ddl
    #  - make filter panel only from module$datanames (or available subset) - make a .resolve_module_datanames function
    #    which does the same thing as active_datanames() because we will need it in the later stage
    #  - make a teal_data (filtered_teal_data) containing everything. No code substitution, no datanames restriction.
    #  - transformed_teal_data can add any datasets to teal_data object
    #  - at the end, validate and use .resolve_module_datanames again to determine relevant datanames.
    #    Set datanames in the teal_data object so that app developer don't have to bother about `teal.data::datanames`
    #    in each transform module which adds datasets. Restrict the code to the "resolved" datanames. Remove bindings
    #    which are not in the resolved datanames.
    #  - send data to teal_module and to the summary
    # side comment:
    #  - looks like the only purpose of the `teal.data::datanames` is to limit the datasets for modules which have
    #    $datanames = "all". Otherwise, it is not needed as modules$datanames is the primary source of truth.
    module_teal_data <- srv_validate_reactive_teal_data(
      "validate_datanames",
      data = transformed_teal_data,
      modules = modules
    )

    # Call modules.
    module_out <- reactiveVal(NULL)
    if (!inherits(modules, "teal_module_previewer")) {
      obs_module <- observeEvent(
        # wait for module_teal_data() to be not NULL but only once:
        ignoreNULL = TRUE,
        once = TRUE,
        eventExpr = module_teal_data(),
        handlerExpr = {
          module_out(.call_teal_module(modules, datasets, module_teal_data, reporter))
        }
      )
    } else {
      # Report previewer must be initiated on app start for report cards to be included in bookmarks.
      # When previewer is delayed, cards are bookmarked only if previewer has been initiated (visited).
      module_out(.call_teal_module(modules, datasets, module_teal_data, reporter))
    }

    # todo: (feature request) add a ReporterCard to the reporter as an output from the teal_module
    #       how to determine if module returns a ReporterCard so that reportPreviewer is needed?
    #       Should we insertUI of the ReportPreviewer then?
    #       What about attr(module, "reportable") - similar to attr(module, "bookmarkable")
    if ("report" %in% names(module_out)) {
      # (reactively) add card to the reporter
    }

    module_out
  })
}

# This function calls a module server function.
.call_teal_module <- function(modules, datasets, filtered_teal_data, reporter) {
  # collect arguments to run teal_module
  args <- c(list(id = "module"), modules$server_args)
  if (is_arg_used(modules$server, "reporter")) {
    args <- c(args, list(reporter = reporter))
  }

  if (is_arg_used(modules$server, "datasets")) {
    args <- c(args, datasets = datasets())
    warning("datasets argument is not reactive and therefore it won't be updated when data is refreshed.")
  }

  if (is_arg_used(modules$server, "data")) {
    args <- c(args, data = list(filtered_teal_data))
  }

  if (is_arg_used(modules$server, "filter_panel_api")) {
    args <- c(args, filter_panel_api = teal.slice::FilterPanelAPI$new(datasets()))
  }

  if (is_arg_used(modules$server, "id")) {
    do.call(modules$server, args)
  } else {
    do.call(callModule, c(args, list(module = modules$server)))
  }
}
