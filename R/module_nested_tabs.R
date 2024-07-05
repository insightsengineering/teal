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
#' @inheritParams module_tabs_with_filters
#'
#' @param depth (`integer(1)`)
#'  number which helps to determine depth of the modules nesting.
#' @param is_module_specific (`logical(1)`)
#'  flag determining if the filter panel is global or module-specific.
#'  When set to `TRUE`, a filter panel is called inside of each module tab.
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
  checkmate::assert_multi_class(modules, c("teal_modules", "teal_module"))
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
        column(width = 9, do.call(modules$ui, args), class = "teal_primary_col"),
        column(
          width = 3,
          ui_filter_panel(ns("module_filter_panel")),
          if (is_arg_used(modules$ui, "transformers")) {
            div(
              class = "well",
              ui_teal_data_module(ns("module-data_transform"), args$transformers)
            )
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
                            datasets,
                            modules,
                            slices_global,
                            reporter = teal.reporter::Reporter$new()) {
  checkmate::assert_multi_class(modules, c("teal_modules", "teal_module"))
  checkmate::assert_class(reporter, "Reporter")
  UseMethod("srv_teal_module", modules)
}

#' @rdname module_teal_module
#' @export
srv_teal_module.default <- function(id,
                                    data_rv,
                                    datasets,
                                    modules,
                                    slices_global,
                                    reporter = teal.reporter::Reporter$new()) {
  stop("Modules class not supported: ", paste(class(modules), collapse = " "))
}

#' @rdname module_teal_module
#' @export
srv_teal_module.teal_modules <- function(id,
                                         data_rv,
                                         datasets,
                                         modules,
                                         slices_global,
                                         reporter = teal.reporter::Reporter$new()) {
  moduleServer(id = id, module = function(input, output, session) {
    logger::log_trace("srv_teal_module.teal_modules initializing the module { deparse1(modules$label) }.")

    labels <- vapply(modules$children, `[[`, character(1), "label")
    modules_reactive <- sapply(
      names(modules$children),
      function(module_id) {
        srv_teal_module(
          id = module_id,
          data_rv = data_rv,
          datasets = datasets,
          modules = modules$children[[module_id]],
          slices_global = slices_global,
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

#' @rdname module_teal_module
#' @export
srv_teal_module.teal_module <- function(id,
                                        data_rv,
                                        datasets,
                                        modules,
                                        slices_global,
                                        reporter = teal.reporter::Reporter$new()) {
  logger::log_trace("srv_teal_module.teal_module initializing the module: { deparse1(modules$label) }.")
  moduleServer(id = id, module = function(input, output, session) {
    active_datanames <- reactive({
      req(data_rv())
      if (is.null(modules$datanames) || identical(modules$datanames, "all")) {
        teal_data_datanames(data_rv())
      } else {
        include_parent_datanames(
          modules$datanames,
          teal.data::join_keys(data_rv())
        )
      }
    })
    if (is.null(datasets)) {
      datasets <- eventReactive(data_rv(), {
        logger::log_trace("srv_teal_module@1 initializing module-specific FilteredData")

        # Otherwise, FilteredData will be created in the modules' scope later
        progress_data <- Progress$new(
          max = length(unlist(module_labels(modules)))
        )
        on.exit(progress_data$close())
        progress_data$set(message = "Preparing data filtering", detail = "0%")
        filtered_data <- teal_data_to_filtered_data(data_rv(), datanames = active_datanames())
        filtered_data
      })
    }

    # manage module filters on the module level
    # important:
    #   filter_manager_module_srv needs to be called before filter_panel_srv
    #   Because available_teal_slices is used in FilteredData$srv_available_slices (via srv_filter_panel)
    #   and if it is not set, then it won't be available in the srv_filter_panel
    srv_module_filter_manager(modules$label, module_fd = datasets, slices_global = slices_global)
    srv_filter_panel(
      "module_filter_panel",
      datasets = datasets,
      active_datanames = active_datanames
    )

    # Create two triggers to limit reactivity between filter-panel and modules.
    # We want to recalculate only visible modules (renderUI triggers only when visible, when tab is displayed)
    # - trigger the data when the tab is selected
    # - trigger module to be called when the tab is selected for the first time
    trigger_data <- reactiveVal(1L)
    trigger_module <- reactiveVal(NULL)
    output$data_reactive <- renderUI({
      req(datasets())
      get_filter_expr(datasets())
      isolate(trigger_data(trigger_data() + 1))
      isolate(trigger_module(TRUE))
      NULL
    })

    # This function calls a module server function.
    call_module <- function() {
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
        filtered_teal_data <- eventReactive(trigger_data(), {
          .make_teal_data(modules, data = data_rv(), datasets = datasets(), datanames = active_datanames())
        })
        args <- c(args, data = list(filtered_teal_data))
      }

      if (is_arg_used(modules$server, "transformers")) {
        stopifnot(
          "\"transformer\" argument also requires \"data\" argument." = is_arg_used(modules$server, "data")
        )
        args$data <- srv_teal_data_module("module-data_transform", data = args$data, transformers = args$transformers)
      }

      if (is_arg_used(modules$server, "filter_panel_api")) {
        filter_panel_api <- teal.slice::FilterPanelAPI$new(datasets())
        args <- c(args, filter_panel_api = filter_panel_api)
      }

      if (is_arg_used(modules$server, "id")) {
        do.call(modules$server, args)
      } else {
        do.call(callModule, c(args, list(module = modules$server)))
      }
    }

    # Call modules.
    module_out <- if (!inherits(modules, "teal_module_previewer")) {
      observeEvent(
        ignoreNULL = TRUE,
        once = TRUE,
        eventExpr = trigger_module(),
        handlerExpr = call_module()
      )
    } else {
      # Report previewer must be initiated on app start for report cards to be included in bookmarks.
      # When previewer is delayed, cards are bookmarked only if previewer has been initiated (visited).
      call_module()
    }

    # todo: (feature request) add a ReporterCard to the reporter as an output from the teal_module
    #       how to determine if module returns a ReporterCard so that reportPreviewer is needed?
    #       Should we insertUI of the ReportPreviewer then?
    #       What about attr(module, "reportable") - similar to attr(module, "bookmarkable")
    if ("report" %in% names(module_out)) {
      # (reactively) add card to the reporter
    }

    reactive(modules)
  })
}

.make_teal_data <- function(modules, data, datasets, datanames) {
  new_datasets <- c(
    # Filtered data
    sapply(datanames, function(x) datasets$get_data(x, filtered = TRUE), simplify = FALSE),
    # Raw (unfiltered data)
    stats::setNames(
      lapply(datanames, function(x) datasets$get_data(x, filtered = FALSE)),
      sprintf("%s_raw", datanames)
    )
  )

  data_code <- teal.data::get_code(data, datanames = datanames)
  hashes_code <- .get_hashes_code(datasets, datanames)
  raw_data_code <- sprintf("%1$s_raw <- %1$s", datanames)
  filter_code <- get_filter_expr(datasets, datanames = datanames)

  all_code <- paste(unlist(c(data_code, "", hashes_code, raw_data_code, "", filter_code)), collapse = "\n")
  tdata <- do.call(
    teal.data::teal_data,
    c(
      list(code = trimws(all_code, which = "right")),
      list(join_keys = teal.data::join_keys(data)),
      new_datasets
    )
  )

  tdata@verified <- data@verified
  teal.data::datanames(tdata) <- datanames
  tdata
}

#' Get code that tests the integrity of the reproducible data
#'
#' @param datasets (`FilteredData`) object holding the data
#' @param datanames (`character`) names of datasets
#'
#' @return A character vector with the code lines.
#' @keywords internal
#'
.get_hashes_code <- function(datasets, datanames) {
  vapply(
    datanames,
    function(dataname, datasets) {
      hash <- rlang::hash(datasets$get_data(dataname, filtered = FALSE))
      sprintf(
        "stopifnot(%s == %s)",
        deparse1(bquote(rlang::hash(.(as.name(dataname))))),
        deparse1(hash)
      )
    },
    character(1L),
    datasets = datasets,
    USE.NAMES = FALSE
  )
}
