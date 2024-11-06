#' Calls all `modules`
#'
#' On the UI side each `teal_modules` is translated to a `tabsetPanel` and each `teal_module` is a
#' `tabPanel`. Both, UI and server are called recursively so that each tab is a separate module and
#' reflect nested structure of `modules` argument.
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
#' @param data_load_status (`reactive` returning `character`)
#'  Determines action dependent on a data loading status:
#'  - `"ok"` when `teal_data` is returned from the data loading.
#'  - `"teal_data_module failed"` when [teal_data_module()] didn't return  `teal_data`. Disables tabs buttons.
#'  - `"external failed"` when a `reactive` passed to `srv_teal(data)` didn't return `teal_data`. Hides the whole tab
#'    panel.
#'
#' @return
#' output of currently active module.
#' - `srv_teal_module.teal_module` returns `reactiveVal` containing output of the called module.
#' - `srv_teal_module.teal_modules` returns output of module selected by `input$active_tab`.
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
  tags$div(
    id = ns("wrapper"),
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
              module_label <- icon("fas fa-database")
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
  )
}

#' @rdname module_teal_module
#' @export
ui_teal_module.teal_module <- function(id, modules, depth = 0L) {
  ns <- NS(id)
  args <- c(list(id = ns("module")), modules$ui_args)

  ui_teal <- tagList(
    div(
      id = ns("validate_datanames"),
      ui_validate_reactive_teal_data(ns("validate_datanames"))
    ),
    shinyjs::hidden(
      tags$div(
        id = ns("transformer_failure_info"),
        class = "teal_validated",
        div(
          class = "teal-output-warning",
          "One of transformers failed. Please fix and continue."
        )
      )
    ),
    tags$div(
      id = ns("teal_module_ui"),
      do.call(modules$ui, args)
    )
  )

  div(
    id = id,
    class = "teal_module",
    uiOutput(ns("data_reactive"), inline = TRUE),
    tagList(
      if (depth >= 2L) tags$div(style = "mt-6"),
      if (!is.null(modules$datanames)) {
        fluidRow(
          column(width = 9, ui_teal, class = "teal_primary_col"),
          column(
            width = 3,
            ui_data_summary(ns("data_summary")),
            ui_filter_data(ns("filter_panel")),
            ui_transform_data(ns("data_transform"), transformers = modules$transformers, class = "well"),
            class = "teal_secondary_col"
          )
        )
      } else {
        div(
          div(
            class = "teal_validated",
            uiOutput(ns("data_input_error"))
          ),
          ui_teal
        )
      }
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
                            data_load_status = reactive("ok"),
                            is_active = reactive(TRUE)) {
  checkmate::assert_string(id)
  assert_reactive(data_rv)
  checkmate::assert_multi_class(modules, c("teal_modules", "teal_module"))
  assert_reactive(datasets, null.ok = TRUE)
  checkmate::assert_class(slices_global, ".slicesGlobal")
  checkmate::assert_class(reporter, "Reporter")
  assert_reactive(data_load_status)
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
                                    data_load_status = reactive("ok"),
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
                                         data_load_status = reactive("ok"),
                                         is_active = reactive(TRUE)) {
  moduleServer(id = id, module = function(input, output, session) {
    logger::log_debug("srv_teal_module.teal_modules initializing the module { deparse1(modules$label) }.")

    observeEvent(data_load_status(), {
      tabs_selector <- sprintf("#%s li a", session$ns("active_tab"))
      if (identical(data_load_status(), "ok")) {
        logger::log_debug("srv_teal_module@1 enabling modules tabs.")
        shinyjs::show("wrapper")
        shinyjs::enable(selector = tabs_selector)
      } else if (identical(data_load_status(), "teal_data_module failed")) {
        logger::log_debug("srv_teal_module@1 disabling modules tabs.")
        shinyjs::disable(selector = tabs_selector)
      } else if (identical(data_load_status(), "external failed")) {
        logger::log_debug("srv_teal_module@1 hiding modules tabs.")
        shinyjs::hide("wrapper")
      }
    })

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
          is_active = reactive(
            is_active() &&
              input$active_tab == module_id &&
              identical(data_load_status(), "ok")
          )
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
                                        data_load_status = reactive("ok"),
                                        is_active = reactive(TRUE)) {
  logger::log_debug("srv_teal_module.teal_module initializing the module: { deparse1(modules$label) }.")
  moduleServer(id = id, module = function(input, output, session) {
    module_out <- reactiveVal()

    active_datanames <- reactive({
      .resolve_module_datanames(data = data_rv(), modules = modules)
    })
    if (is.null(datasets)) {
      datasets <- eventReactive(data_rv(), {
        req(inherits(data_rv(), "teal_data"))
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

    call_once_when(is_active(), {
      filtered_teal_data <- srv_filter_data(
        "filter_panel",
        datasets = datasets,
        active_datanames = active_datanames,
        data_rv = data_rv,
        is_active = is_active
      )
      is_transformer_failed <- reactiveValues()
      transformed_teal_data <- srv_transform_data(
        "data_transform",
        data = filtered_teal_data,
        transformers = modules$transformers,
        modules = modules,
        is_transformer_failed = is_transformer_failed
      )
      any_transformer_failed <- reactive({
        any(unlist(reactiveValuesToList(is_transformer_failed)))
      })

      observeEvent(any_transformer_failed(), {
        if (isTRUE(any_transformer_failed())) {
          shinyjs::hide("teal_module_ui")
          shinyjs::hide("validate_datanames")
          shinyjs::show("transformer_failure_info")
        } else {
          shinyjs::show("teal_module_ui")
          shinyjs::show("validate_datanames")
          shinyjs::hide("transformer_failure_info")
        }
      })

      module_teal_data <- reactive({
        req(inherits(transformed_teal_data(), "teal_data"))
        all_teal_data <- transformed_teal_data()
        module_datanames <- .resolve_module_datanames(data = all_teal_data, modules = modules)
        all_teal_data[c(module_datanames, ".raw_data")] # verify if .raw_data is not skipped
      })

      srv_validate_reactive_teal_data(
        "validate_datanames",
        data = module_teal_data,
        modules = modules
      )

      summary_table <- srv_data_summary("data_summary", module_teal_data)

      # Call modules.
      if (!inherits(modules, "teal_module_previewer")) {
        obs_module <- call_once_when(
          !is.null(module_teal_data()),
          ignoreNULL = TRUE,
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
    })

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

.resolve_module_datanames <- function(data, modules) {
  stopifnot("data_rv must be teal_data object." = inherits(data, "teal_data"))
  if (is.null(modules$datanames) || identical(modules$datanames, "all")) {
    .topologically_sort_datanames(ls(teal.code::get_env(data)), teal.data::join_keys(data))
  } else {
    intersect(
      .include_parent_datanames(modules$datanames, teal.data::join_keys(data)),
      ls(teal.code::get_env(data))
    )
  }
}

#' Calls expression when condition is met
#'
#' Function postpones `handlerExpr` to the moment when `eventExpr` (condition) returns `TRUE`,
#' otherwise nothing happens.
#' @param eventExpr A (quoted or unquoted) logical expression that represents the event;
#' this can be a simple reactive value like input$click, a call to a reactive expression
#' like dataset(), or even a complex expression inside curly braces.
#' @param ... additional arguments passed to `observeEvent` with the exception of `eventExpr` that is not allowed.
#' @inheritParams shiny::observeEvent
#'
#' @return An observer.
#'
#' @keywords internal
call_once_when <- function(eventExpr, # nolint: object_name.
                           handlerExpr, # nolint: object_name.
                           event.env = parent.frame(), # nolint: object_name.
                           handler.env = parent.frame(), # nolint: object_name.
                           ...) {
  event_quo <- rlang::new_quosure(substitute(eventExpr), env = event.env)
  handler_quo <- rlang::new_quosure(substitute(handlerExpr), env = handler.env)

  # When `condExpr` is TRUE, then `handlerExpr` is evaluated once.
  activator <- reactive({
    if (isTRUE(rlang::eval_tidy(event_quo))) {
      TRUE
    }
  })

  observeEvent(
    eventExpr = activator(),
    once = TRUE,
    handlerExpr = rlang::eval_tidy(handler_quo),
    ...
  )
}
