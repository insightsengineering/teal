#' Calls all `modules`
#'
#' On the UI side each `teal_modules` is translated to a custom dropdown navigation and each `teal_module` is a
#' panel that shows/hides based on dropdown selection. Both, UI and server are called recursively so that each tab is a separate module and
#' reflect nested structure of `modules` argument.
#'
#' @name module_teal_module
#'
#' @inheritParams module_teal
#'
#' @param data (`reactive` returning `teal_data`)
#'
#' @param slices_global (`reactiveVal` returning `modules_teal_slices`)
#'   see [`module_filter_manager`]
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
#' - `srv_teal_module.teal_modules` returns output of module selected by dropdown.
#'
#' @keywords internal
NULL

#' @rdname module_teal_module
ui_teal_module <- function(id, modules) {
  checkmate::assert_multi_class(modules, c("teal_modules", "teal_module", "shiny.tag"))
  UseMethod("ui_teal_module", modules)
}

#' @rdname module_teal_module
#' @export
ui_teal_module.default <- function(id, modules) {
  stop("Modules class not supported: ", paste(class(modules), collapse = " "))
}

#' @rdname module_teal_module
#' @export
ui_teal_module.teal_modules <- function(id, modules) {
  ns <- NS(id)
  modules_ui <- lapply(names(modules), function(module_id) {
    ui_teal_module(ns(module_id), modules[[module_id]])
  })
  names(modules_ui) <- names(modules)
  .teal_custom_nav(ns("active_module_id"), modules, modules_ui)
}

#' @rdname module_teal_module
#' @export
ui_teal_module.teal_module <- function(id, modules) {
  ns <- NS(id)
  args <- c(list(id = ns("module")), modules$ui_args)

  ui_teal <- tags$div(
    shinyjs::hidden(
      tags$div(
        id = ns("transform_failure_info"),
        class = "teal_validated",
        div(
          class = "teal-output-warning",
          "One of transformators failed. Please check its inputs."
        )
      )
    ),
    tags$div(
      id = ns("teal_module_ui"),
      tags$div(
        class = "teal_validated",
        ui_check_module_datanames(ns("validate_datanames"))
      ),
      do.call(what = modules$ui, args = args, quote = TRUE)
    )
  )

  div(
    id = id,
    class = "teal_module",
    uiOutput(ns("data_reactive"), inline = TRUE),
    tagList(
      .get_breadcrum(modules),
      if (!is.null(modules$datanames)) {
        tagList(
          bslib::layout_sidebar(
            class = "teal-sidebar-layout",
            sidebar = bslib::sidebar(
              id = ns("teal_module_sidebar"),
              class = "teal-sidebar",
              width = getOption("teal.sidebar.width", 250),
              tags$div(
                tags$div(
                  class = "teal-active-data-summary-panel",
                  bslib::accordion(
                    id = ns("data_summary_accordion"),
                    bslib::accordion_panel(
                      "Active Data Summary",
                      tags$div(
                        class = "teal-active-data-summary",
                        ui_data_summary(ns("data_summary"))
                      )
                    )
                  )
                ),
                tags$br(),
                tags$div(
                  class = "teal-filter-panel",
                  ui_filter_data(ns("filter_panel"))
                ),
                if (length(modules$transformators) > 0 && !isTRUE(attr(modules$transformators, "custom_ui"))) {
                  tags$div(
                    tags$br(),
                    tags$div(
                      class = "teal-transform-panel",
                      bslib::accordion(
                        id = ns("data_transform_accordion"),
                        bslib::accordion_panel(
                          "Transform Data",
                          ui_transform_teal_data(
                            ns("data_transform"),
                            transformators = modules$transformators
                          )
                        )
                      )
                    )
                  )
                }
              )
            ),
            ui_teal
          ),
          div(
            id = ns("sidebar_toggle_buttons"),
            class = "sidebar-toggle-buttons",
            actionButton(
              class = "data-summary-toggle btn-outline-primary",
              ns("data_summary_toggle"),
              icon("fas fa-list")
            ),
            actionButton(
              class = "data-filters-toggle btn-outline-secondary",
              ns("data_filters_toggle"),
              icon("fas fa-filter")
            ),
            if (length(modules$transformators) > 0) {
              actionButton(
                class = "data-transforms-toggle btn-outline-primary",
                ns("data_transforms_toggle"),
                icon("fas fa-pen-to-square")
              )
            }
          ),
          tags$script(
            HTML(
              sprintf(
                "
                  $(document).ready(function() {
                    $('#%s').insertAfter('#%s > .bslib-sidebar-layout > button.collapse-toggle');
                  });
                ",
                ns("sidebar_toggle_buttons"),
                id
              )
            )
          )
        )
      } else {
        ui_teal
      }
    )
  )
}

#' @rdname module_teal_module
srv_teal_module <- function(id,
                            data,
                            modules,
                            datasets = NULL,
                            slices_global,
                            reporter = teal.reporter::Reporter$new(),
                            data_load_status = reactive("ok"),
                            is_active = reactive(TRUE)) {
  checkmate::assert_string(id)
  assert_reactive(data)
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
                                    data,
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
                                         data,
                                         modules,
                                         datasets = NULL,
                                         slices_global,
                                         reporter = teal.reporter::Reporter$new(),
                                         data_load_status = reactive("ok"),
                                         is_active = reactive(TRUE)) {
  moduleServer(id = id, module = function(input, output, session) {
    logger::log_debug("srv_teal_module.teal_modules initializing the module { deparse1(modules$label) }.")

    observeEvent(data_load_status(), {
      nav_buttons_selector <- sprintf("[id*='%s'][id*='nav_']", session$ns(""))
      if (identical(data_load_status(), "ok")) {
        logger::log_debug("srv_teal_module@1 enabling modules navigation.")
        shinyjs::show("wrapper")
        shinyjs::enable(selector = nav_buttons_selector)
      } else if (identical(data_load_status(), "teal_data_module failed")) {
        logger::log_debug("srv_teal_module@1 disabling modules navigation.")
        shinyjs::disable(selector = nav_buttons_selector)
      } else if (identical(data_load_status(), "external failed")) {
        logger::log_debug("srv_teal_module@1 hiding modules navigation.")
        shinyjs::hide("wrapper")
      }
    })

    modules_output <- sapply(
      names(modules),
      function(module_id) {
        module_info <- modules[[module_id]]
        srv_teal_module(
          id = module_id,
          data = data,
          modules = module_info,
          datasets = datasets,
          slices_global = slices_global,
          reporter = reporter,
          is_active = reactive(
            is_active() &&
              input$active_module_id == module_id &&
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
                                        data,
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
      .resolve_module_datanames(data = data(), modules = modules)
    })
    if (is.null(datasets)) {
      datasets <- eventReactive(data(), {
        req(inherits(data(), "teal_data"))
        logger::log_debug("srv_teal_module@1 initializing module-specific FilteredData")
        teal_data_to_filtered_data(data(), datanames = active_datanames())
      })
    }

    # manage module filters on the module level
    # important:
    #   filter_manager_module_srv needs to be called before filter_panel_srv
    #   Because available_teal_slices is used in FilteredData$srv_available_slices (via srv_filter_panel)
    #   and if it is not set, then it won't be available in the srv_filter_panel
    srv_module_filter_manager(modules$label, module_fd = datasets, slices_global = slices_global)

    .call_once_when(is_active(), {
      filtered_teal_data <- srv_filter_data(
        "filter_panel",
        datasets = datasets,
        active_datanames = active_datanames,
        data = data,
        is_active = is_active
      )
      is_transform_failed <- reactiveValues()
      transformed_teal_data <- srv_transform_teal_data(
        "data_transform",
        data = filtered_teal_data,
        transformators = modules$transformators,
        modules = modules,
        is_transform_failed = is_transform_failed
      )
      any_transform_failed <- reactive({
        any(unlist(reactiveValuesToList(is_transform_failed)))
      })

      observeEvent(any_transform_failed(), {
        if (isTRUE(any_transform_failed())) {
          shinyjs::hide("teal_module_ui")
          shinyjs::show("transform_failure_info")
        } else {
          shinyjs::show("teal_module_ui")
          shinyjs::hide("transform_failure_info")
        }
      })

      module_teal_data <- reactive({
        req(inherits(transformed_teal_data(), "teal_data"))
        all_teal_data <- transformed_teal_data()
        module_datanames <- .resolve_module_datanames(data = all_teal_data, modules = modules)
        all_teal_data[c(module_datanames, ".raw_data")]
      })

      srv_check_module_datanames(
        "validate_datanames",
        data = module_teal_data,
        modules = modules
      )

      summary_table <- srv_data_summary("data_summary", module_teal_data)

      observeEvent(input$data_summary_toggle, {
        bslib::toggle_sidebar(id = "teal_module_sidebar", open = TRUE)
        bslib::accordion_panel_open(id = "data_summary_accordion", values = TRUE)
        bslib::accordion_panel_close(id = "filter_panel-filters-main_filter_accordian", values = TRUE)
        bslib::accordion_panel_close(id = "data_transform_accordion", values = TRUE)
      })

      observeEvent(input$data_filters_toggle, {
        bslib::toggle_sidebar(id = "teal_module_sidebar", open = TRUE)
        bslib::accordion_panel_close(id = "data_summary_accordion", values = TRUE)
        bslib::accordion_panel_open(id = "filter_panel-filters-main_filter_accordian", values = TRUE)
        bslib::accordion_panel_close(id = "data_transform_accordion", values = TRUE)
      })

      observeEvent(input$data_transforms_toggle, {
        bslib::toggle_sidebar(id = "teal_module_sidebar", open = TRUE)
        bslib::accordion_panel_close(id = "data_summary_accordion", values = TRUE)
        bslib::accordion_panel_close(id = "filter_panel-filters-main_filter_accordian", values = TRUE)
        bslib::accordion_panel_open(id = "data_transform_accordion", values = TRUE)
      })

      # Call modules.
      if (!inherits(modules, "teal_module_previewer")) {
        obs_module <- .call_once_when(
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
    })

    module_out
  })
}

# This function calls a module server function.
.call_teal_module <- function(modules, datasets, data, reporter) {
  assert_reactive(data)

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
    args <- c(args, data = list(data))
  }

  if (is_arg_used(modules$server, "filter_panel_api")) {
    args <- c(args, filter_panel_api = teal.slice::FilterPanelAPI$new(datasets()))
  }

  if (is_arg_used(modules$server, "id")) {
    do.call(what = modules$server, args = args, quote = TRUE)
  } else {
    do.call(what = callModule, args = c(args, list(module = modules$server)), quote = TRUE)
  }
}

.resolve_module_datanames <- function(data, modules) {
  stopifnot("data must be teal_data object." = inherits(data, "teal_data"))
  if (is.null(modules$datanames) || identical(modules$datanames, "all")) {
    names(data)
  } else {
    intersect(
      names(data), # Keep topological order from teal.data::names()
      .include_parent_datanames(modules$datanames, teal.data::join_keys(data))
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
.call_once_when <- function(eventExpr, # nolint: object_name.
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

#' @keywords internal
.get_breadcrum <- function(module) {
  if (is.null(module$group)) {
    breadcrumb_items <- c("Home", module$label)
  } else {
    breadcrumb_items <- c("Home", module$group, module$label)
  }
  paste(breadcrumb_items, collapse = " / ")
}


#' @keywords internal
.teal_custom_nav_deps <- function() {
  htmltools::htmlDependency(
    name = "module-navigation",
    version = utils::packageVersion("teal"),
    package = "teal",
    src = "module-navigation",
    stylesheet = "module-navigation.css",
    script = "module-navigation.js"
  )
}

#' Custom Navigation Widget for `teal_modules`
#'
#' @param id (`character(1)`) The ID of the navigation widget. Which returns the `module_id` of the active module.
#' @param modules (`list`) A `teal_modules` object.
#' @param modules_ui (`list`) A list of UI elements that should be linked with the `module_id` of the.`teal_modules`
#' @keywords internal
.teal_custom_nav <- function(id, modules, modules_ui, selected_module = NULL) {
  active_module_id <- shiny::restoreInput(id, default = names(modules)[1])
  tags$div(
    class = "teal-modules-wrapper tabbable",
    .teal_custom_nav_deps(),
    tags$ul(
      id = id,
      style = "background: light-green",
      class = "nav shiny-tab-input",
      `data-tabsetid` = "test",
      tags$div(
        class = "dropdown",
        onmouseover = "initNavigationMouseOver.call(this)",
        onmouseout = "initNavigationMouseOut.call(this)",
        tags$a(
          class = "nav-item-custom dropdown-toggle active",
          role = "button",
          id = "modulesDropdown",
          "Modules"
        ),
        tags$div(
          class = "dropdown-menu",
          !!!lapply(names(modules), function(module_id) {
            module <- modules[[module_id]]
            tags$a(
              href = paste0("#", id, module_id),
              `data-bs-toggle` = "tab",
              `data-value` = module_id,
              `class` = ifelse(
                module_id == active_module_id,
                "nav-link btn-default active",
                "nav-link btn-default"
              ),
              module$label
            )
          })
        )
      )
    ),
    tags$div(
      class = "tab-content",
      `data-tabsetid` = "test",
      !!!lapply(names(modules), function(module_id) {
        module <- modules[[module_id]]
        tags$div(
          id = paste0(id, module_id),
          class = ifelse(
            module_id == active_module_id,
            "tab-pane active",
            "tab-pane"
          ),
          modules_ui[[module_id]]
        )
      })
    )
  )
}
