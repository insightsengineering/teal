#' Calls all `modules`
#'
#' Modules create navigation bar with drop-down menu and tab content. Each `teal_module` is called recursively
#' according to the structure of `modules` argument. This is a custom module which utilizes shiny/Bootstrap
#' `.nav` class. `modules` are called with an `id` derived from `teal_module`'s label and labels of its
#' ancestors (if any).
#'
#' ### Functions
#'
#' - `ui/srv_teal_module` - wrapper module which links drop-down buttons with modules panels.
#'   Here `input$active_module_id` is instantiated.
#' - `.ui/srv_teal_module` - recursive S3 method which calls each module
#' - `.teal_navbar_append` - wrapper for [htmltools::tagAppendChild()] to add any element to navigation bar.
#' - `.teal_navbar_insert_ui` - wrapper for [shiny::insertUI()] to insert any element to navigation bar.
#' - `.teal_navbar_menu` - UI function to create a drop-down menu for navigation bar.
#'
#' ### Utilizing `.nav` class
#'
#' No extra `javascript` or server functionality were introduced to have navigation buttons toggle between
#' tab panels. This works thanks to `.nav` container which links `.nav-link` buttons `href = #<module id>`
#' attribute with `.tab-pane`'s `id = <module id>` (see ``.ui_teal_module.teal_module`).
#'
#' ### Initialization and isolation of the `teal_module`(s)
#'
#' Modules are initialized only when they are active. This speeds up app initialization and on
#' startup only the first module is activated and its outputs are calculated.
#' Only the active module is listening to reactive events. This way, modules are isolated and only
#' one can run at any given time. This makes the app more efficient by reducing unnecessary
#' computations on server side.
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
#'  which implies the filter-panel to be "global". When `NULL` then filter-panel is "module-specific".
#'
#' @param reporter (`Reporter`, singleton)
#'  Stores reporter-cards appended in the server of `teal_module`.
#'
#' @param data_load_status (`reactive` returning `character(1)`)
#'  Determines action dependent on a data loading status:
#'  - `"ok"` when `teal_data` is returned from the data loading.
#'  - `"teal_data_module failed"` when [teal_data_module()] didn't return `teal_data`. Disables tab buttons.
#'  - `"external failed"` when a `reactive` passed to `srv_teal(data)` didn't return `teal_data`. Hides the whole tab
#'    panel.
#'
#' @param active_module_id (`reactive` returning `character(1)`)
#'   `id` of the currently active module. This helps to determine which module can listen to reactive events.
#'
#' @return
#' Output of currently active module.
#' - `srv_teal_module.teal_module` returns `reactiveVal` containing output of the called module.
#' - `srv_teal_module.teal_modules` returns output of modules in a list following the hierarchy of `modules`
#'
#' @keywords internal
NULL


#' @rdname module_teal_module
ui_teal_module <- function(id, modules) {
  ns <- NS(id)
  active_module_id <- restoreInput(
    ns("active_module_id"),
    unlist(modules_slot(modules, "path"), use.names = FALSE)[1]
  )

  module_items <- .ui_teal_module(id = ns("nav"), modules = modules, active_module_id = active_module_id)

  tags$div(
    class = "teal-modules-wrapper",
    htmltools::htmlDependency(
      name = "module-navigation",
      version = utils::packageVersion("teal"),
      package = "teal",
      src = "module-navigation",
      stylesheet = "module-navigation.css"
    ),
    tags$ul(
      id = ns("active_module_id"),
      style = "align-items: center; gap: 1em; font-size: large;",
      class = "teal-navbar nav shiny-tab-input", # to mimic nav and mimic tabsetPanel
      `data-tabsetid` = "test",
      .teal_navbar_menu(
        !!!module_items$link,
        label = sprintf("Module (%d)", length(unlist(modules_slot(modules, "label")))),
        class = "teal-modules-tree",
        icon = "diagram-3-fill"
      )
    ),
    tags$div(class = "tab-content", module_items$tab_pane)
  )
}

#' @rdname module_teal_module
srv_teal_module <- function(id,
                            data,
                            modules,
                            datasets = NULL,
                            slices_global,
                            reporter = teal.reporter::Reporter$new(),
                            data_load_status = reactive("ok")) {
  moduleServer(id, function(input, output, session) {
    .srv_teal_module(
      id = "nav",
      data = data,
      modules = modules,
      datasets = datasets,
      slices_global = slices_global,
      reporter = reporter,
      data_load_status = data_load_status,
      active_module_id = reactive(input$active_module_id)
    )
  })
}

#' @rdname module_teal_module
.teal_navbar_append <- function(navbar, child) {
  tagAppendChild(tag = navbar, child = child, .cssSelector = ".teal-navbar")
}

#' @rdname module_teal_module
.teal_navbar_insert_ui <- function(ui, where = "afterBegin", session = getDefaultReactiveDomain()) {
  insertUI(
    selector = ".teal-navbar",
    where = where,
    ui = ui,
    session = session
  )
}

#' @rdname module_teal_module
.teal_navbar_menu <- function(..., id = NULL, label = NULL, class = NULL, icon = NULL) {
  tags$div(
    class = "dropdown nav-item-custom",
    .dropdown_button(
      id = id,
      label = label,
      icon = icon
    ),
    tags$div(
      class = "dropdown-menu",
      tags$ul(class = class, !!!rlang::list2(...))
    )
  )
}

#' @rdname module_teal_module
.ui_teal_module <- function(id, modules, active_module_id) {
  checkmate::assert_multi_class(modules, c("teal_modules", "teal_module", "shiny.tag"))
  UseMethod(".ui_teal_module", modules)
}

#' @rdname module_teal_module
#' @export
.ui_teal_module.default <- function(id, modules, active_module_id) {
  stop("Modules class not supported: ", paste(class(modules), collapse = " "))
}

#' @rdname module_teal_module
#' @export
.ui_teal_module.teal_modules <- function(id, modules, active_module_id) {
  items <- mapply(
    FUN = .ui_teal_module,
    id = NS(id, .label_to_id(sapply(modules$children, `[[`, "label"))),
    modules = modules$children,
    active_module_id = active_module_id,
    SIMPLIFY = FALSE
  )

  list(
    link = tagList(
      if (length(modules$label)) tags$li(tags$span(modules$label, class = "module-group-label")),
      tags$li(tags$ul(lapply(items, `[[`, "link")))
    ),
    tab_pane = tagList(lapply(items, `[[`, "tab_pane"))
  )
}

#' @rdname module_teal_module
#' @export
.ui_teal_module.teal_module <- function(id, modules, active_module_id) {
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
  container_id <- ns("wrapper")
  module_id <- modules$path

  link <- tags$li(
    tags$a(
      href = paste0("#", container_id), # links button with module content in `tab-content` with same id.
      `data-bs-toggle` = "tab", # signals shiny to treat this element as bootstrap tab buttons for toggle.
      `data-value` = module_id, # this data is set as the shiny input.
      class = c("nav-link", "module-button", "btn-default", if (identical(module_id, active_module_id)) "active"),
      # `nav-link` is required to mimic bslib tab panel.
      modules$label
    )
  )

  tab_pane <- div(
    id = container_id,
    class = c("tab-pane", "teal_module", if (identical(module_id, active_module_id)) "active"),
    tagList(
      .modules_breadcrumb(modules),
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
                ns("wrapper")
              )
            )
          )
        )
      } else {
        ui_teal
      }
    )
  )

  list(link = link, tab_pane = tab_pane)
}

#' @rdname module_teal_module
.srv_teal_module <- function(id,
                             data,
                             modules,
                             datasets = NULL,
                             slices_global,
                             reporter = teal.reporter::Reporter$new(),
                             data_load_status = reactive("ok"),
                             active_module_id = reactive(TRUE)) {
  checkmate::assert_string(id)
  assert_reactive(data)
  checkmate::assert_multi_class(modules, c("teal_modules", "teal_module"))
  assert_reactive(datasets, null.ok = TRUE)
  checkmate::assert_class(slices_global, ".slicesGlobal")
  checkmate::assert_class(reporter, "Reporter")
  assert_reactive(data_load_status)
  UseMethod(".srv_teal_module", modules)
}

#' @rdname module_teal_module
#' @export
.srv_teal_module.default <- function(id,
                                     data,
                                     modules,
                                     datasets = NULL,
                                     slices_global,
                                     reporter = teal.reporter::Reporter$new(),
                                     data_load_status = reactive("ok"),
                                     active_module_id = reactive(TRUE)) {
  stop("Modules class not supported: ", paste(class(modules), collapse = " "))
}

#' @rdname module_teal_module
#' @export
.srv_teal_module.teal_modules <- function(id,
                                          data,
                                          modules,
                                          datasets = NULL,
                                          slices_global,
                                          reporter = teal.reporter::Reporter$new(),
                                          data_load_status = reactive("ok"),
                                          active_module_id = reactive(TRUE)) {
  moduleServer(id = id, module = function(input, output, session) {
    logger::log_debug("srv_teal_module.teal_modules initializing the module { deparse1(modules$label) }.")
    modules_output <- mapply(
      function(id, modules) {
        .srv_teal_module(
          id = id,
          modules = modules,
          data = data,
          datasets = datasets,
          slices_global = slices_global,
          reporter = reporter,
          data_load_status = data_load_status,
          active_module_id = active_module_id
        )
      },
      id = .label_to_id(sapply(modules$children, `[[`, "label")),
      modules = modules$children,
      SIMPLIFY = FALSE
    )

    modules_output
  })
}

#' @rdname module_teal_module
#' @export
.srv_teal_module.teal_module <- function(id,
                                         data,
                                         modules,
                                         datasets = NULL,
                                         slices_global,
                                         reporter = teal.reporter::Reporter$new(),
                                         data_load_status = reactive("ok"),
                                         active_module_id = reactive(TRUE)) {
  logger::log_debug("srv_teal_module.teal_module initializing the module: { deparse1(modules$label) }.")
  moduleServer(id = id, module = function(input, output, session) {
    module_out <- reactiveVal()
    module_id <- modules$path
    is_active <- reactive({
      identical(data_load_status(), "ok") && identical(module_id, active_module_id())
    })
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
        bslib::accordion_panel_close(id = "filter_panel-filters-main_filter_accordion", values = TRUE)
        bslib::accordion_panel_close(id = "data_transform_accordion", values = TRUE)
      })

      observeEvent(input$data_filters_toggle, {
        bslib::toggle_sidebar(id = "teal_module_sidebar", open = TRUE)
        bslib::accordion_panel_close(id = "data_summary_accordion", values = TRUE)
        bslib::accordion_panel_open(id = "filter_panel-filters-main_filter_accordion", values = TRUE)
        bslib::accordion_panel_close(id = "data_transform_accordion", values = TRUE)
      })

      observeEvent(input$data_transforms_toggle, {
        bslib::toggle_sidebar(id = "teal_module_sidebar", open = TRUE)
        bslib::accordion_panel_close(id = "data_summary_accordion", values = TRUE)
        bslib::accordion_panel_close(id = "filter_panel-filters-main_filter_accordion", values = TRUE)
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

.modules_breadcrumb <- function(module) {
  tags$div(
    style = "color: var(--bs-secondary); font-size: large; opacity: 0.6; margin: 0 0.5em 0.5em 0.5em;",
    paste("Home", module$path, sep = " / ")
  )
}
