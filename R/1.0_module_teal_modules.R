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
  module_ui <- div(
    id = id,
    class = "teal_module",
    uiOutput(ns("data_reactive"), inline = TRUE),
    tagList(
      if (depth >= 2L) tags$div(style = "mt-6"),
      do.call(modules$ui, args)
    )
  )

  module_ui
}

#' @rdname module_teal_module
srv_teal_module <- function(id, data_rv, datasets, modules, reporter = teal.reporter::Reporter$new()) {
  checkmate::assert_multi_class(modules, c("teal_modules", "teal_module"))
  checkmate::assert_class(reporter, "Reporter")
  UseMethod("srv_teal_module", modules)
}

#' @rdname module_teal_module
#' @export
srv_teal_module.default <- function(id, data_rv, datasets, modules, reporter = teal.reporter::Reporter$new()) {
  stop("Modules class not supported: ", paste(class(modules), collapse = " "))
}

#' @rdname module_teal_module
#' @export
srv_teal_module.teal_modules <- function(id, data_rv, datasets, modules, reporter = teal.reporter::Reporter$new()) {
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
srv_teal_module.teal_module <- function(id, data_rv, datasets, modules, reporter = teal.reporter::Reporter$new()) {
  logger::log_trace("srv_teal_module.teal_module initializing the module: { deparse1(modules$label) }.")
  moduleServer(id = id, module = function(input, output, session) {
    srv_global_filter_panel("filter_panel", teal_slices(), datasets)

    trigger_data <- reactiveVal(1L)
    trigger_module <- reactiveVal(NULL)
    output$data_reactive <- renderUI({
      lapply(data_rv(), function(x) x())
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
      filtered_teal_data <- eventReactive(
        trigger_data(),
        {
          .make_teal_data(modules, data_rv())
        }
      )
      data <- srv_teal_transform_data("transformers", filtered_teal_data, modules)
      args <- c(args, data = list(data))
    }

    # if (is_arg_used(modules$server, "filter_panel_api")) {
    #   filter_panel_api <- teal.slice::FilterPanelAPI$new(datasets)
    #   args <- c(args, filter_panel_api = filter_panel_api)
    # }

    # This function calls a module server function.
    call_module <- function() {
      if (is_arg_used(modules$server, "id")) {
        do.call(modules$server, args)
      } else {
        do.call(callModule, c(args, list(module = modules$server)))
      }
    }

    # Call modules.
    if (isTRUE(session$restoreContext$active)) {
      # When restoring bookmark, all modules must be initialized on app start.
      # Delayed module initiation (below) precludes restoring state b/c inputs do not exist when restoring occurs.
      call_module()
    } else if (inherits(modules, "teal_module_previewer")) {
      # Report previewer must be initiated on app start for report cards to be included in bookmarks.
      # When previewer is delayed, cards are bookmarked only if previewer has been initiated (visited).
      call_module()
    } else {
      # When app starts normally, modules are initialized only when corresponding tabs are clicked.
      # Observing trigger_module() induces the module only when output$data_reactive is triggered (see above).
      observeEvent(
        ignoreNULL = TRUE,
        once = TRUE,
        eventExpr = trigger_module(),
        handlerExpr = call_module()
      )
    }

    reactive(modules)
  })
}

.make_teal_data <- function(modules, data) {
  datasets <- isolate(lapply(data, function(x) x()))
  filter_code <- isolate(lapply(datasets, function(x) attr(x, "filter_code")))
  data_code <- attr(data, "code")
  all_code <- paste(unlist(c(data_code, filter_code)), collapse = "\n")
  tdata <- do.call(
    teal.data::teal_data,
    c(
      list(code = all_code),
      list(join_keys = attr(data, "join_keys")),
      datasets
    )
  )
  tdata@verified <- TRUE # todo: change to original value from from data
  tdata
}
