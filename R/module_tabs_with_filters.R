#' Add right filter panel into each of the top-level `teal_modules` UIs
#'
#' The [ui_nested_tabs] function returns a nested tabbed UI corresponding
#' to the nested modules.
#' This function adds the right filter panel to each main tab.
#'
#' The right filter panel's filter choices affect the `datasets` object. Therefore,
#' all modules using the same `datasets` share the same filters.
#'
#' This works with nested modules of depth greater than 2, though the filter
#' panel is inserted at the right of the modules at depth 1 and not at the leaves.
#'
#' @name module_tabs_with_filters
#'
#' @inheritParams module_teal
#'
#' @param datasets (named `list` of `FilteredData`)
#'   object to store filter state and filtered datasets, shared across modules. For more
#'   details see [`teal.slice::FilteredData`]. Structure of the list must be the same as structure
#'   of the `modules` argument and list names must correspond to the labels in `modules`.
#'   When filter is not module-specific then list contains the same object in all elements.
#' @param reporter (`Reporter`) object from `teal.reporter`
#'
#' @return
#' A `shiny.tag.list` containing the main menu, placeholders for filters and placeholders for the `teal` modules.
#'
#' @keywords internal
#'
NULL

#' @rdname module_tabs_with_filters
ui_tabs_with_filters <- function(id, modules, datasets, filter = teal_slices()) {
  checkmate::assert_class(modules, "teal_modules")
  checkmate::assert_list(datasets, types = c("list", "FilteredData"))
  checkmate::assert_class(filter, "teal_slices")

  ns <- NS(id)
  is_module_specific <- isTRUE(attr(filter, "module_specific"))

  teal_ui <- ui_nested_tabs(ns("root"), modules = modules, datasets, is_module_specific = is_module_specific)
  filter_panel_btns <- tags$li(
    class = "flex-grow",
    tags$button(
      class = "btn action-button filter_hamburger", # see sidebar.css for style filter_hamburger
      href = "javascript:void(0)",
      onclick = "toggleFilterPanel();", # see sidebar.js
      title = "Toggle filter panels",
      icon("fas fa-bars")
    ),
    filter_manager_modal_ui(ns("filter_manager"))
  )
  teal_ui$children[[1]] <- tagAppendChild(teal_ui$children[[1]], filter_panel_btns)

  if (!is_module_specific) {
    # need to rearrange html so that filter panel is within tabset
    tabset_bar <- teal_ui$children[[1]]
    teal_modules <- teal_ui$children[[2]]
    filter_ui <- unlist(datasets)[[1]]$ui_filter_panel(ns("filter_panel"))
    list(
      tabset_bar,
      tags$hr(class = "my-2"),
      fluidRow(
        column(width = 9, teal_modules, class = "teal_primary_col"),
        column(width = 3, filter_ui, class = "teal_secondary_col")
      )
    )
  } else {
    teal_ui
  }
}

#' @rdname module_tabs_with_filters
srv_tabs_with_filters <- function(id,
                                  datasets,
                                  modules,
                                  reporter = teal.reporter::Reporter$new(),
                                  filter = teal_slices()) {
  checkmate::assert_class(modules, "teal_modules")
  checkmate::assert_list(datasets, types = c("list", "FilteredData"))
  checkmate::assert_class(reporter, "Reporter")
  checkmate::assert_class(filter, "teal_slices")

  moduleServer(id, function(input, output, session) {
    logger::log_trace("srv_tabs_with_filters initializing the module.")

    is_module_specific <- isTRUE(attr(filter, "module_specific"))
    manager_out <- filter_manager_modal_srv("filter_manager", filtered_data_list = datasets, filter = filter)

    active_module <- srv_nested_tabs(
      id = "root",
      datasets = datasets,
      modules = modules,
      reporter = reporter,
      is_module_specific = is_module_specific
    )

    if (!is_module_specific) {
      active_datanames <- reactive({
        if (identical(active_module()$datanames, "all")) {
          singleton$datanames()
        } else {
          include_parent_datanames(
            active_module()$datanames,
            singleton$get_join_keys()
          )
        }
      })
      singleton <- unlist(datasets)[[1]]
      singleton$srv_filter_panel("filter_panel", active_datanames = active_datanames)

      observeEvent(
        eventExpr = active_datanames(),
        handlerExpr = {
          script <- if (length(active_datanames()) == 0 || is.null(active_datanames())) {
            # hide the filter panel and disable the burger button
            "handleNoActiveDatasets();"
          } else {
            # show the filter panel and enable the burger button
            "handleActiveDatasetsPresent();"
          }
          shinyjs::runjs(script)
        },
        ignoreNULL = FALSE
      )
    }

    showNotification("Data loaded - App fully started up")
    logger::log_trace("srv_tabs_with_filters initialized the module")

    active_module
  })
}
