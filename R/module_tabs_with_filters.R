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

  tabs_ui <- ui_nested_tabs(ns("root"), modules = modules, datasets, is_module_specific = is_module_specific)
  do.call(
    bslib::navset_tab,
    c(
      unname(tabs_ui),
      list(bslib::nav_spacer()),
      list(
        bslib::nav_item(wunder_bar_ui(ns("wunder_bar")))
      )
    )
  )
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
    wunder_bar_out <- wunder_bar_srv("wunder_bar", datasets, filter, modules)

    active_module <- srv_nested_tabs(
      id = "root",
      datasets = datasets,
      modules = modules,
      reporter = reporter,
      is_module_specific = is_module_specific
    )

    showNotification("Data loaded - App fully started up")
    logger::log_trace("srv_tabs_with_filters initialized the module")

    active_module
  })
}
