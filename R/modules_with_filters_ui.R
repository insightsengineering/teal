#' Add right filter panel into each main tab of the teal_modules UI.
#' # todo: does this work with depth > 2?
#'
#' The `ui_tab_nested` function returns a nested tabbed UI corresponding
#' to the nested modules.
#' This function adds the right filter panel to each main tab.
#'
#' It uses the prefix "teal_modules" for the ids of the tabs. These can then be obtained
#' recursively by using the function `label_to_id`.
#'
#' @inheritParams init
#' @param datasets \link{FilteredData} object where all datasets
#'   to be used inside the teal app are in
#'
#' @return A \code{tagList} of The main menu, place holders for filters and
#'   place holders for the teal modules
#'
#' @import shiny
#  todo: put ui first and write server function
ui_modules_with_filters <- function(modules, datasets) {
  stopifnot(
    is(modules, "teal_modules"),
    is(datasets, "FilteredData")
  )

  # use isolate because we assume that the number of datasets does not change over the course of the teal app
  # this will just create placeholders which are shown only if non-empty
  filter_and_info_ui <- ui_filter_panel("filter_panel", datanames = isolate(datasets$datanames()))

  # modules must be teal_modules, not teal_module; otherwise we will get the UI and not a tabsetPanel of UIs
  teal_ui <- ui_tab_nested(modules, datasets, idprefix = "teal_modules", is_root = TRUE)

  stopifnot(length(teal_ui$children) == 2)
  # teal_ui$children[[1]] contains links to tabs
  # teal_ui$children[[2]] contains actual tab contents
  teal_ui$children <- list(
    teal_ui$children[[1]],
    tags$hr(style = "margin: 7px 0;"),
    fluidRow(
      column(9, teal_ui$children[[2]]),
      column(3, filter_and_info_ui)
    )
  )
  return(teal_ui)
}
