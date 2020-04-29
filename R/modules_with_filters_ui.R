#' Initial user-interface of any teal app
#'
#' The `tab_nested_ui` function returns a nested tabbed UI corresponding
#' to the nested modules.
#' This function adds the right filter and info panel for each main tab.
#'
#' It uses the prefix "teal_modules" for the ids of the tabs. These can then be obtained
#' recursively by using the function `label_to_id`.
#'
#' For each dataname, it also adds the id `paste0("teal_add_", dataname, "_filter")` which allows
#' to add a filter variable for that dataname.
#' All the filters for the dataname are regrouped within a module with id
#' `paste0("teal_filters_", dataname)`.
#'
#'
#' @inheritParams init
#' @param datasets \link{FilteredData} object where all datasets
#'   to be used inside the teal app are in
#'
#' @return A \code{tagList} of The main menu, place holders for filters and
#'   place holders for the teal modules
#'
#' @import shiny
modules_with_filters_ui <- function(modules, datasets) {

  # use isolate because we assume that the number of datasets does not change over the course of the teal app
  # otherwise need dynamic UI
  datanames <- isolate(datasets$datanames())
  filter_and_info_ui <- div(
    id = "teal_filter-panel", # id not used elsewhere
    div(
      id = "teal_filter_active_vars", # id not used elsewhere
      class = "well",
      tags$label("Active Filter Variables", class = "text-primary", style = "margin-bottom: 15px;"),
      tagList(
        #tags$p("Filters info currently not shown")
        ui_filter_info("teal_filters_info")
      ),
      tagList(
        lapply(datanames, function(dataname) {
          ui_filter_items(paste0("teal_filters_", dataname), dataname)
        })
      )
    ),
    div(
      id = "teal_filter_add_vars", # id not used elsewhere
      class = "well",
      tags$label("Add Filter Variables", class = "text-primary", style = "margin-bottom: 15px;"),
      tagList(
        lapply(datanames, function(dataname) {
          ui_add_filter_variable(paste0("teal_add_", dataname, "_filter"), dataname)
        })
      )
    )
  )

  stopifnot(is(modules, "teal_modules")) # otherwise we will get the UI and not a tabsetPanel
  teal_ui <- tab_nested_ui(modules, datasets, idprefix = "teal_modules", is_root = TRUE)

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
