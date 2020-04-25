#' Initial user-interface of any teal app
#'
#' @param datasets \link{FilteredData} object where all datasets
#'   to be used inside the teal app are in
#' @inheritParams init
#'
#' @return A \code{tagList} of The main menu, place holders for filters and
#'   place holders for the teal modules
#'
#' @import shiny
init_ui <- function(datasets, modules) {

  tp <- create_ui(modules, datasets, idprefix = "teal_modules", is_root = TRUE)

  # use isolate because we assume that the number of datasets does not change over the course of the teal app
  # otherwise need dynamic UI
  datanames <- make_adsl_first(isolate(datasets$datanames()))

  # separate the nested tabs
  tp$children <- list(
    tp$children[[1]],
    tags$hr(style = "margin: 7px 0;"),
    fluidRow(
      column(9, tp$children[[2]]),
      column(
        3,
        shinyjs::hidden(div(
          id = "teal_filter-panel",
          div(
            id = "teal_filter_active_vars",
            class = "well",
            tags$label("Active Filter Variables", class = "text-primary", style = "margin-bottom: 15px;"),
            tagList(
              ui_filter_info("teal_filters_info")
            ),
            tagList(
              lapply(datanames, function(dataname) {
                ui_filter_items(paste0("teal_filters_", dataname), dataname)
              })
            )
          ),
          div(
            id = "teal_filter_add_vars",
            class = "well",
            tags$label("Add Filter Variables", class = "text-primary", style = "margin-bottom: 15px;"),
            tagList(
              lapply(datanames, function(dataname) {
                ui_add_filter_variable(paste0("teal_add_", dataname, "_filters"), dataname)
              })
            )
          )
        ))
      )
    )
  )
  tp
}
