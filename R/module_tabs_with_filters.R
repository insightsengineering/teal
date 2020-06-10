#' Add right filter panel into each main tab of the teal_modules UI.
#' # todo: does this work with depth > 2?
#'
#' The `ui_nested_tabs` function returns a nested tabbed UI corresponding
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
#' @examples
#' mods <- get_dummy_modules()
#' datasets <- get_dummy_datasets()
#' shinyApp(
#'   ui = function() {
#'     tagList(
#'       include_teal_css_js(),
#'       textOutput("info"),
#'       fluidPage( # needed for nice tabs
#'         ui_tabs_with_filters("dummy", modules = mods, datasets = datasets)
#'       )
#'     )
#'   },
#'   server = function(input, output, session) {
#'     active_module <- callModule(srv_tabs_with_filters, "dummy", modules = mods, datasets = datasets)
#'     output$info <- renderText({
#'       paste0("The currently active tab name is ", active_module()$label)
#'     })
#'   }
#' ) %>% invisible() # to not run
ui_tabs_with_filters <- function(id, modules, datasets) {
  stopifnot(
    # `teal_module` not supported because we insert the filters into the UI below
    is(modules, "teal_modules"),
    is(datasets, "FilteredData")
  )

  ns <- NS(id)

  # use isolate because we assume that the number of datasets does not change over the course of the teal app
  # this will just create placeholders which are shown only if non-empty
  filter_and_info_ui <- ui_filter_panel(ns("filter_panel"), datanames = isolate(datasets$datanames()))

  # modules must be teal_modules, not teal_module; otherwise we will get the UI and not a tabsetPanel of UIs
  teal_ui <- ui_nested_tabs(ns("modules_ui"), modules = modules, datasets)

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

# returns active_module
srv_tabs_with_filters <- function(input, output, session, modules, datasets) {
  active_module <- callModule(srv_nested_tabs, "modules_ui", modules, datasets)

  active_datanames <- reactive({
    #  todo: put into other module
    # todo: make reactiveEvent


    active_datanames <- active_module()$filter
    if (identical(active_datanames, "all")) {
      active_datanames <- datasets$datanames()
    }
    # always add ADSL because the other datasets are filtered based on ADSL
    active_datanames <- union("ADSL", active_datanames)
    return(list_adsl_first(active_datanames))
  })

  callModule(srv_filter_panel, "filter_panel", datasets, reactive(active_datanames()))

  return(active_module)
}
