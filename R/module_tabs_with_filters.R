# Module that adds the filter panel as a shared module on the right of each of the
# top-level modules, shared across all modules

#' Add right filter panel into each of the top-level `teal_modules` UIs.
#'
#' The `\link{ui_nested_tabs}` function returns a nested tabbed UI corresponding
#' to the nested modules.
#' This function adds the right filter panel to each main tab.
#'
#' The right filter panel's filter choices affect the `datasets` object. Therefore,
#' all modules using the same `datasets` share the same filters.
#'
#' This works with nested modules of depth greater than 2, though the filter
#' panel is inserted at the right of the modules at depth 1 and not at the leaves.
#'
#' @inheritParams init
#' @inheritParams srv_shiny_module_arguments
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
#'     active_module <- callModule(srv_tabs_with_filters, "dummy", datasets = datasets, modules = mods)
#'     output$info <- renderText({
#'       paste0("The currently active tab name is ", active_module()$label)
#'     })
#'   }
#' ) %>% invisible() # to not run
#'
#'
#' # An example with two filter panels in two apps side-by-side
#' mods <- get_dummy_modules()
#' datasets1 <- get_dummy_datasets()
#' datasets2 <- get_dummy_datasets()
#' shinyApp(
#'   ui = function() {
#'     tagList(
#'       include_teal_css_js(),
#'       textOutput("info"),
#'       fluidPage( # needed for nice tabs
#'         fluidRow(
#'           column(6, ui_tabs_with_filters("app1", modules = mods, datasets = datasets1)),
#'           column(6, ui_tabs_with_filters("app2", modules = mods, datasets = datasets2))
#'         )
#'       )
#'     )
#'   },
#'   server = function(input, output, session) {
#'     active_module1 <- callModule(
#'       srv_tabs_with_filters, "app1", datasets = datasets1, modules = mods
#'     )
#'     active_module2 <- callModule(
#'       srv_tabs_with_filters, "app2", datasets = datasets2, modules = mods
#'     )
#'     output$info <- renderText({
#'       paste0(
#'         "The currently active tab names are: ",
#'         active_module1()$label, ", ", active_module1()$label
#'       )
#'     })
#'   }
#' )  %>% invisible() # to not run
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

#' Server function
#'
#' @md
#' @inheritParams srv_shiny_module_arguments
#' @return `reactive` currently selected active_module
srv_tabs_with_filters <- function(input, output, session, datasets, modules) {
  active_module <- callModule(srv_nested_tabs, "modules_ui", datasets = datasets, modules = modules)

  active_datanames <- reactive({
    active_datanames <- active_module()$filter
    if (identical(active_datanames, "all")) {
      active_datanames <- datasets$datanames()
    }
    # always add ADSL because the other datasets are filtered based on ADSL
    active_datanames <- union("ADSL", active_datanames)
    return(list_adsl_first(active_datanames))
  })

  callModule(srv_filter_panel, "filter_panel", datasets, active_datanames)

  return(active_module)
}
