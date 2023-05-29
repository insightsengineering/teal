# Module that adds the filter panel as a shared module on the right of each of the
# top-level modules, shared across all modules

#' Add right filter panel into each of the top-level `teal_modules` UIs.
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
#' @inheritParams ui_teal_with_splash
#' @param modules (`teal_modules`) the modules which will be displayed in the teal application.
#'   See [modules()] and [module()] for more details.
#' @inheritParams init
#' @param datasets (`FilteredData`)\cr
#'   object to store filter state and filtered datasets, shared across modules. For more
#'   details see [`teal.slice::FilteredData`]
#'
#' @return A `tagList` of The main menu, place holders for filters and
#'   place holders for the teal modules
#' @keywords internal
#'
#' @examples
#'
#' mods <- teal:::get_dummy_modules()
#' datasets <- teal:::get_dummy_datasets()
#'
#' app <- shinyApp(
#'   ui = function() {
#'     tagList(
#'       teal:::include_teal_css_js(),
#'       textOutput("info"),
#'       fluidPage( # needed for nice tabs
#'         ui_tabs_with_filters("dummy", modules = mods, datasets = datasets)
#'       )
#'     )
#'   },
#'   server = function(input, output, session) {
#'     active_module <- srv_tabs_with_filters(
#'       id = "dummy",
#'       datasets = datasets,
#'       modules = mods,
#'       filter = NULL
#'     )
#'     output$info <- renderText({
#'       paste0("The currently active tab name is ", active_module()$label)
#'     })
#'   }
#' )
#' \dontrun{
#' runApp(app)
#' }
#'
#'
#' # An example with two filter panels in two apps side-by-side
#' datasets1 <- datasets2 <- datasets
#' app <- shinyApp(
#'   ui = function() {
#'     tagList(
#'       teal:::include_teal_css_js(),
#'       textOutput("info"),
#'       fluidPage( # needed for nice tabs
#'         fluidRow(
#'           column(6, teal:::ui_tabs_with_filters("app1", modules = mods, datasets = datasets1)),
#'           column(6, teal:::ui_tabs_with_filters("app2", modules = mods, datasets = datasets2))
#'         )
#'       )
#'     )
#'   },
#'   server = function(input, output, session) {
#'     active_module1 <- teal:::srv_tabs_with_filters(
#'       id = "app1",
#'       datasets = datasets1,
#'       modules = mods,
#'       filter = NULL
#'     )
#'     active_module2 <- teal:::srv_tabs_with_filters(
#'       id = "app2",
#'       datasets = datasets2,
#'       modules = mods,
#'       filter = NULL
#'     )
#'     output$info <- renderText({
#'       paste0(
#'         "The currently active tab names are: ",
#'         active_module1()$label, ", ", active_module1()$label
#'       )
#'     })
#'   }
#' )
#' \dontrun{
#' runApp(app)
#' }
ui_tabs_with_filters <- function(id, modules, datasets) {
  stopifnot(
    # `teal_module` not supported because we insert the filters into the UI below
    is(modules, "teal_modules"),
    is(datasets, "FilteredData")
  )
  ns <- NS(id)

  teal_ui <- ui_nested_tabs(ns("root"), modules = modules, datasets)
  filter_panel_btn <- tags$li(
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

  # stopifnot(length(teal_ui$children) == 2)
  # teal_ui$children[[1]] contains links to tabs
  # teal_ui$children[[2]] contains actual tab contents

  # # adding filter_panel_btn to the tabsetPanel pills
  teal_ui$children[[1]] <- tagAppendChild(teal_ui$children[[1]], filter_panel_btn)

  teal_ui$children <- list(
    teal_ui$children[[1]],
    tags$hr(class = "my-2"),
    teal_ui$children[[2]]
  )

  teal_ui
}

#' Server function
#'
#' @inheritParams srv_teal_with_splash
#' @param datasets (`FilteredData`)\cr
#'   object to store filter state and filtered datasets, shared across modules. For more
#'   details see [`teal.slice::FilteredData`].
#' @param reporter (`Reporter`) object from `teal.reporter`
#' @return `reactive` currently selected active_module
#' @keywords internal
srv_tabs_with_filters <- function(id, datasets, modules, reporter = teal.reporter::Reporter$new(), filter) {
  checkmate::assert_class(datasets, "FilteredData")
  checkmate::assert_class(reporter, "Reporter")
  moduleServer(id, function(input, output, session) {
    logger::log_trace(
      "srv_tabs_with_filters initializing the module with datasets { paste(datasets$datanames(), collapse = ' ') }."
    )

    # set filterable variables for each dataset
    teal.slice::set_filter_state(datasets = datasets, filter = filter)
    modules_out <- active_module <- srv_nested_tabs(id = "root", datasets = datasets, modules = modules, reporter = reporter)
    filter_manager_modal_srv("filter_manager", modules_out)

    showNotification("Data loaded - App fully started up")
    logger::log_trace(
      "srv_tabs_with_filters initialized the module with datasets { paste(datasets$datanames(), collapse = ' ') }."
    )
    return(NULL)
  })
}
