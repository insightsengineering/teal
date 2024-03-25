#' Manager bar module
#'
#' Bar of buttons that open modal dialogs.
#'
#' Creates a bar of buttons that open modal dialogs where manager modules reside.
#' Currently contains three modules:
#' - [`module_filter_manager`]
#' - [`module_snapshot_manager`]
#' - [`module_bookmark_manager`]
#'
#' The bar is placed in the `teal` app UI, next to the filter panel hamburger.
#'
#' @name module_wunder_bar
#' @aliases wunder_bar wunder_bar_module
#'
#' @param id (`character(1)`) `shiny` module instance id.
#' @inheritParams module_filter_manager
#'
#' @return Nothing is returned.

#' @rdname module_wunder_bar
#' @keywords internal
wunder_bar_ui <- function(id, modules) {
  ns <- NS(id)
  tagList(
    title = "",
    tags$button(
      id = ns("show_filter_manager"),
      class = "btn action-button wunder_bar_button",
      title = "View filter mapping",
      suppressMessages(icon("solid fa-grip"))
    ),
    tags$button(
      id = ns("show_snapshot_manager"),
      class = "btn action-button wunder_bar_button",
      title = "Manage filter state snapshots",
      icon("camera")
    ),
    bookmark_module_ui(ns("bookmark_manager"), modules)
  )
}

#' @rdname module_wunder_bar
#' @keywords internal
wunder_bar_srv <- function(id, datasets, filter, modules) {
  moduleServer(id, function(input, output, session) {
    logger::log_trace("wunder_bar_srv initializing")

    setBookmarkExclude(c("show_filter_manager", "show_bookmark_manager", "show_bookmark_manager"))

    ns <- session$ns

    observeEvent(input$show_filter_manager, {
      logger::log_trace("wunder_bar_srv@1 show_filter_manager button has been clicked.")
      showModal(
        modalDialog(
          filter_manager_ui(ns("filter_manager")),
          size = "l",
          footer = NULL,
          easyClose = TRUE
        )
      )
    })

    observeEvent(input$show_snapshot_manager, {
      logger::log_trace("wunder_bar_srv@1 show_snapshot_manager button has been clicked.")
      showModal(
        modalDialog(
          snapshot_manager_ui(ns("snapshot_manager")),
          size = "m",
          footer = NULL,
          easyClose = TRUE
        )
      )
    })


    filter_manager_results <- filter_manager_srv(
      id = "filter_manager",
      datasets = datasets,
      filter = filter
    )
    snapshot_history <- snapshot_manager_srv(
      id = "snapshot_manager",
      slices_global = filter_manager_results$slices_global,
      mapping_matrix = filter_manager_results$mapping_matrix,
      datasets = filter_manager_results$datasets_flat
    )
    bookmark_history <- bookmark_manager_srv(
      id = "bookmark_manager",
      modules = modules
    )
  })
}
