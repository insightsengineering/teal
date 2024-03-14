#' Filter manager modal
#'
#' Opens a modal containing the filter manager UI.
#'
#' @name module_filter_manager_modal
#' @inheritParams module_filter_manager
#' @keywords internal
#'
NULL

#' @rdname module_filter_manager_modal
wunder_bar_ui <- function(id) {
  ns <- NS(id)
  rev( # Reversing order because buttons show up in UI from right to left.
    tagList(
      tags$button(
        id = ns("show_filter_manager"),
        class = "btn action-button wunder_bar_button",
        title = "Show filter manager modal",
        suppressMessages(icon("solid fa-filter"))
      ),
      tags$button(
        id = ns("show_snapshot_manager"),
        class = "btn action-button wunder_bar_button",
        title = "Show snapshot manager modal",
        icon("camera")
      ),
      tags$button(
        id = ns("show_bookmark_manager"),
        class = "btn action-button wunder_bar_button",
        title = "Show bookmark manager modal",
        suppressMessages(icon("solid fa-bookmark"))
      )
    )
  )
}

#' @rdname module_filter_manager_modal
wunder_bar_srv <- function(id, filtered_data_list, filter) {
  moduleServer(id, function(input, output, session) {

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

    observeEvent(input$show_bookmark_manager, {
      logger::log_trace("wunder_bar_srv@1 show_bookmark_manager button has been clicked.")
      showModal(
        modalDialog(
          bookmark_manager_ui(ns("bookmark_manager")),
          size = "m",
          footer = NULL,
          easyClose = TRUE
        )
      )
    })

    filter_manager_results <- filter_manager_srv(
      id = "filter_manager",
      filtered_data_list = filtered_data_list,
      filter = filter
    )
    snapshot_history <- snapshot_manager_srv(
      id = "snapshot_manager",
      slices_global = filter_manager_results$slices_global,
      mapping_matrix = filter_manager_results$mapping_matrix,
      filtered_data_list = filter_manager_results$filtered_data_list
    )
    bookmark_manager_srv(
      id = "bookmark_manager",
      slices_global = filter_manager_results$slices_global,
      mapping_matrix = filter_manager_results$mapping_matrix,
      filtered_data_list = filter_manager_results$filtered_data_list,
      snapshot_history = snapshot_history
    )

  })
}
