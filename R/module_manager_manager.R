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
manager_manager_ui <- function(id) {
  ns <- NS(id)
  rev( # Reversing order because buttons show up in UI from right to left.
    tagList(
      tags$button(
        id = ns("show_filter_manager"),
        class = "btn action-button manager_manager_button",
        title = "Show filter manager modal",
        suppressMessages(icon("solid fa-filter"))
      ),
      tags$button(
        id = ns("show_snapshot_manager"),
        class = "btn action-button manager_manager_button",
        title = "Show snapshot manager modal",
        icon("camera")
      ),
      tags$button(
        id = ns("show_state_manager"),
        class = "btn action-button manager_manager_button",
        title = "Show state manager modal",
        suppressMessages(icon("solid fa-bookmark"))
      )
    )
  )
}

#' @rdname module_filter_manager_modal
manager_manager_srv <- function(id, filtered_data_list, filter) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    observeEvent(input$show_filter_manager, {
      logger::log_trace("manager_manager_modal_srv@1 show_filter_manager button has been clicked.")
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
      logger::log_trace("manager_manager_modal_srv@1 show_snapshot_manager button has been clicked.")
      showModal(
        modalDialog(
          snapshot_manager_ui(ns("snapshot_manager")),
          size = "m",
          footer = NULL,
          easyClose = TRUE
        )
      )
    })

    observeEvent(input$show_state_manager, {
      logger::log_trace("manager_manager_modal_srv@1 show_state_manager button has been clicked.")
      showModal(
        modalDialog(
          state_manager_ui(ns("state_manager")),
          size = "m",
          footer = NULL,
          easyClose = TRUE
        )
      )
    })

    filtrer_manager_results <- filter_manager_srv(
      id = "filter_manager",
      filtered_data_list = filtered_data_list,
      filter = filter
    )
    snapshot_history <- snapshot_manager_srv(
      id = "snapshot_manager",
      slices_global = filtrer_manager_results$slices_global,
      mapping_matrix = filtrer_manager_results$mapping_matrix,
      filtered_data_list = filtrer_manager_results$filtered_data_list
    )
    state_manager_srv(
      id = "state_manager",
      slices_global = filtrer_manager_results$slices_global,
      mapping_matrix = filtrer_manager_results$mapping_matrix,
      filtered_data_list = filtrer_manager_results$filtered_data_list,
      snapshot_history = snapshot_history
    )

  })
}
