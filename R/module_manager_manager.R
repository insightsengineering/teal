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
  tags$button(
    id = ns("show"),
    class = "btn action-button filter_manager_button",
    title = "Show filters manager modal",
    icon("gear")
  )
}

#' @rdname module_filter_manager_modal
manager_manager_srv <- function(id, filtered_data_list, filter) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$show, {
      logger::log_trace("filter_manager_modal_srv@1 show button has been clicked.")
      showModal(
        modalDialog(
          filter_manager_ui(session$ns("filter_manager")),
          size = "l",
          footer = NULL,
          easyClose = TRUE
        )
      )
    })

    filter_manager_srv("filter_manager", filtered_data_list, filter)
  })
}
