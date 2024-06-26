#' Filter panel module in teal
#'
#' Reactive filter panel module in teal
#'
#' @param id (`character(1)`) module id
#' @param filter (`teal_slices`) filter object
#' @param datasets (`reactive`) reactive `FilteredData` object
#' @name module_teal
#' @keywords internal
NULL

#' @rdname module_teal
ui_filter_panel <- function(id) {
  ns <- shiny::NS(id)
  uiOutput(ns("panel"))
}

#' @rdname module_teal
srv_filter_panel <- function(id, filter, datasets) {
  checkmate::assert_class(filter, "teal_slices")
  checkmate::assert_class(datasets, "reactive")
  moduleServer(id, function(input, output, session) {
    output$panel <- renderUI({
      req(datasets())
      logger::log_trace("srv_filter_panel rendering filter panel.")
      datasets()$srv_filter_panel("filters")
      datasets()$ui_filter_panel(session$ns("filters"))
    })
  })
}
