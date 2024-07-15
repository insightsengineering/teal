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
srv_filter_panel <- function(id, datasets, active_datanames) {
  checkmate::assert_class(datasets, "reactive")
  moduleServer(id, function(input, output, session) {
    output$panel <- renderUI({
      req(inherits(datasets(), "FilteredData"))
      isolate({
        # render will be triggered only when FilteredData object changes (not when filters change)
        # technically it means that teal_data_module needs to be refreshed
        logger::log_trace("srv_filter_panel rendering filter panel.")

        filtered_data <- datasets()
        filtered_data$srv_filter_panel("filters", active_datanames = active_datanames)
        # todo: make sure to bump the `teal.slice` version. Please use the branch `669_insertUI@main` in `teal.slice`.
        filtered_data$ui_filter_panel(session$ns("filters"), active_datanames = active_datanames())
      })
    })
  })
}
