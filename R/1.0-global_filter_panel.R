ui_global_filter_panel <- function(id) {
  ns <- shiny::NS(id)
  uiOutput(ns("panel"))
}

srv_global_filter_panel <- function(id, filter, datasets) {
  moduleServer(id, function(input, output, session) {
    output$panel <- renderUI({
      req(datasets())
      datasets()$srv_filter_panel("filters")
      datasets()$ui_filter_panel(session$ns("filters"))
    })
  })
}
