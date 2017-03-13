ui_add_filter_variable <- function(id, dataname) {
  
  ns <- NS(id)
  selectInput(ns("variables"), label=dataname , choices=NULL)
  
  #    actionButton(ns("add"), label=NULL, icon=icon("plus"))
  
}