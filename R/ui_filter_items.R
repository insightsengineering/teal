ui_filter_items <- function(id, title=NULL) {
  
  ns <- NS(id)
  
  uiOutput(ns("uifilters"))
  
}