ui_add_filter_variable <- function(id, dataname) {

  ns <- NS(id)

  div( class = paste0("teal_filter_", tolower(dataname)),
    selectInput(ns("variables"), label=dataname , choices=NULL),
    uiOutput(ns("warning"))
  )


  #    actionButton(ns("add"), label=NULL, icon=icon("plus"))

}
