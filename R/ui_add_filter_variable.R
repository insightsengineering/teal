ui_add_filter_variable <- function(id, dataname) {

  ns <- NS(id)

  div( class = paste0("teal_filter_", tolower(dataname)),
    selectInput(ns("variables"), label=dataname , choices=NULL)
  )


  #    actionButton(ns("add"), label=NULL, icon=icon("plus"))

}
