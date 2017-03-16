ui_filter_items <- function(id, dataname, title=NULL) {

  ns <- NS(id)

  div(class = paste0("teal_filter_", tolower(dataname)),
      uiOutput(ns("uifilters"))
  )

}
