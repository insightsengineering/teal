ui_page_data_table <- function(id, datasets) {

  ns <- NS(id)

  datanames <- datasets$datanames()
  sel_data <- datanames[1]
  sel_varnames <- names(datasets$get_data(sel_data))

  tagList(
    fluidRow(
      div(class="col-md-3",
          radioButtons(ns("dataset"), "data", choices = datanames,
                       selected=sel_data, inline = TRUE),
          radioButtons(ns("dataraworfiltered"), NULL, choices = c("raw data"="raw", "filtered data"="filtered"),
                       selected="filtered", inline = TRUE),
          checkboxInput(ns("distinct"), "show only distinct rows", value = FALSE)
      ),
      div(class="col-md-9", selectInput(ns("variables"), "select variables",
                                        choices=sel_varnames, selected = head(sel_varnames),
                                        multiple = TRUE, width = "100%"))
    ),
    tags$hr(),
    fluidRow(
      div(class="col-md-12", DT::dataTableOutput(ns('tbl')))
    ),
    div(style="height:30px;")
  )
}
