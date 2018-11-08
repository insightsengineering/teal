

#' Data Table Viewer Teal Module
#'
#' A data table viewer shows the data using a paginated table.
#'
#' @inheritParams module
#' @param variables_selected a named list that says which variables should be
#'   initially  shown for which dataset
#'
#' @export
#'
#'
#' @examples
#'
#' \dontrun{
#' ASL <- generate_sample_data('ASL')
#'
#' x <- teal::init(
#'   data =  list(ASL = ASL),
#'   modules = root_modules(
#'     tm_data_table()
#'   )
#' )
#'
#' shinyApp(x$ui, x$server)
#' }
tm_data_table <- function(label = "data table", variables_selected=NULL) {
  module(
    label,
    server = srv_page_data_table,
    ui = ui_page_data_table,
    filters = "all",
    server_args = list(cache_selected = if (is.null(variables_selected)) list() else variables_selected),
    ui_args = list(datasets='teal_datasets')
  )
}



## ui function

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
          radioButtons(ns("dataraworfiltered"), NULL, choices = c("unfiltered data"="raw", "filtered data"="filtered"),
                       selected="filtered", inline = TRUE),
          checkboxInput(ns("distinct"), "show only distinct rows", value = FALSE)
      ),
      div(class="col-md-9", selectInput(ns("variables"), "select variables",
                                        choices=sel_varnames, selected = head(sel_varnames),
                                        multiple = TRUE, width = "100%"))
    ),
    tags$hr(),
    fluidRow(
      div(class="col-md-12", DT::dataTableOutput(ns('tbl'), width = "100%"))
    ),
    div(style="height:30px;")
  )
}

## data table

srv_page_data_table <- function(input, output, session, datasets, cache_selected = list()) {


  # select first 6 variables for each dataset if not otherwise specified
  for (name in setdiff(datasets$datanames(), names(cache_selected))) {
    cache_selected[[name]] <- head(names(datasets$get_data(name, filtered = FALSE, reactive=FALSE)), 6)
  }


  observe({

    dataname <- input$dataset


    validate(
      need(dataname, "need valid dataset name"),
      need(dataname %in% datasets$datanames(), paste("data", dataname, "was not specified"))
    )

    df <- datasets$get_data(dataname, filtered = FALSE, reactive = FALSE)
    choices <- names(df)

    vo <- cache_selected[[dataname]]

    selected <- if (is.null(vo)) head(choices, 6) else intersect(vo, choices)
    choices <- c(selected, setdiff(choices, selected))

    .log("data table, update variables for", dataname)

    updateSelectInput(session, "variables",
                      choices = choices,
                      selected = selected)

    cache_selected[[dataname]] <- selected

  })


  output$tbl <- DT::renderDataTable({

    dataname <- input$dataset
    is.filtered <- input$dataraworfiltered == "filtered"
    distinct <- input$distinct


    validate(need(dataname, "need valid dataname"))

    variables <- input$variables

    validate(need(variables, "need valid variable names"))

    .log("data table update", dataname)

    df <- datasets$get_data(dataname, filtered = is.filtered, reactive = TRUE)

    validate(need(df, paste("data", dataname, "is empty")))
    validate(need(all(variables %in% names(df)), "not all selected variables exist"))

    df_s <- if (distinct) dplyr::count_(df, variables) else df[variables]

    # filter = 'top'
    DT::datatable(
      df_s,
      options = list(
        searching = FALSE,
        pageLength = 30,
        lengthMenu = c(5, 15, 30, 100),
        scrollX = TRUE
      )
    )

  })

}
