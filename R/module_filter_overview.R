# Module to compare the number of patients in the unfiltered and filtered dataset.

#' Creates the UI for the module showing counts for each dataset
#' contrasting the filtered to the full unfiltered dataset
#'
#' Per dataset, it displays
#' the number of rows/observations in each dataset,
#' the number of unique subjects.
#'
#' @param id module id
#' @examples
#' datasets <- teal:::get_dummy_datasets()
#' app <- shinyApp(ui = function() {
#'   tagList(
#'     teal:::include_teal_css_js(),
#'     teal:::ui_filter_overview("filter_panel")
#'   )
#' }, server = function(input, output, session) {
#'   callModule(
#'     teal:::srv_filter_overview, "filter_panel", datasets
#'   )
#' })
#' \dontrun{
#' runApp(app)
#' }
ui_filter_overview <- function(id) {
  ns <- NS(id)

  div(
    style = "overflow: overlay",
    tableOutput(ns("table"))
  )
}

#' Server function to display the number of patients in the filtered and unfiltered
#' data
#'
#' @inheritParams srv_shiny_module_arguments
#' @param datanames `function / reactive returning a character vector` datanames
#'   to show information for; if `"all"`, takes all datanames, if `NULL`
srv_filter_overview <- function(input, output, session, datasets, datanames = function() "all") {
  stopifnot(
    is(datasets, "FilteredData"),
    is.function(datanames) || is.reactive(datanames)
  )

  output$table <- renderTable({
    .log("update uifiltersinfo")

    datasets$get_filter_overview_tbl(datanames())
  }, width = "100%")

  return(invisible(NULL))
}
