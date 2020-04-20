#' Creates the UI for the module showing counts for each dataset
#' contrasting the filtered to the full unfiltered dataset
#'
#' Per dataset, it displays
#' the number of rows/observations in each dataset,
#' the number of unique subjects.
#'
#' @param id module id
ui_filter_info <- function(id) {
  ns <- NS(id)

  div(
    style = "overflow: overlay",
    tableOutput(ns("table"))
  )
}

srv_filter_info <- function(input, output, session, datasets) {

  output$table <- renderTable({
    .log("update uifiltersinfo")

    observations <- vapply(
      X = datasets$datanames(),
      FUN = function(dataname, datasets) {
        paste0(
          datasets$get_data_info(dataname, filtered = TRUE)$dim[1], "/",
          datasets$get_data_info(dataname = dataname, filtered = FALSE)$dim[1]
        )
      },
      FUN.VALUE = character(1),
      dataset = datasets
    )
    subjects <- vapply(
      X = datasets$datanames(),
      FUN = function(dataname, datasets) {
        paste0(
          datasets$get_data_info(dataname, filtered = TRUE)$patients, "/",
          datasets$get_data_info(dataname = dataname, filtered = FALSE)$patients
        )
      },
      FUN.VALUE = character(1),
      dataset = datasets
    )
    data.frame(
      Dataset = datasets$datanames(),
      Obs = observations,
      Subjects = subjects
    )
  }, width = "100%")
}
