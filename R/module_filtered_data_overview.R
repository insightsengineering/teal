# Module to compare the number of patients in the unfiltered and filtered dataset.

#' Creates the UI for the module showing counts for each dataset
#' contrasting the filtered to the full unfiltered dataset
#'
#' Per dataset, it displays
#' the number of rows/observations in each dataset,
#' the number of unique subjects.
#'
#' @param id module id
#' # todo: example
ui_filtered_data_overview <- function(id) {
  ns <- NS(id)

  div(
    style = "overflow: overlay",
    tableOutput(ns("table"))
  )
}

#' Server function to display the number of patients in the filtered and unfiltered
#' data
#'
#' @md
#' @inheritParams srv_shiny_module_arguments
#' @param datanames `function / reactive returning a character vector` datanames
#'   to show information for; if `"all"`, takes all datanames, if `NULL`
srv_filtered_data_overview <- function(input, output, session, datasets, datanames = function() "all") {
  stopifnot(
    is(datasets, "FilteredData"),
    is.function(datanames)
  )

  output$table <- renderTable({
    .log("update uifiltersinfo")

    datanames <- handle_active_datanames(datasets, datanames())

    observations <- vapply(
      X = datanames,
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
      X = datanames,
      FUN = function(dataname, datasets) {
        paste0(
          datasets$get_data_info(dataname, filtered = TRUE)$patients, "/",
          datasets$get_data_info(dataname, filtered = FALSE)$patients
        )
      },
      FUN.VALUE = character(1),
      dataset = datasets
    )
    data.frame(
      Dataset = datanames,
      Obs = observations,
      Subjects = subjects
    )
  }, width = "100%")

  return(invisible(NULL))
}
