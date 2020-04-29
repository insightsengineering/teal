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

# datanames are datanames to show among those that are available
srv_filter_info <- function(input, output, session, datasets, datanames) {
  stopifnot(is.function(datanames))

  output$table <- renderTable({
    .log("update uifiltersinfo")

    datanames <- if_null(datanames(), datasets$datanames())

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
          datasets$get_data_info(dataname = dataname, filtered = FALSE)$patients
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
}

#' Makes ADSL appear first in the datanames
#'
#' This is useful in the UI as ADSL should always appear first in
#' any lists of the datasets.
#' If it does not contain ADSL, the list is returned unmodified.
#'
#' @md
#' @param datanames (`character` vector) datanames
make_adsl_first <- function(datanames) {
  if ("ADSL" %in% datanames) {
    # make ADSL first
    datanames <- c("ADSL", setdiff(datanames, "ADSL"))
  }
  return(datanames)
}
