ui_filter_info <- function(id) {
  ns <- NS(id)

  div(
    style = "overflow: overlay",
    tableOutput(ns("table"))
  )
}

srv_filter_info <- function(input, output, session, datasets) {
  # have to force arguments
  force(datasets)

  output$table <- renderTable({
    .log("update uifiltersinfo")

    on_filters <- vapply(
      datasets$datanames(),
      function(dataname) {
        length(names(datasets$get_filter_state(dataname, reactive = TRUE))) >= 1
      },
      logical(1)
    )

    already_rendered <- length(which(on_filters)) > 1

    if (already_rendered) {
      NULL
    } else {
      observations <- vapply(
        X = datasets$datanames(),
        FUN = function(dataname, datasets) {
          paste0(
            datasets$get_data_info(dataname, filtered = TRUE, reactive = TRUE)$dim[1], "/",
            datasets$get_data_info(dataname = dataname, filtered = FALSE, reactive = TRUE)$dim[1]
          )
        },
        FUN.VALUE = character(1),
        dataset = datasets
      )
      subjects <- vapply(
        X = datasets$datanames(),
        FUN = function(dataname, datasets) {
          paste0(
            datasets$get_data_info(dataname, filtered = TRUE, reactive = TRUE)$patients, "/",
            datasets$get_data_info(dataname = dataname, filtered = FALSE, reactive = TRUE)$patients
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
    }
  }, width = "100%")
}
