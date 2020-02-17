ui_filter_info <- function(id, dataname) {
  ns <- NS(id)

  div(
    class = paste0("teal_filter_", dataname),
    style = "overflow: overlay",
    tableOutput(ns("uifiltersinfo"))
  )
}

srv_filter_info <- function(input, output, session, datasets, dataname) {

  output$uifiltersinfo <- renderTable({
    .log("update uifiltersinfo")

    on_filters <- unlist(lapply(
      datasets$datanames(),
      function(dataname_int) {
        length(names(datasets$get_filter_state(dataname_int, reactive = TRUE))) >= 1
      }
    ))

    on_filters <- setNames(on_filters, datasets$datanames())

    already_rendered <- which(dataname == datasets$datanames()) > 1 && length(which(on_filters)) > 1

    if (!on_filters[dataname] || already_rendered) {
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
