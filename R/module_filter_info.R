ui_filter_info <- function(id, dataname, title = NULL) {
  ns <- NS(id)

  div(
    class = paste0("teal_filter_", dataname),

    uiOutput(ns("uifiltersinfo"))
  )
}

#' @import methods
srv_filter_info <- function(input, output, session, datasets, dataname) {
  uistate <- reactiveValues(filters_shown = character(0))

  observeEvent(datasets$get_filter_state(dataname, reactive = TRUE), {
    fs <- datasets$get_filter_state(dataname)
    current <- uistate$filters_shown

    .log("filter items observer updated for data", dataname)

    if (!identical(names(fs), current)) {
      uistate$filters_shown <- names(fs)
    }
  })

  info_tags <- function(dataname, datasets = NULL) {
    tagList(
      tags$b(paste(dataname, "dataset information:")),
      tags$p(paste(
        "# observations:", datasets$get_data_info(dataname, filtered = TRUE, reactive = TRUE)$dim[1], "/",
        datasets$get_data_info(dataname = dataname, filtered = FALSE, reactive = TRUE)$dim[1]
      ))
    )
  }

  output$uifiltersinfo <- renderUI({
    uistate$filters_shown

    .log("update uiFilters")

    fs_data <- datasets$get_filter_state(dataname)

    # Check if filters are already rendered by checking if the number of filtered datasets > 1 and
    # this is at least the second dataset.
    on_filters <- lapply(
      datasets$datanames(),
      function(dataname_int) !is.null(datasets$get_filter_state(dataname_int, reactive = TRUE))
    ) %>%
      unlist()

    already_rendered <- which(dataname == datasets$datanames()) > 1 && length(which(on_filters)) > 1

    if (is.null(fs_data) || length(fs_data) == 0 || already_rendered) {
      div()
    } else {
      els <- lapply(datasets$datanames(), function(dataname_int) info_tags(dataname_int, datasets = datasets))
      do.call(tagList, els)
    }
  })

  NULL
}
