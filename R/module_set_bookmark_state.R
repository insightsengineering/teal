srv_set_bookmark_state <- function(id, datasets, filter) {
  moduleServer(id, function(input, output, server) {
    # Shiny bookmarking ----
    # The Shiny bookmarking functionality by default only stores inputs.
    # We need to add `FilteredData` object to the state so we restore it as well.
    # To test bookmarking, include the `bookmark_module`, click on the bookmark
    # button and then get the link. Keep the Shiny app running and open the
    # obtained link in another browser tab.
    onBookmark(function(state) {
      # this function is isolated  by Shiny
      # We store the entire R6 class with reactive values in it, but set the data to NULL.
      # Note that we cannnot directly do this on datasets as this would trigger
      # reactivity to recompute the filtered datasets, which is not needed.
      logger::log_trace(
        paste(
          "srv_teal@2 saving active filter state for",
          "datasets: { paste(names(datasets$get_bookmark_state()), collapse = ' ') }."
        )
      )
      state$values$datasets_state <- datasets$get_bookmark_state()
    })
    saved_datasets_state <- reactiveVal(NULL) # set when restored because data must already be populated
    onRestore(function(state) {
      # The saved datasets mainly contains the filter states as the data
      # was set to NULL before storing. The data should have been set again
      # by the user, so we just need to set the filters.
      logger::log_trace(
        paste(
          "srv_teal@2 restoring filter states from the bookmark for",
          "datasets: { paste(names(state$values$datasets_state), collapse = ' ') }."
        )
      )
      saved_datasets_state(state$values$datasets_state)
    })

    # initialize datasets ------

    if (!is.null(saved_datasets_state())) {
      # actual thing to restore
      # cannot call this directly in onRestore because the data is not set at that time
      # for example, the data may only be loaded once a password is provided
      # however, onRestore only runs in the first flush and not in the flush when the
      # password was finally provided
      tryCatch({
        progress$set(0.75, message = "Restoring from bookmarked state")
        filtered_data_set_filters(datasets, saved_datasets_state())

      },
      error = function(cnd) {
        logger::log_error("Attempt to set bookmark state failed.")
        showModal(
          modalDialog(
            div(
              p("Could not restore the session: "),
              tags$pre(id = session$ns("error_msg"), cnd$message)
            ),
            title = "Error restoring the bookmarked state",
            footer = tagList(
              actionButton(
                "copy_code", "Copy to Clipboard",
                `data-clipboard-target` = paste0("#", session$ns("error_msg"))
              ),
              modalButton("Dismiss")
            ),
            size = "l",
            easyClose = TRUE
          )
        )
      }
      )
    } else {
      progress$set(0.75, message = "Setting initial filter state")
      logger::log_trace("srv_teal@4 setting the initial filter state.")
      filtered_data_set_filters(datasets, filter)
    }
  })
}
