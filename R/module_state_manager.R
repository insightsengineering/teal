#' App state management.
#'
#' Capture and restore the global (app) input state.
#'
#' This is a work in progress.
#'
#' @param id (`character(1)`) `shiny` module id
#'
#' @return Nothing is returned.
#'
#' @name state_manager_module
#' @aliases grab grab_manager state_manager
#'
#' @author Aleksander Chlebowski
#'
#' @rdname state_manager_module
#' @keywords internal
#'
state_manager_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "snapshot_manager_content",
    div(
      class = "snapshot_table_row",
      span(tags$b("State manager")),
      actionLink(ns("grab_add"), NULL, icon = suppressMessages(icon("solid fa-bookmark")), title = "grab input state"),
      NULL
    ),
    uiOutput(ns("grab_list"))
  )
}

#' @rdname state_manager_module
#' @keywords internal
#'
state_manager_srv <- function(id, slices_global, mapping_matrix, filtered_data_list, snapshot_history) {
  checkmate::assert_character(id)
  checkmate::assert_true(is.reactive(slices_global))
  checkmate::assert_class(isolate(slices_global()), "teal_slices")
  checkmate::assert_true(is.reactive(mapping_matrix))
  checkmate::assert_data_frame(isolate(mapping_matrix()), null.ok = TRUE)
  checkmate::assert_list(filtered_data_list, types = "FilteredData", any.missing = FALSE, names = "named")
  checkmate::assert_true(is.reactive(snapshot_history))
  checkmate::assert_list(isolate(snapshot_history()), names = "unique")

  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    app_session <- .subset2(shiny::getDefaultReactiveDomain(), "parent")

    # Store input states.
    grab_history <- reactiveVal({
      list()
    })

    app_session$onBookmark(function(state) {
      # Add current filter state to bookmark.
      snapshot <- as.list(slices_global(), recursive = TRUE)
      attr(snapshot, "mapping") <- matrix_to_mapping(mapping_matrix())
      state$values$filter_state_on_bookmark <- snapshot
      # Add snapshot history and grab history to bookmark.
      state$values$snapshot_history <- snapshot_history()   # isolate this?
      state$values$grab_history <- grab_history()           # isolate this?
    })
    app_session$onRestored(function(state) {
      # Restore filter state.
      snapshot <- state$values$filter_state_on_bookmark
      snapshot_state <- as.teal_slices(snapshot)
      mapping_unfolded <- unfold_mapping(attr(snapshot_state, "mapping"), names(filtered_data_list))
      mapply(
        function(filtered_data, filter_ids) {
          filtered_data$clear_filter_states(force = TRUE)
          slices <- Filter(function(x) x$id %in% filter_ids, snapshot_state)
          filtered_data$set_filter_state(slices)
        },
        filtered_data = filtered_data_list,
        filter_ids = mapping_unfolded
      )
      slices_global(snapshot_state)
      # Restore snapshot history and grab history.
      snapshot_history(state$values$snapshot_history)
      grab_history(state$values$grab_history)
    })

    app_session$onBookmarked(function(url) {
      grab_name <- trimws(input$grab_name)
      if (identical(grab_name, "")) {
        showNotification(
          "Please name the grab.",
          type = "message"
        )
        updateTextInput(inputId = "grab_name", value = "", placeholder = "Meaningful, unique name")
        unlink(strsplit(url, "_state_id_=")[[1L]][[2L]], recursive = TRUE, force = TRUE, expand = FALSE)
      } else if (is.element(make.names(grab_name), make.names(names(grab_history())))) {
        showNotification(
          "This name is in conflict with other grab names. Please choose a different one.",
          type = "message"
        )
        updateTextInput(inputId = "grab_name", value = , placeholder = "Meaningful, unique name")
        unlink(strsplit(url, "_state_id_=")[[1L]][[2L]], recursive = TRUE, force = TRUE, expand = FALSE)
      } else {
        # Add bookmark URL to grab history (with name).
        grab_update <- c(grab_history(), list(url))
        names(grab_update)[length(grab_update)] <- grab_name
        grab_history(grab_update)

        removeModal()
      }
    })

    # Grab current input state - name grab.
    observeEvent(input$grab_add, {
      showModal(
        modalDialog(
          textInput(ns("grab_name"), "Name the grab", width = "100%", placeholder = "Meaningful, unique name"),
          footer = tagList(
            bookmarkButton("Accept", icon = icon("thumbs-up")),
            modalButton(label = "Cancel", icon = icon("thumbs-down"))
          ),
          size = "s"
        )
      )
    })

    # Create UI elements and server logic for the grab table.
    # Divs are tracked for a slight speed margin.
    divs <- reactiveValues()

    observeEvent(grab_history(), {
      lapply(names(grab_history()), function(s) {
        id_rowme <- sprintf("rowme_%s", make.names(s))

        # Create a row for the grab table.
        if (!is.element(id_rowme, names(divs))) {
          divs[[id_rowme]] <- div(
            class = "snapshot_table_row",
            a(h5(s), title = "restore bookmark", href = grab_history()[[s]], target = "blank")
          )
        }
      })
    })

    # Create table to display list of grabs and their actions.
    output$grab_list <- renderUI({
      rows <- lapply(rev(reactiveValuesToList(divs)), function(d) d)
      if (length(rows) == 0L) {
        div(
          class = "snapshot_manager_placeholder",
          "Input states will appear here."
        )
      } else {
        rows
      }
    })

    grab_history
  })
}
