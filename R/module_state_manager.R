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
#' @seealso [`app_state_grab`], [`app_state_store`], [`app_state_restore`]
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
      actionLink(ns("grab_add"), label = NULL, icon = icon("camera"), title = "grab input state"),
      NULL
    ),
    uiOutput(ns("grab_list"))
  )
}

#' @rdname state_manager_module
#' @keywords internal
#'
state_manager_srv <- function(id) {
  checkmate::assert_character(id)

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Store initial input states.
    grab_history <- reactiveVal({
      list(
        "Initial input state" = grab_state(get_master_session())
      )
    })

    # Grab current input state - name grab.
    observeEvent(input$grab_add, {
      showModal(
        modalDialog(
          textInput(ns("grab_name"), "Name the grab", width = "100%", placeholder = "Meaningful, unique name"),
          footer = tagList(
            actionButton(ns("grab_name_accept"), "Accept", icon = icon("thumbs-up")),
            modalButton(label = "Cancel", icon = icon("thumbs-down"))
          ),
          size = "s"
        )
      )
    })
    # Grab current input state - store grab.
    observeEvent(input$grab_name_accept, {
      grab_name <- trimws(input$grab_name)
      if (identical(grab_name, "")) {
        showNotification(
          "Please name the grab.",
          type = "message"
        )
        updateTextInput(inputId = "grab_name", value = "", placeholder = "Meaningful, unique name")
      } else if (is.element(make.names(grab_name), make.names(names(grab_history())))) {
        showNotification(
          "This name is in conflict with other grab names. Please choose a different one.",
          type = "message"
        )
        updateTextInput(inputId = "grab_name", value = , placeholder = "Meaningful, unique name")
      } else {
        sesh <- get_master_session()
        # 1. get input names and isolate filter panel
        filter_panel_inputs <- grep("filter_panel", names(sesh$input), value = TRUE)
        # 2. exclude filter panel from bookmark
        sesh$setBookmarkExclude(character(0L))
        sesh$setBookmarkExclude(filter_panel_inputs)
        # 3. arrange restoring filter state after restoring bookmark
        ### work in progress
        sesh$onBookmark(function(state) {
          ### smth like this should happen:
          snapshot <- as.list(slices_global(), recursive = TRUE)
          attr(snapshot, "mapping") <- matrix_to_mapping(mapping_matrix())
          state$filter_state_on_bookmark <- snapshot
          ### end; requires access to slices_global and mapping_matrix
          state$snapshot_history <- snapshot_history()   # isolate this?
          state$grab_history <- grab_history()           # isolate this?
        })
        sesh$onRestored(function(state) {
          ### smth like this should happen:
          snapshot <- state$filter_state_on_bookmark
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
          ### end; requires access to slices_global and filtered_data_list
          snapshot_history(state$snapshot_history)
          grab_history(state$grab_history)
        })
        # 4. do bookmark
        url <- grab_state(sesh)
        # 5. add bookmark URL to grab history (with name)
        grab_update <- c(grab_history(), list(url))
        names(grab_update)[length(grab_update)] <- grab_name
        grab_history(grab_update)
        # 6. remove modal
        removeModal()
        # Reopen filter manager modal by clicking button in the main application.
        shinyjs::click(id = "teal-main_ui-filter_manager-show", asis = TRUE)
      }
    })

    # Create UI elements and server logic for the grab table.
    # Divs are tracked for a slight speed margin.
    divs <- reactiveValues()

    observeEvent(grab_history(), {
      lapply(names(grab_history())[-1L], function(s) {
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
  })
}



# utility functions ----

#' @keywords internal
#'
get_master_session <- function() {
  local_session <- shiny::getDefaultReactiveDomain()
  app_session <- .subset2(local_session, "parent")
  if (is.null(app_session)) {
    local_session
  } else {
    app_session
  }
}


# add bookmark and return URL to saved state
# simplified from session$doBookmark
#' @keywords internal
#'
grab_state <- function(session) {
  if (getShinyOption("bookmarkStore", default = "disable") != "server") {
    showNotification("Bookmarks have not been enabled for this application.")
    return(invisible(NULL))
  }
  tryCatch(shiny:::withLogErrors({
    saveState <- shiny:::ShinySaveState$new(
      input = session$.__enclos_env__$self$input,
      exclude = session$.__enclos_env__$self$getBookmarkExclude(),
      onSave = function(state) {
        session$.__enclos_env__$private$bookmarkCallbacks$invoke(state)
      })
    url <- shiny:::saveShinySaveState(saveState)
    clientData <- session$.__enclos_env__$self$clientData
    url <- paste0(
      clientData$url_protocol,
      "//",
      clientData$url_hostname,
      if (nzchar(clientData$url_port)) paste0(":", clientData$url_port),
      clientData$url_pathname,
      "?",
      url
    )
  }), error = function(e) {
    msg <- paste0("Error bookmarking state: ", e$message)
    shiny::showNotification(msg, duration = NULL, type = "error")
  })

  url
}
