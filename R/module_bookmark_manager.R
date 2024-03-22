#' App state management.
#'
#' Capture and restore the global (app) input state.
#'
#' This module introduces bookmarks into `teal` apps: the `shiny` bookmarking mechanism becomes enabled
#' and server-side bookmarks can be created.
#'
#' The bookmark manager is accessed with the bookmark icon in the [`wunder_bar`].
#' The manager's header contains a title and a bookmark icon. Clicking the icon creates a bookmark.
#' As bookmarks are added, they will show up as rows in a table, each being a link that, when clicked,
#' will open the bookmarked application in a new window.
#'
#' @section Server logic:
#' A bookmark is a URL that contains the app address with a `/?_state_id_=<bookmark_dir>` suffix.
#' `<bookmark_dir>` is a directory created on the server, where the state of the application is saved.
#' Accessing the bookmark URL opens a new session of the app that starts in the previously saved state.
#'
#' Bookmarks are stored in a `reactiveVal` as a named list.
#' For every bookmark created a piece of HTML is created that contains a link,
#' whose text is the name of the bookmark and whose href is the bookmark URL.
#'
#' @section Bookmark mechanics:
#' When a bookmark is added, the user is prompted to name it.
#' New bookmark names are validated so that thy are unique. Leading and trailing white space is trimmed.
#'
#' Once a bookmark name has been accepted, the app state is saved: values of all inputs,
#' which are kept in the `input` slot of the `session` object, are dumped into the `input.rds` file
#' in the `<bookmark_dir>` directory on the server.
#' This is out of the box behavior that permeates the entire app, no adjustments to modules are necessary.
#' An additional `onBookmark` callback creates a snapshot of the current filter state
#' (the module has access to the filter state of the application through `slices_global` and `mapping_matrix`).
#' Then that snapshot, the previous snapshot history (which is passed to this module as argument),
#' and the previous bookmark history are dumped into the `values.rds` file in `<bookmark_dir>`.
#'
#' Finally, an `onBookmarked` callback adds the newly created bookmark to the bookmark history.
#' Notably, this occurs _after_ creating the bookmark is concluded so the bookmark history that was stored
#' does not include the newly added bookmark.
#'
#' When starting the app from a bookmark, `shiny` recognizes that the app is being restored,
#' locates the bookmark directory and loads both `.rds` file.
#' Values stored in `input.rds` are automatically set to their corresponding inputs.
#' The filter state that the app had upon bookmarking, which was saved as a separate snapshot, is restored.
#' This is done in the same manner as in the `snapshot_manager` module and thus requires access to `datasets_flat`,
#' which is passed to this module as argument.
#' Finally, snapshot history and bookmark history are loaded from `values.rds` and set to appropriate `reactiveVal`s.
#'
#' @section Note:
#' All `teal` apps are inherently bookmarkable. Normal `shiny` apps require that `enableBookmarking` be set to "server",
#' either by setting an argument in a `shinyApp` call or by calling a special function. In `teal` bookmarks are enabled
#' by automatically setting an option when the package is loaded.
#'
#' @param id (`character(1)`) `shiny` module instance id.
#' @inheritParams module_snapshot_manager
#' @param snapshot_history (named `list`) of unlisted `teal_slices` objects, as returned by the `snapshot_manager`.
#'
#' @return `reactiveVal` containing a named list of bookmark URLs.
#'
#' @name module_bookmark_manager
#' @aliases bookmark bookmark_manager bookmark_manager_module
#'

#' @rdname module_bookmark_manager
#' @keywords internal
#'
bookmark_manager_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "manager_content",
    div(
      class = "manager_table_row",
      span(tags$b("Bookmark manager")),
      actionLink(ns("bookmark_add"), NULL, icon = suppressMessages(icon("solid fa-bookmark")), title = "add bookmark"),
      NULL
    ),
    uiOutput(ns("bookmark_list"))
  )
}

#' @rdname module_bookmark_manager
#' @keywords internal
#'
bookmark_manager_srv <- function(id, slices_global, mapping_matrix, datasets, snapshot_history) {
  checkmate::assert_character(id)
  checkmate::assert_true(is.reactive(slices_global))
  checkmate::assert_class(isolate(slices_global()), "teal_slices")
  checkmate::assert_true(is.reactive(mapping_matrix))
  checkmate::assert_data_frame(isolate(mapping_matrix()), null.ok = TRUE)
  checkmate::assert_list(datasets, types = "FilteredData", any.missing = FALSE, names = "named")
  checkmate::assert_true(is.reactive(snapshot_history))
  checkmate::assert_list(isolate(snapshot_history()), names = "unique")

  moduleServer(id, function(input, output, session) {
    logger::log_trace("bookmark_manager_srv initializing")

    # Set up bookmarking callbacks.
    app_session <- .subset2(shiny::getDefaultReactiveDomain(), "parent")
    # Register bookmark exclusions: all buttons and the `textInput` for bookmark name.
    # Run in observer so list is updated every time new input item is registered.
    observe({
      inputs <- reactiveValuesToList(app_session$input)
      ids_buttons <- names(Filter(function(x) inherits(x, "shinyActionButtonValue"), inputs))
      id_bookmark_name <- grep("bookmark_name", names(inputs), value = TRUE, fixed = TRUE)
      setBookmarkExclude(union(ids_buttons, id_bookmark_name), session = app_session)
    })
    app_session$onBookmark(function(state) {
      # Add current filter state to bookmark.
      logger::log_trace("bookmark_manager_srv@onBookmark: storing filter state")
      snapshot <- as.list(slices_global(), recursive = TRUE)
      attr(snapshot, "mapping") <- matrix_to_mapping(mapping_matrix())
      state$values$filter_state_on_bookmark <- snapshot
      # Add snapshot history and bookmark history to bookmark.
      logger::log_trace("bookmark_manager_srv@onBookmark: storing snapshot and bookmark history")
      state$values$snapshot_history <- snapshot_history() # isolate this?
      state$values$bookmark_history <- bookmark_history() # isolate this?
    })
    app_session$onBookmarked(function(url) {
      logger::log_trace("bookmark_manager_srv@onBookmarked: bookmark button clicked, registering bookmark")
      bookmark_name <- trimws(input$bookmark_name)
      if (identical(bookmark_name, "")) {
        logger::log_trace("bookmark_manager_srv@onBookmarked: bookmark name rejected")
        showNotification(
          "Please name the bookmark.",
          type = "message"
        )
        updateTextInput(inputId = "bookmark_name", value = "", placeholder = "Meaningful, unique name")
        unlink(strsplit(url, "_state_id_=")[[1L]][[2L]], recursive = TRUE, force = TRUE, expand = FALSE)
      } else if (is.element(make.names(bookmark_name), make.names(names(bookmark_history())))) {
        logger::log_trace("bookmark_manager_srv@onBookmarked: bookmark name rejected")
        showNotification(
          "This name is in conflict with other bookmark names. Please choose a different one.",
          type = "message"
        )
        updateTextInput(inputId = "bookmark_name", value = "", placeholder = "Meaningful, unique name")
        unlink(strsplit(url, "_state_id_=")[[1L]][[2L]], recursive = TRUE, force = TRUE, expand = FALSE)
      } else {
        # Add bookmark URL to bookmark history (with name).
        logger::log_trace("bookmark_manager_srv@onBookmarked: bookmark name accepted, adding to history")
        bookmark_update <- c(bookmark_history(), list(url))
        names(bookmark_update)[length(bookmark_update)] <- bookmark_name
        bookmark_history(bookmark_update)

        removeModal()
      }
    })
    app_session$onRestored(function(state) {
      browser()
      # Restore filter state.
      logger::log_trace("bookmark_manager_srv@onRestored: restoring filter state")
      snapshot <- state$values$filter_state_on_bookmark
      snapshot_state <- as.teal_slices(snapshot)
      mapping_unfolded <- unfold_mapping(attr(snapshot_state, "mapping"), names(datasets))
      mapply(
        function(filtered_data, filter_ids) {
          filtered_data$clear_filter_states(force = TRUE)
          slices <- Filter(function(x) x$id %in% filter_ids, snapshot_state)
          filtered_data$set_filter_state(slices)
        },
        filtered_data = datasets,
        filter_ids = mapping_unfolded
      )
      slices_global(snapshot_state)
      # Restore snapshot history and bookmark history.
      logger::log_trace("bookmark_manager_srv@onRestored: restoring snapshot and bookmark history")
      snapshot_history(state$values$snapshot_history)
      bookmark_history(state$values$bookmark_history)
    })

    ns <- session$ns

    # Store input states.
    bookmark_history <- reactiveVal({
      list()
    })

    # Bookmark current input state - name bookmark.
    observeEvent(input$bookmark_add, {
      logger::log_trace("bookmark_manager_srv: bookmark_add button clicked")
      showModal(
        modalDialog(
          textInput(ns("bookmark_name"), "Name the bookmark", width = "100%", placeholder = "Meaningful, unique name"),
          footer = tagList(
            actionButton(ns("bookmark_accept"), label = "Accept", icon = icon("thumbs-up")),
            modalButton(label = "Cancel", icon = icon("thumbs-down"))
          ),
          size = "s"
        )
      )
    })

    # Initiate bookmarking with normal action button b/c `bookmarkButton` may not work on Windows.
    observeEvent(input$bookmark_accept, {
      app_session$doBookmark()
    })

    # Create UI elements and server logic for the bookmark table.
    # Divs are tracked for a slight speed margin.
    divs <- reactiveValues()

    observeEvent(bookmark_history(), {
      logger::log_trace("bookmark_manager_srv: bookmark history changed, updating bookmark list")
      lapply(names(bookmark_history()), function(s) {
        id_rowme <- sprintf("rowme_%s", make.names(s))

        # Create a row for the bookmark table.
        if (!is.element(id_rowme, names(divs))) {
          divs[[id_rowme]] <- div(
            class = "manager_table_row",
            a(h5(s), title = "go to bookmark", href = bookmark_history()[[s]], target = "blank")
          )
        }
      })
    })

    # Create table to display list of bookmarks and their actions.
    output$bookmark_list <- renderUI({
      rows <- rev(reactiveValuesToList(divs))
      if (length(rows) == 0L) {
        div(
          class = "manager_placeholder",
          "Bookmarks will appear here."
        )
      } else {
        rows
      }
    })

    bookmark_history
  })
}


restoreValue <- function(object_name, default) {
  session <- .subset2(shiny::getDefaultReactiveDomain(), "parent")
  if (isTRUE(session$restoreContext$active)) {
    if (exists(object_name, session$restoreContext$values, inherits = FALSE)) {
      session$restoreContext$values[[object_name]]
    } else {
      default
    }
  } else {
    default
  }
}
