#' Filter state snapshot management
#'
#' Capture and restore snapshots of the global (app) filter state.
#'
#' This module introduces snapshots: stored descriptions of the filter state of the entire application.
#' Snapshots allow the user to save the current filter state of the application for later use in the session,
#' as well as to save it to file in order to share it with an app developer or other users,
#' who in turn can upload it to their own session.
#'
#' The snapshot manager is accessed with the camera icon in the tabset bar.
#' At the beginning of a session it presents three icons: a camera, an upload, and an circular arrow.
#' Clicking the camera captures a snapshot, clicking the upload adds a snapshot from a file
#' and applies the filter states therein, and clicking the arrow resets initial application state.
#' As snapshots are added, they will show up as rows in a table and each will have a select button and a save button.
#'
#' @section Server logic:
#' Snapshots are basically `teal_slices` objects, however, since each module is served by a separate instance
#' of `FilteredData` and these objects require shared state, `teal_slice` is a `reactiveVal` so `teal_slices`
#' cannot be stored as is. Therefore, `teal_slices` are reversibly converted to a list of lists representation
#' (attributes are maintained).
#'
#' Snapshots are stored in a `reactiveVal` as a named list.
#' The first snapshot is the initial state of the application and the user can add a snapshot whenever they see fit.
#'
#' For every snapshot except the initial one, a piece of UI is generated that contains
#' the snapshot name, a select button to restore that snapshot, and a save button to save it to a file.
#' The initial snapshot is restored by a separate "reset" button.
#' It cannot be saved directly but a user is welcome to capture the initial state as a snapshot and save that.
#'
#' @section Snapshot mechanics:
#' When a snapshot is captured, the user is prompted to name it.
#' Names are displayed as is but since they are used to create button ids,
#' under the hood they are converted to syntactically valid strings.
#' New snapshot names are validated so that their valid versions are unique.
#' Leading and trailing white space is trimmed.
#'
#' The module can read the global state of the application from `slices_global` and `mapping_matrix`.
#' The former provides a list of all existing `teal_slice`s and the latter says which slice is active in which module.
#' Once a name has been accepted, `slices_global` is converted to a list of lists - a snapshot.
#' The snapshot contains the `mapping` attribute of the initial application state
#' (or one that has been restored), which may not reflect the current one,
#' so `mapping_matrix` is transformed to obtain the current mapping, i.e. a list that,
#' when passed to the `mapping` argument of [teal_slices()], would result in the current mapping.
#' This is substituted as the snapshot's `mapping` attribute and the snapshot is added to the snapshot list.
#'
#' To restore app state, a snapshot is retrieved from storage and rebuilt into a `teal_slices` object.
#' Then state of all `FilteredData` objects (provided in `datasets`) is cleared
#' and set anew according to the `mapping` attribute of the snapshot.
#' The snapshot is then set as the current content of `slices_global`.
#'
#' To save a snapshot, the snapshot is retrieved and reassembled just like for restoring,
#' and then saved to file with [slices_store()].
#'
#' When a snapshot is uploaded, it will first be added to storage just like a newly created one,
#' and then used to restore app state much like a snapshot taken from storage.
#' Upon clicking the upload icon the user will be prompted for a file to upload
#' and may choose to name the new snapshot. The name defaults to the name of the file (the extension is dropped)
#' and normal naming rules apply. Loading the file yields a `teal_slices` object,
#' which is disassembled for storage and used directly for restoring app state.
#'
#' @section Transferring snapshots:
#' Snapshots uploaded from disk should only be used in the same application they come from,
#' _i.e._ an application that uses the same data and the same modules.
#' To ensure this is the case, `init` stamps `teal_slices` with an app id that is stored in the `app_id` attribute of
#' a `teal_slices` object. When a snapshot is restored from file, its `app_id` is compared to that
#' of the current app state and only if the match is the snapshot admitted to the session.
#'
#' @section Bookmarks:
#' An `onBookmark` callback creates a snapshot of the current filter state.
#' This is done on the app session, not the module session.
#' (The snapshot will be retrieved by `module_teal` in order to set initial app state in a restored app.)
#' Then that snapshot, and the previous snapshot history are dumped into the `values.rds` file in `<bookmark_dir>`.
#'
#' @param id (`character(1)`) `shiny` module instance id.
#' @param slices_global (`reactiveVal`) that contains a `teal_slices` object
#'                      containing all `teal_slice`s existing in the app, both active and inactive.
#'
#' @return `list` containing the snapshot history, where each element is an unlisted `teal_slices` object.
#'
#' @name module_snapshot_manager
#' @rdname module_snapshot_manager
#'
#' @author Aleksander Chlebowski
#' @keywords internal
NULL

#' @rdname module_snapshot_manager
ui_snapshot_manager_panel <- function(id) {
  ns <- NS(id)
  tags$button(
    id = ns("show_snapshot_manager"),
    class = "btn action-button wunder_bar_button",
    title = "View filter mapping",
    suppressMessages(icon("fas fa-camera"))
  )
}

#' @rdname module_snapshot_manager
srv_snapshot_manager_panel <- function(id, slices_global) {
  moduleServer(id, function(input, output, session) {
    logger::log_debug("srv_snapshot_manager_panel initializing")
    setBookmarkExclude(c("show_snapshot_manager"))
    observeEvent(input$show_snapshot_manager, {
      logger::log_debug("srv_snapshot_manager_panel@1 show_snapshot_manager button has been clicked.")
      showModal(
        modalDialog(
          ui_snapshot_manager(session$ns("module")),
          class = "snapshot_manager_modal",
          size = "m",
          footer = NULL,
          easyClose = TRUE
        )
      )
    })
    srv_snapshot_manager("module", slices_global = slices_global)
  })
}

#' @rdname module_snapshot_manager
ui_snapshot_manager <- function(id) {
  ns <- NS(id)
  tags$div(
    class = "manager_content",
    tags$div(
      class = "manager_table_row",
      tags$span(tags$b("Snapshot manager")),
      actionLink(ns("snapshot_add"), label = NULL, icon = icon("fas fa-camera"), title = "add snapshot"),
      actionLink(ns("snapshot_load"), label = NULL, icon = icon("fas fa-upload"), title = "upload snapshot"),
      actionLink(ns("snapshot_reset"), label = NULL, icon = icon("fas fa-undo"), title = "reset initial state"),
      NULL
    ),
    uiOutput(ns("snapshot_list"))
  )
}

#' @rdname module_snapshot_manager
srv_snapshot_manager <- function(id, slices_global) {
  checkmate::assert_character(id)

  moduleServer(id, function(input, output, session) {
    logger::log_debug("srv_snapshot_manager initializing")

    # Set up bookmarking callbacks ----
    # Register bookmark exclusions (all buttons and text fields).
    setBookmarkExclude(c(
      "snapshot_add", "snapshot_load", "snapshot_reset",
      "snapshot_name_accept", "snaphot_file_accept",
      "snapshot_name", "snapshot_file"
    ))
    # Add snapshot history to bookmark.
    session$onBookmark(function(state) {
      logger::log_debug("srv_snapshot_manager@onBookmark: storing snapshot and bookmark history")
      state$values$snapshot_history <- snapshot_history() # isolate this?
    })

    ns <- session$ns

    # Track global filter states ----
    snapshot_history <- reactiveVal({
      # Restore directly from bookmarked state, if applicable.
      restoreValue(
        ns("snapshot_history"),
        list("Initial application state" = shiny::isolate(as.list(slices_global$all_slices(), recursive = TRUE)))
      )
    })

    # Snapshot current application state ----
    # Name snaphsot.
    observeEvent(input$snapshot_add, {
      logger::log_debug("srv_snapshot_manager: snapshot_add button clicked")
      showModal(
        modalDialog(
          textInput(ns("snapshot_name"), "Name the snapshot", width = "100%", placeholder = "Meaningful, unique name"),
          footer = tagList(
            actionButton(ns("snapshot_name_accept"), "Accept", icon = icon("far fa-thumbs-up")),
            modalButton(label = "Cancel", icon = icon("far fa-thumbs-down"))
          ),
          size = "s"
        )
      )
    })
    # Store snaphsot.
    observeEvent(input$snapshot_name_accept, {
      logger::log_debug("srv_snapshot_manager: snapshot_name_accept button clicked")
      snapshot_name <- trimws(input$snapshot_name)
      if (identical(snapshot_name, "")) {
        logger::log_debug("srv_snapshot_manager: snapshot name rejected")
        showNotification(
          "Please name the snapshot.",
          type = "message"
        )
        updateTextInput(inputId = "snapshot_name", value = "", placeholder = "Meaningful, unique name")
      } else if (is.element(make.names(snapshot_name), make.names(names(snapshot_history())))) {
        logger::log_debug("srv_snapshot_manager: snapshot name rejected")
        showNotification(
          "This name is in conflict with other snapshot names. Please choose a different one.",
          type = "message"
        )
        updateTextInput(inputId = "snapshot_name", value = "", placeholder = "Meaningful, unique name")
      } else {
        logger::log_debug("srv_snapshot_manager: snapshot name accepted, adding snapshot")
        snapshot <- as.list(slices_global$all_slices(), recursive = TRUE)
        snapshot_update <- c(snapshot_history(), list(snapshot))
        names(snapshot_update)[length(snapshot_update)] <- snapshot_name
        snapshot_history(snapshot_update)
        removeModal()
        # Reopen filter manager modal by clicking button in the main application.
        shinyjs::click(id = "teal-wunder_bar-show_snapshot_manager", asis = TRUE)
      }
    })

    # Upload a snapshot file ----
    # Select file.
    observeEvent(input$snapshot_load, {
      logger::log_debug("srv_snapshot_manager: snapshot_load button clicked")
      showModal(
        modalDialog(
          fileInput(ns("snapshot_file"), "Choose snapshot file", accept = ".json", width = "100%"),
          textInput(
            ns("snapshot_name"),
            "Name the snapshot (optional)",
            width = "100%",
            placeholder = "Meaningful, unique name"
          ),
          footer = tagList(
            actionButton(ns("snaphot_file_accept"), "Accept", icon = icon("far fa-thumbs-up")),
            modalButton(label = "Cancel", icon = icon("far fa-thumbs-down"))
          )
        )
      )
    })
    # Store new snapshot to list and restore filter states.
    observeEvent(input$snaphot_file_accept, {
      logger::log_debug("srv_snapshot_manager: snapshot_file_accept button clicked")
      snapshot_name <- trimws(input$snapshot_name)
      if (identical(snapshot_name, "")) {
        logger::log_debug("srv_snapshot_manager: no snapshot name provided, naming after file")
        snapshot_name <- tools::file_path_sans_ext(input$snapshot_file$name)
      }
      if (is.element(make.names(snapshot_name), make.names(names(snapshot_history())))) {
        logger::log_debug("srv_snapshot_manager: snapshot name rejected")
        showNotification(
          "This name is in conflict with other snapshot names. Please choose a different one.",
          type = "message"
        )
        updateTextInput(inputId = "snapshot_name", value = "", placeholder = "Meaningful, unique name")
      } else {
        # Restore snapshot and verify app compatibility.
        logger::log_debug("srv_snapshot_manager: snapshot name accepted, loading snapshot")
        snapshot_state <- try(slices_restore(input$snapshot_file$datapath))
        if (!inherits(snapshot_state, "modules_teal_slices")) {
          logger::log_debug("srv_snapshot_manager: snapshot file corrupt")
          showNotification(
            "File appears to be corrupt.",
            type = "error"
          )
        } else if (!identical(attr(snapshot_state, "app_id"), attr(slices_global$all_slices(), "app_id"))) {
          logger::log_debug("srv_snapshot_manager: snapshot not compatible with app")
          showNotification(
            "This snapshot file is not compatible with the app and cannot be loaded.",
            type = "warning"
          )
        } else {
          # Add to snapshot history.
          logger::log_debug("srv_snapshot_manager: snapshot loaded, adding to history")
          snapshot <- as.list(slices_global$all_slices(), recursive = TRUE)
          snapshot_update <- c(snapshot_history(), list(snapshot))
          names(snapshot_update)[length(snapshot_update)] <- snapshot_name
          snapshot_history(snapshot_update)
          ### Begin simplified restore procedure. ###
          logger::log_debug("srv_snapshot_manager: restoring snapshot")
          slices_global$slices_set(snapshot_state)
          removeModal()
          ### End  simplified restore procedure. ###
        }
      }
    })
    # Apply newly added snapshot.

    # Restore initial state ----
    observeEvent(input$snapshot_reset, {
      logger::log_debug("srv_snapshot_manager: snapshot_reset button clicked, restoring snapshot")
      s <- "Initial application state"
      ### Begin restore procedure. ###
      snapshot <- snapshot_history()[[s]]
      snapshot_state <- as.teal_slices(snapshot)
      slices_global$slices_set(snapshot_state)
      removeModal()
      ### End restore procedure. ###
    })

    # Build snapshot table ----
    # Create UI elements and server logic for the snapshot table.
    # Observers must be tracked to avoid duplication and excess reactivity.
    # Remaining elements are tracked likewise for consistency and a slight speed margin.
    observers <- reactiveValues()
    handlers <- reactiveValues()
    divs <- reactiveValues()

    observeEvent(snapshot_history(), {
      logger::log_debug("srv_snapshot_manager: snapshot history modified, updating snapshot list")
      lapply(names(snapshot_history())[-1L], function(s) {
        id_pickme <- sprintf("pickme_%s", make.names(s))
        id_saveme <- sprintf("saveme_%s", make.names(s))
        id_rowme <- sprintf("rowme_%s", make.names(s))

        # Observer for restoring snapshot.
        if (!is.element(id_pickme, names(observers))) {
          observers[[id_pickme]] <- observeEvent(input[[id_pickme]], {
            ### Begin restore procedure. ###
            snapshot <- snapshot_history()[[s]]
            snapshot_state <- as.teal_slices(snapshot)

            slices_global$slices_set(snapshot_state)
            removeModal()
            ### End restore procedure. ###
          })
        }
        # Create handler for downloading snapshot.
        if (!is.element(id_saveme, names(handlers))) {
          output[[id_saveme]] <- downloadHandler(
            filename = function() {
              sprintf("teal_snapshot_%s_%s.json", s, Sys.Date())
            },
            content = function(file) {
              snapshot <- snapshot_history()[[s]]
              snapshot_state <- as.teal_slices(snapshot)
              slices_store(tss = snapshot_state, file = file)
            }
          )
          handlers[[id_saveme]] <- id_saveme
        }
        # Create a row for the snapshot table.
        if (!is.element(id_rowme, names(divs))) {
          divs[[id_rowme]] <- tags$div(
            class = "manager_table_row",
            tags$span(tags$h5(s)),
            actionLink(inputId = ns(id_pickme), label = icon("far fa-circle-check"), title = "select"),
            downloadLink(outputId = ns(id_saveme), label = icon("far fa-save"), title = "save to file")
          )
        }
      })
    })

    # Create table to display list of snapshots and their actions.
    output$snapshot_list <- renderUI({
      rows <- rev(reactiveValuesToList(divs))
      if (length(rows) == 0L) {
        tags$div(
          class = "manager_placeholder",
          "Snapshots will appear here."
        )
      } else {
        rows
      }
    })

    snapshot_history
  })
}
