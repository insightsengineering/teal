#' Filter state snapshot management.
#'
#' Capture and restore snapshots of the global (app) filter state.
#'
#' This module introduces snapshots: stored descriptions of the filter state of the entire application.
#' Snapshots allow the user to save the current filter state of the application for later use in the session,
#' as well as to save it to file in order to share it with an app developer or other users.
#'
#' The snapshot manager is accessed through the filter manager, with the cog icon in the top right corner.
#' At the beginning of a session it presents two icons: a camera and an circular arrow.
#' Clicking the camera captures a snapshot and clicking the arrow resets initial application state.
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
#' when passed to the `mapping` argument of [`teal::teal_slices`], would result in the current mapping.
#' This is substituted as the snapshot's `mapping` attribute and the snapshot is added to the snapshot list.
#'
#' To restore app state, a snapshot is retrieved from storage and rebuilt into a `teal_slices` object.
#' Then state of all `FilteredData` objects (provided in `filtered_data_list`) is cleared
#' and set anew according to the `mapping` attribute of the snapshot.
#' The snapshot is then set as the current content of `slices_global`.
#'
#' To save a snapshot, the snapshot is retrieved and reassembled just like for restoring,
#' and then saved to file with [`teal.slice::slices_store`].
#'
#' @param id (`character(1)`) `shiny` module id
#' @param slices_global (`reactiveVal`) that contains a `teal_slices` object
#'                      containing all `teal_slice`s existing in the app, both active and inactive
#' @param mapping_matrix (`reactive`) that contains a `data.frame` representation
#'                       of the mapping of filter state ids (rows) to modules labels (columns);
#'                       all columns are `logical` vectors
#' @param filtered_data_list non-nested (`named list`) that contains `FilteredData` objects
#'
#' @return Nothing is returned.
#'
#' @name snapshot_manager_module
#' @aliases snapshot snapshot_manager
#'
#' @author Aleksander Chlebowski
#'
#' @rdname snapshot_manager_module
#' @keywords internal
#'
snapshot_manager_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "snapshot_manager_content",
    div(
      class = "snapshot_table_row",
      span(tags$b("Snapshot manager")),
      actionLink(ns("snapshot_add"), label = NULL, icon = icon("camera"), title = "add snapshot"),
      actionLink(ns("snapshot_reset"), label = NULL, icon = icon("undo"), title = "reset initial state"),
      NULL
    ),
    uiOutput(ns("snapshot_list"))
  )
}

#' @rdname snapshot_manager_module
#' @keywords internal
#'
snapshot_manager_srv <- function(id, slices_global, mapping_matrix, filtered_data_list) {
  checkmate::assert_character(id)
  checkmate::assert_true(is.reactive(slices_global))
  checkmate::assert_class(isolate(slices_global()), "teal_slices")
  checkmate::assert_true(is.reactive(mapping_matrix))
  checkmate::assert_data_frame(isolate(mapping_matrix()), null.ok = TRUE)
  checkmate::assert_list(filtered_data_list, types = "FilteredData", any.missing = FALSE, names = "named")

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Store global filter states.
    filter <- isolate(slices_global())
    snapshot_history <- reactiveVal({
      list(
        "Initial application state" = as.list(filter, recursive = TRUE)
      )
    })

    # Snapshot current application state - name snaphsot.
    observeEvent(input$snapshot_add, {
      showModal(
        modalDialog(
          textInput(ns("snapshot_name"), "Name the snapshot", width = "100%", placeholder = "Meaningful, unique name"),
          footer = tagList(
            actionButton(ns("snapshot_name_accept"), "Accept", icon = icon("thumbs-up")),
            modalButton(label = "Cancel", icon = icon("thumbs-down"))
          ),
          size = "s"
        )
      )
    })
    # Snapshot current application state - store snaphsot.
    observeEvent(input$snapshot_name_accept, {
      snapshot_name <- trimws(input$snapshot_name)
      if (identical(snapshot_name, "")) {
        showNotification(
          "Please name the snapshot.",
          type = "message"
        )
        updateTextInput(inputId = "snapshot_name", value = "", placeholder = "Meaningful, unique name")
      } else if (is.element(make.names(snapshot_name), make.names(names(snapshot_history())))) {
        showNotification(
          "This name is in conflict with other snapshot names. Please choose a different one.",
          type = "message"
        )
        updateTextInput(inputId = "snapshot_name", value = , placeholder = "Meaningful, unique name")
      } else {
        snapshot <- as.list(slices_global(), recursive = TRUE)
        attr(snapshot, "mapping") <- matrix_to_mapping(mapping_matrix())
        snapshot_update <- c(snapshot_history(), list(snapshot))
        names(snapshot_update)[length(snapshot_update)] <- snapshot_name
        snapshot_history(snapshot_update)
        removeModal()
        # Reopen filter manager modal by clicking button in the main application.
        shinyjs::click(id = "teal-main_ui-filter_manager-show", asis = TRUE)
      }
    })

    # Restore initial state.
    observeEvent(input$snapshot_reset, {
      s <- "Initial application state"
      ### Begin restore procedure. ###
      snapshot <- snapshot_history()[[s]]
      snapshot_state <- as.teal_slices(snapshot)
      mapping_unfolded <- unfold_mapping(attr(snapshot_state, "mapping"), names(filtered_data_list))
      mapply(
        function(filtered_data, filters) {
          filtered_data$clear_filter_states(force = TRUE)
          slices <- Filter(function(x) x$id %in% filters, snapshot_state)
          filtered_data$set_filter_state(slices)
        },
        filtered_data = filtered_data_list,
        filters = mapping_unfolded
      )
      slices_global(snapshot_state)
      removeModal()
      ### End restore procedure. ###
    })

    # Create UI elements and server logic for the snapshot table.
    # Observers must be tracked to avoid duplication and excess reactivity.
    # Remaining elements are tracked likewise for consistency and a slight speed margin.
    observers <- reactiveValues()
    handlers <- reactiveValues()
    divs <- reactiveValues()

    observeEvent(snapshot_history(), {
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
            mapping_unfolded <- unfold_mapping(attr(snapshot_state, "mapping"), names(filtered_data_list))
            mapply(
              function(filtered_data, filters) {
                filtered_data$clear_filter_states(force = TRUE)
                slices <- Filter(function(x) x$id %in% filters, snapshot_state)
                filtered_data$set_filter_state(slices)
              },
              filtered_data = filtered_data_list,
              filters = mapping_unfolded
            )
            slices_global(snapshot_state)
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
              teal.slice::slices_store(tss = snapshot_state, file = file)
            }
          )
          handlers[[id_saveme]] <- id_saveme
        }
        # Create a row for the snapshot table.
        if (!is.element(id_rowme, names(divs))) {
          divs[[id_rowme]] <- div(
            class = "snapshot_table_row",
            span(h5(s)),
            actionLink(inputId = ns(id_pickme), label = icon("circle-check"), title = "select"),
            downloadLink(outputId = ns(id_saveme), label = icon("save"), title = "save to file")
          )
        }
      })
    })

    # Create table to display list of snapshots and their actions.
    output$snapshot_list <- renderUI({
      lapply(rev(reactiveValuesToList(divs)), function(d) d)
    })
  })
}




### utility functions ----

#' Explicitly enumerate global filters.
#'
#' Transform module mapping such that global filters are explicitly specified for every module.
#'
#' @param mapping (`named list`) as stored in mapping parameter of `teal_slices`
#' @param module_names (`character`) vector containing names of all modules in the app
#' @return A `named_list` with one element per module, each element containing all filters applied to that module.
#' @keywords internal
#'
unfold_mapping <- function(mapping, module_names) {
  module_names <- structure(module_names, names = module_names)
  lapply(module_names, function(x) c(mapping[[x]], mapping[["global_filters"]]))
}

#' Convert mapping matrix to filter mapping specification.
#'
#' Transform a mapping matrix, i.e. a data frame that maps each filter state to each module,
#' to a list specification like the one used in the `mapping` attribute of `teal_slices`.
#' Global filters are gathered in one list element.
#' If a module has no active filters but the global ones, it will not be mentioned in the output.
#'
#' @param mapping_matrix (`data.frame`) of logical vectors where
#'                       columns represent modules and row represent `teal_slice`s
#' @return `named list` like that in the `mapping` attribute of a `teal_slices` object.
#' @keywords internal
#'
matrix_to_mapping <- function(mapping_matrix) {
  global <- vapply(as.data.frame(t(mapping_matrix)), all, logical(1L))
  global_filters <- names(global[global])
  local_filters <- mapping_matrix[!rownames(mapping_matrix) %in% global_filters, ]

  mapping <- c(lapply(local_filters, function(x) rownames(local_filters)[x]), list(global_filters = global_filters))
  Filter(function(x) length(x) != 0L, mapping)
}
