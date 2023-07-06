
# ### SNAPSHOT MANAGEMENT ###
#
#' 1. snapshots are stored in reactiveVal as a list
#' 2. each snapshot is stored as stripped teal_slices (list of lists of fields)
#' 3. first snapshot is initial state
#' 4. adding snapshot prompts user to give it a name
#'   a. forbid invalid names
#'   b. check against existing names and disallow
#' 5. restoring snapshots
#'   a. present list of existing snapshots to choose from
#'   b. derive name of snapshot from selection and select
#' 6. button to save snapshotv
#' 7. button to save all snapshots(?) - nest JSON? save archive?
#' 8. button to load snapshot(s) - nested JSON would be the best probably
#'
#' TODO: saved snapshot must also contain the mapping matrix
#
# ### END SNAPSHOT MANAGMENT ###


snapshot_manager_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "snapshot_manager_content",
    div(
      class = "snapshot_table_row",
      span(tags$b("Snapshot manager")),
      actionLink(ns("snapshot_add"), label = NULL, icon = icon("camera"), title = "add snapshot"),
      actionLink(ns("snapshot_reset"), label = NULL, icon = icon("undo"), title = "reset initial state"),
      # actionLink(ns("snapshots_save"), label = NULL, icon = icon("save"), title = "save all snapshots to file"),
      # actionLink(ns("snapshots_load"), label = NULL, icon = icon("rotate"), title = "load snapshots from file"),
      NULL
    ),
    uiOutput(ns("snapshot_list"))
  )
}

snapshot_manager_srv <- function(id, slices_global, mapping_matrix, filtered_data_list) {
  checkmate::assert_character(id)
  checkmate::assert_true(is.reactive(slices_global))
  checkmate::assert_class(slices_global(), "teal_slices")
  checkmate::assert_true(is.reactive(mapping_matrix))
  checkmate::assert_data_frame(mapping_matrix())
  checkmate::assert_list(filtered_data_list, types = "FilteredData", any.missing = FALSE, names = "named")

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # Store global filter states on demand.
    # This reactiveVal contains a snapshot of the initial state of the application
    #   as well as any state of the user's choosing.
    # Initiate with initial state
    filter <- isolate(slices_global())
    snapshot_history <- reactiveVal({
      list(
        "Initial application state" = strip_slices(filter)
      )
    })

    # Snapshot current application state.
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
        snapshot <- strip_slices(slices_global())
        attr(snapshot, "mapping") <- matrix_to_mapping(mapping_matrix())
        snapshot_update <- c(snapshot_history(), list(snapshot))
        names(snapshot_update)[length(snapshot_update)] <- snapshot_name
        snapshot_history(snapshot_update)
        removeModal()
      }
    })

    # Restore initial state.
    observeEvent(input$snapshot_reset, {
      s <- "Initial application state"
      ### Begin restore procedure. ###
      snapshot <- snapshot_history()[[s]]
      snapshot_state <- redress_slices(snapshot)
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
      ### End restore procedure. ###
    })

    # Create table to display list of snapshots and their actions.
    output$snapshot_list <- renderUI({
      lapply(names(snapshot_history())[-1L], function(s) {
        id_pickme <- sprintf("pickme_%s", make.names(s))
        id_saveme <- sprintf("saveme_%s", make.names(s))

        # Listen for button to restore snapshot.
        observeEvent(input[[id_pickme]], {
          ### Begin restore procedure. ###
          snapshot <- snapshot_history()[[s]]
          snapshot_state <- redress_slices(snapshot)
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
          ### End restore procedure. ###
        })

        # Listen for button to save snapshot.
        output[[id_saveme]] <- downloadHandler(
          filename = function() {
            sprintf("teal_snapshot-%s.json", Sys.Date())
          },
          content = function(file) {
            snapshot <- snapshot_history()[[s]]
            snapshot_state <- redress_slices(snapshot)
            teal.slice::slices_store(tss = snapshot_state, file = file)
          }
        )

        # Create a row for the snapshot table.
        div(
          class = "snapshot_table_row",
          span(h5(s)),
          actionLink(inputId = ns(id_pickme), label = icon("circle-check"), title = "select"),
          downloadLink(outputId = ns(id_saveme), label = icon("save"), title = "save to file")
        )
      })
    })

  })
}




### utility functions
# these will end up in other files, presented here for convenience

# convert teal_slices and to list of lists (drop classes), while maintaining attributes
# adds special class so that the reverse can have assertion on argument type
strip_slices <- function(tss) {
  checkmate::assert_class(tss, "teal_slices")
  ans <- unclass(tss)
  ans[] <- lapply(ans, as.list)
  class(ans) <- "teal_slices_stripped"
  ans
}

# rebuild teal_slices from list of lists
redress_slices <- function(x) {
  checkmate::assert_class(x, "teal_slices_stripped")
  attrs <- attributes(unclass(x))
  ans <- lapply(x, as.teal_slice)
  do.call(teal_slices, c(ans, attrs))
}

# This is needed temporarily, while filter_var and filter_expr are separate functions.
# When both classes can be created by one function (that is exported), that can be called in redress_slices.
as.teal_slice <- getFromNamespace("as.teal_slice", "teal.slice") # nolint


# transform module mapping such that global filters are explicitly specified for every module
# @param mapping named list as stored in mapping parameter of `teal_slices`
# @param module_names character vector enumerating names of all modules in the app
unfold_mapping <- function(mapping, module_names) {
  module_names <- structure(module_names, names = module_names)
  lapply(module_names, function(x) c(mapping[[x]], mapping[["global_filters"]]))
}

# convert filter mapping matirx to mapping specification
# @param mapping_matrix data.frame of logicals vectors; columns represent modules and row represent teal_slices
# @return named list like that in the mapping attribute of `teal_slices`
matrix_to_mapping <- function(mapping_matrix) {
  global <- vapply(as.data.frame(t(mapping_matrix)), all, logical(1L))
  global_filters <- names(global[global])
  local_filters <- mapping_matrix[!rownames(mapping_matrix) %in% global_filters, ]

  mapping <- c(lapply(local_filters, function(x) rownames(local_filters)[x]), list(global_filters = global_filters))
  Filter(function(x) length(x) != 0L, mapping)
}
