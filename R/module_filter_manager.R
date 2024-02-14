#' Manage multiple `FilteredData` objects
#'
#' Oversee filter states across the entire application.
#'
#' This module observes changes in the filters of each `FilteredData` object
#' and keeps track of all filters used. A mapping of filters to modules
#' is kept in the `mapping_matrix` object (which is actually a `data.frame`)
#' that tracks which filters (rows) are active in which modules (columns).
#'
#' @name module_filter_manager
#'
#' @param id (`character(1)`)
#'  `shiny` module id.
#' @param filtered_data_list (named `list`)
#'  A list, possibly nested, of `FilteredData` objects.
#'  Each `FilteredData` will be served to one module in the `teal` application.
#'  The structure of the list must reflect the nesting of modules in tabs
#'  and the names of the list must match the labels of their respective modules.
#' @inheritParams init
#' @return A list of `reactive`s, each holding a `teal_slices`, as returned by `filter_manager_module_srv`.
#' @keywords internal
#'
NULL

#' Filter manager modal
#'
#' Opens a modal containing the filter manager UI.
#'
#' @name module_filter_manager_modal
#' @inheritParams module_filter_manager
#' @keywords internal
#'
NULL

#' @rdname module_filter_manager_modal
filter_manager_modal_ui <- function(id) {
  ns <- NS(id)
  tags$button(
    id = ns("show"),
    class = "btn action-button filter_manager_button",
    title = "Show filters manager modal",
    icon("gear")
  )
}

#' @rdname module_filter_manager_modal
filter_manager_modal_srv <- function(id, filtered_data_list, filter) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$show, {
      logger::log_trace("filter_manager_modal_srv@1 show button has been clicked.")
      showModal(
        modalDialog(
          filter_manager_ui(session$ns("filter_manager")),
          size = "l",
          footer = NULL,
          easyClose = TRUE
        )
      )
    })

    filter_manager_srv("filter_manager", filtered_data_list, filter)
  })
}

#' @rdname module_filter_manager
filter_manager_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "filter_manager_content",
    tableOutput(ns("slices_table")),
    snapshot_manager_ui(ns("snapshot_manager"))
  )
}

#' @rdname module_filter_manager
filter_manager_srv <- function(id, filtered_data_list, filter) {
  moduleServer(id, function(input, output, session) {
    logger::log_trace("filter_manager_srv initializing for: { paste(names(filtered_data_list), collapse = ', ')}.")

    is_module_specific <- isTRUE(attr(filter, "module_specific"))

    # Create a global list of slices.
    # Contains all available teal_slice objects available to all modules.
    # Passed whole to instances of FilteredData used for individual modules.
    # Down there a subset that pertains to the data sets used in that module is applied and displayed.
    slices_global <- reactiveVal(filter)

    filtered_data_list <-
      if (!is_module_specific) {
        # Retrieve the first FilteredData from potentially nested list.
        # List of length one is named "global_filters" because that name is forbidden for a module label.
        list(global_filters = unlist(filtered_data_list)[[1]])
      } else {
        # Flatten potentially nested list of FilteredData objects while maintaining useful names.
        # Simply using `unlist` would result in concatenated names.
        flatten_nested <- function(x, name = NULL) {
          if (inherits(x, "FilteredData")) {
            setNames(list(x), name)
          } else {
            unlist(lapply(names(x), function(name) flatten_nested(x[[name]], name)))
          }
        }
        flatten_nested(filtered_data_list)
      }

    # Create mapping of filters to modules in matrix form (presented as data.frame).
    # Modules get NAs for filters that cannot be set for them.
    mapping_matrix <- reactive({
      state_ids_global <- vapply(slices_global(), `[[`, character(1L), "id")
      mapping_smooth <- lapply(filtered_data_list, function(x) {
        state_ids_local <- vapply(x$get_filter_state(), `[[`, character(1L), "id")
        state_ids_allowed <- vapply(x$get_available_teal_slices()(), `[[`, character(1L), "id")
        states_active <- state_ids_global %in% state_ids_local
        ifelse(state_ids_global %in% state_ids_allowed, states_active, NA)
      })

      as.data.frame(mapping_smooth, row.names = state_ids_global, check.names = FALSE)
    })

    output$slices_table <- renderTable(
      expr = {
        # Display logical values as UTF characters.
        mm <- mapping_matrix()
        mm[] <- lapply(mm, ifelse, yes = intToUtf8(9989), no = intToUtf8(10060))
        mm[] <- lapply(mm, function(x) ifelse(is.na(x), intToUtf8(128306), x))
        if (!is_module_specific) colnames(mm) <- "Global Filters"

        # Display placeholder if no filters defined.
        if (nrow(mm) == 0L) {
          mm <- data.frame(`Filter manager` = "No filters specified.", check.names = FALSE)
          rownames(mm) <- ""
        }

        # Report Previewer will not be displayed.
        mm[names(mm) != "Report previewer"]
      },
      align = paste(c("l", rep("c", sum(names(filtered_data_list) != "Report previewer"))), collapse = ""),
      rownames = TRUE
    )

    # Create list of module calls.
    modules_out <- lapply(names(filtered_data_list), function(module_name) {
      filter_manager_module_srv(
        id = module_name,
        module_fd = filtered_data_list[[module_name]],
        slices_global = slices_global
      )
    })

    # Call snapshot manager.
    snapshot_manager_srv("snapshot_manager", slices_global, mapping_matrix, filtered_data_list)

    modules_out # returned for testing purpose
  })
}

#' Module specific filter manager
#'
#' Tracks filter states in a single module.
#'
#' This module tracks the state of a single `FilteredData` object and global `teal_slices`
#' and updates both objects as necessary. Filter states added in different modules
#' Filter states added any individual module are added to global `teal_slices`
#' and from there become available in other modules
#' by setting `private$available_teal_slices` in each `FilteredData`.
#'
#' @param id (`character(1)`)
#'  `shiny` module id.
#' @param module_fd (`FilteredData`)
#'   Object containing the data to be filtered in a single `teal` module.
#' @param slices_global (`reactiveVal`)
#'   stores `teal_slices` with all available filters; allows the following actions:
#'   - to disable/enable a specific filter in a module
#'   - to restore saved filter settings
#'   - to save current filter panel settings
#' @return A `reactive` expression containing the slices active in this module.
#' @keywords internal
#'
filter_manager_module_srv <- function(id, module_fd, slices_global) {
  moduleServer(id, function(input, output, session) {
    # Only operate on slices that refer to data sets present in this module.
    module_fd$set_available_teal_slices(reactive(slices_global()))

    # Track filter state of this module.
    slices_module <- reactive(module_fd$get_filter_state())

    # Reactive values for comparing states.
    previous_slices <- reactiveVal(isolate(slices_module()))
    slices_added <- reactiveVal(NULL)

    # Observe changes in module filter state and trigger appropriate actions.
    observeEvent(slices_module(), ignoreNULL = FALSE, {
      logger::log_trace("filter_manager_srv@1 detecting states deltas in module: { id }.")
      added <- setdiff_teal_slices(slices_module(), slices_global())
      if (length(added)) slices_added(added)
      previous_slices(slices_module())
    })

    observeEvent(slices_added(), ignoreNULL = TRUE, {
      logger::log_trace("filter_manager_srv@2 added filter in module: { id }.")
      # In case the new state has the same id as an existing state, add a suffix to it.
      global_ids <- vapply(slices_global(), `[[`, character(1L), "id")
      lapply(
        slices_added(),
        function(slice) {
          if (slice$id %in% global_ids) {
            slice$id <- utils::tail(make.unique(c(global_ids, slice$id), sep = "_"), 1)
          }
        }
      )
      slices_global_new <- c(slices_global(), slices_added())
      slices_global(slices_global_new)
      slices_added(NULL)
    })

    slices_module # returned for testing purpose
  })
}
