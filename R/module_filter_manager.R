#' Manage multiple `FilteredData` objects
#'
#' Oversee filter states across the entire application.
#'
#' This module observes changes in the filters of each `FilteredData` object
#' and keeps track of all filters used. A mapping of filters to modules
#' is kept in the `mapping_matrix` object (which is actually a `data.frame`)
#' that tracks which filters (rows) are active in which modules (columns).
#'
#' @param id (`character(1)`)
#'  `shiny` module instance id.
#' @param datasets (named `list`)
#'  A list, possibly nested, of `FilteredData` objects.
#'  Each `FilteredData` will be served to one module in the `teal` application.
#'  The structure of the list must reflect the nesting of modules in tabs
#'  and the names of the list must match the labels of their respective modules.
#' @inheritParams init
#'
#' @return
#' A `list` containing:
#'
#' objects used by other manager modules
#' - `datasets_flat`: named list of `FilteredData` objects,
#' - `mapping_matrix`: `reactive` containing a `data.frame`,
#' - `slices_global`: `reactiveVal` containing a `teal_slices` object,
#'
#' objects used for testing
#' - modules_out: `list` of `reactive`s, each holding a `teal_slices`, as returned by `filter_manager_module_srv`.
#'
#' @name module_filter_manager
#' @aliases filter_manager filter_manager_module
#'

#' @rdname module_filter_manager
#' @keywords internal
#'
filter_manager_ui <- function(id) {
  ns <- NS(id)
  tags$div(
    class = "filter_manager_content",
    tableOutput(ns("slices_table"))
  )
}

#' @rdname module_filter_manager
#' @keywords internal
#'
filter_manager_srv <- function(id, is_module_specific) {
  moduleServer(id, function(input, output, session) {
    logger::log_trace("filter_manager_srv initializing.")

    output$slices_table <- renderTable(
      expr = {
        mapping_list <- session$userData$slices_mapping
        # Display logical values as UTF characters.
        global_ids <- vapply(session$userData$slices_global(), `[[`, character(1L), "id")
        mm <- as.data.frame(do.call(cbind, mapping_list))
        mm[] <- lapply(mm, ifelse, yes = intToUtf8(9989), no = intToUtf8(10060))
        mm[] <- lapply(mm, function(x) ifelse(is.na(x), intToUtf8(128306), x))

        # Display placeholder if no filters defined.
        if (nrow(mm) == 0L) {
          mm <- data.frame(`Filter manager` = "No filters specified.", check.names = FALSE)
          rownames(mm) <- ""
        }

        # Report Previewer will not be displayed.
        mm[names(mm) != "Report previewer"]
      },
      # align = paste(c("l", rep("c", sum(names(datasets_flat) != "Report previewer"))), collapse = ""),
      rownames = TRUE
    )
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
#'  `shiny` module id. Should be a `label` of a `teal_module`.
#' @param module_fd (`FilteredData`)
#'   Object containing the data to be filtered in a single `teal` module.
#' @param slices_global (`reactiveVal`)
#'   stores `teal_slices` with all available filters; allows the following actions:
#'   - to disable/enable a specific filter in a module
#'   - to restore saved filter settings
#'   - to save current filter panel settings
#' @return A `reactive` expression containing a `teal_slices` with the slices active in this module.
#' @keywords internal
#'
filter_manager_module_srv <- function(module_label, module_fd) {
  checkmate::assert_character(module_label, max.len = 1, any.missing = FALSE)
  checkmate::assert_class(module_fd, "reactive")

  moduleServer(module_label, function(input, output, session) {
    # Track filter global and local states.
    slices_global <- session$userData$slices_global
    slices_module <- reactive(req(module_fd())$get_filter_state())

    # Set (reactively) available filters for the module.
    observeEvent(module_fd(), {
      # setting filter states from slices_global:
      # 1. when data initializes it takes initial slices set in module_teal
      # 2. when data reinitializes it takes slices from the last state
      slices <- Filter(
        function(slice) {
          # todo: add global_filters if mapping isn't provided (global filter)
          isolate(slice$id) %in% unlist(attr(slices_global(), "mapping")[c(module_label, "global_filters")])
        },
        slices_global()
      )
      module_fd()$set_filter_state(slices)

      # FilteredData$set_available_teal_slices discards irrelevant filters
      # it means we don't need to subset slices_global() from filters refering to irrelevant datasets
      module_fd()$set_available_teal_slices(reactive(slices_global()))
    })

    # Update global state and mapping matrix.
    observeEvent(slices_module(), {
      # if is needed as c.teal_slices recreates an object.
      # It means `c(slices)` is not identical to `slices`
      global_ids <- vapply(slices_global(), `[[`, character(1L), "id")
      new_slices <- setdiff_teal_slices(slices_module(), slices_global())
      if (length(new_slices)) {
        logger::log_trace("filter_manager_srv@2 added filter in module: { module_label }.")
        # In case the new state has the same id as an existing state, add a suffix to it
        lapply(
          new_slices,
          function(slice) {
            if (slice$id %in% global_ids) {
              slice$id <- utils::tail(make.unique(c(global_ids, slice$id), sep = "_"), 1)
            }
          }
        )
        # todo: when new filter is added in one module in module-specific app it is also
        #       added to other modules. It should be added only to the module where it was added.
        new_slices_global <- c(slices_global(), new_slices)
        slices_global(new_slices_global)

        # because new filters has been added
        global_ids <- vapply(slices_global(), `[[`, character(1L), "id")
      }

      # Set ids of the filters in the mapping matrix for the module
      logger::log_trace("filter_manager_srv@2 updating filter mapping for module: { module_label }.")
      module_ids <- vapply(slices_module(), `[[`, character(1L), "id")
      mapping_matrix <- attr(slices_global(), "mapping")
      mapping_matrix[[module_label]] <- module_ids
      new_slices_with_mapping <- slices_global()
      attr(new_slices_with_mapping, "mapping") <- mapping_matrix
      slices_global(new_slices_with_mapping)

      # Special version of mapping:
      # - TRUE if filter is active in the module
      # - FALSE if filter is inactive but available for the module
      # - NA if filter is not available for the module
      ids_allowed <- vapply(module_fd()$get_available_teal_slices()(), `[[`, character(1L), "id")
      ids_active <- global_ids %in% module_ids
      session$userData$slices_mapping[[module_label]] <- setNames(
        ifelse(global_ids %in% ids_allowed, ids_active, NA),
        global_ids
      )
    })

    slices_module # returned for testing purpose
  })
}



# utilities ----

#' Flatten potentially nested list of FilteredData objects while maintaining useful names.
#' Simply using `unlist` would result in concatenated names.
#' A single `FilteredData` will result in a list named "Global Filters"
#' because that name used in the mapping matrix display.
#' @param x `FilteredData` or a `list` thereof
#' @param name (`character(1)`) string used to name `x` in the resulting list
#' @return Unnested named list of `FilteredData` objects.
#' @keywords internal
#' @noRd
#'
flatten_datasets <- function(x, name = "Global Filters") {
  if (inherits(x, "FilteredData")) {
    setNames(list(x), name)
  } else {
    unlist(lapply(names(x), function(name) flatten_datasets(x[[name]], name)))
  }
}
