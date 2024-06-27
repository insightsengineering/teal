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
filter_manager_srv <- function(id, datasets, filter) {
  moduleServer(id, function(input, output, session) {
    logger::log_trace("filter_manager_srv initializing for: { paste(names(datasets), collapse = ', ')}.")

    is_module_specific <- isTRUE(attr(filter, "module_specific"))

    # Create a global list of slices.
    # Contains all available teal_slice objects available to all modules.
    # Passed whole to instances of FilteredData used for individual modules.
    # Down there a subset that pertains to the data sets used in that module is applied and displayed.
    slices_global <- reactiveVal(filter)

    datasets_flat <-
      if (!is_module_specific) {
        flatten_datasets(unlist(datasets)[[1]])
      } else {
        flatten_datasets(datasets)
      }

    # Create mapping of filters to modules in matrix form (presented as data.frame).
    # Modules get NAs for filters that cannot be set for them.
    mapping_matrix <- reactive({
      state_ids_global <- vapply(slices_global(), `[[`, character(1L), "id")
      mapping_smooth <- lapply(datasets_flat, function(x) {
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

        # Display placeholder if no filters defined.
        if (nrow(mm) == 0L) {
          mm <- data.frame(`Filter manager` = "No filters specified.", check.names = FALSE)
          rownames(mm) <- ""
        }

        # Report Previewer will not be displayed.
        mm[names(mm) != "Report previewer"]
      },
      align = paste(c("l", rep("c", sum(names(datasets_flat) != "Report previewer"))), collapse = ""),
      rownames = TRUE
    )

    # Create list of module calls.
    modules_out <- lapply(names(datasets_flat), function(module_name) {
      filter_manager_module_srv(
        id = module_name,
        module_fd = datasets_flat[[module_name]],
        slices_global = slices_global
      )
    })

    list(
      slices_global = slices_global,
      mapping_matrix = mapping_matrix,
      datasets_flat = datasets_flat,
      modules_out = modules_out # returned for testing purpose
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
#'  `shiny` module id.
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
filter_manager_module_srv <- function(id, module_fd) {
  checkmate::assert_character(id, max.len = 1, any.missing = FALSE)
  checkmate::assert_class(module_fd, "reactive")

  moduleServer(id, function(input, output, session) {
    slices_global <- session$userData$slices_global

    # Only operate on slices that refer to data sets present in this module.
    observeEvent(module_fd(), {
      # FilteredData$set_available_teal_slices discards irrelevant filters
      # it means we don't need to subset slices_global() from filters refering to irrelevant datasets
      module_fd()$set_available_teal_slices(reactive(slices_global()))
    })

    # Track filter state of this module.
    slices_module <- reactive(req(module_fd())$get_filter_state())

    # Add new filters to global state.
    # todo: check if ignoreInit doesn't ignore initial filters. Initial filters should be set
    observeEvent(slices_module(), ignoreInit = TRUE, ignoreNULL = TRUE, {
      # if is needed as c.teal_slices recreates an object.
      # It means `c(slices)` is not identical to `slices`
      new_slices <- setdiff_teal_slices(slices_module(), slices_global())
      if (length(new_slices)) {
        logger::log_trace("filter_manager_module_srv@1 adding new slices to slices_global.")
        # todo: check id of "new_slices" and change if any are duplicated
        #       extra note - slices_global can already contain filter which is based on the same column
        #       and by default id is `$dataname $column_name`, so it can be duplicated.
        slices_global(c(slices_global(), new_slices))
      }
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
