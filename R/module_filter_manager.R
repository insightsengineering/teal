#' Manage multiple `FilteredData` objects
#'
#' Oversee filter states across the entire application.
#'
#' This module observes changes in the filters in `session$userData$slices_global`
#' and displays them in a table utilising information from `mapping`.
#'
#' @param id (`character(1)`)
#'  `shiny` module instance id.
#' @param filter (`teal_slices`)
#'  initial `teal` filter settings.
#' @param module_labels (`character`)
#'  vector of module labels.
#' @inheritParams init
#'
#' @return
#' Module returns `NULL` but it registers singletons in `session$userData`:
#' - `slices_global`: `reactiveVal` containing a `teal_slices` object,
#' - `module_slices_api`: list of `function`(s) linking `FilteredData$get_available_teal_slices` of each module.
#'
#' @name module_filter_manager
#' @aliases filter_manager filter_manager_module
#'
NULL

#' @rdname module_filter_manager
#' @keywords internal
ui_filter_manager_panel <- function(id) {
  ns <- NS(id)
  tags$button(
    id = ns("show_filter_manager"),
    class = "btn action-button wunder_bar_button",
    title = "View filter mapping",
    suppressMessages(icon("solid fa-grip"))
  )
}

#' @rdname module_filter_manager
#' @keywords internal
srv_filter_manager_panel <- function(id, filter, module_labels) {
  moduleServer(id, function(input, output, session) {
    setBookmarkExclude(c("show_snapshot_manager"))
    observeEvent(input$show_filter_manager, {
      logger::log_trace("srv_filter_manager_panel@1 show_filter_manager button has been clicked.")
      showModal(
        modalDialog(
          ui_filter_manager(session$ns("filter_manager")),
          class = "filter_manager_modal",
          size = "l",
          footer = NULL,
          easyClose = TRUE
        )
      )
    })
    srv_filter_manager("filter_manager", filter, module_labels)
  })
}


#' @rdname module_filter_manager
#' @keywords internal
ui_filter_manager <- function(id) {
  ns <- NS(id)

  actionButton(ns("filter_manager"), NULL, icon = icon("filter"))
  tags$div(
    class = "filter_manager_content",
    tableOutput(ns("slices_table"))
  )
}

#' @rdname module_filter_manager
#' @keywords internal
srv_filter_manager <- function(id, filter, module_labels) {
  moduleServer(id, function(input, output, session) {
    logger::log_trace("filter_manager_srv initializing.")

    # Bookmark slices global with mapping.
    session$onBookmark(function(state) {
      logger::log_trace("filter_manager_srv@onBookmark: storing filter state")
      old_filters <- as.list(slices_global(), recursive = TRUE)
      attr(snapshot, "mapping") <- attr(slices_global(), "mapping")
      state$values$filter_state_on_bookmark <- snapshot
    })

    # Restore filter from bookmarked state, if applicable.
    filter_restored <- restoreValue("filter_state_on_bookmark", filter)
    if (!is.teal_slices(filter_restored)) {
      filter_restored <- as.teal_slices(filter_restored)
    }

    # Resolve mapping list to keep it constistent for filter manager.
    # - when !module_specific then all filters are global
    # - global_filters ids needs to be repeated in mapping for each module
    if (!isTRUE(attr(filter_restored, "module_specific"))) {
      attr(filter_restored, "mapping") <- list(
        global_filters = isolate(sapply(filter_restored, `[[`, "id"))
      )
    }
    new_mapping <- sapply(
      unlist(module_labels, use.names = FALSE),
      simplify = FALSE,
      function(module_label) {
        unlist(attr(filter_restored, "mapping")[c(module_label, "global_filters")], use.names = FALSE)
      }
    )
    attr(filter_restored, "mapping") <- new_mapping

    # singletons controlled by filter-manager
    session$userData$slices_global <- reactiveVal(filter_restored)
    session$userData$module_slices_api <- list()

    output$slices_table <- renderTable(
      expr = {
        logger::log_trace("filter_manager_srv@1 rendering slices_table.")
        mm <- as.data.frame(
          sapply(
            module_labels,
            function(module_label) {
              available_slices <- session$userData$module_slices_api[[module_label]]$get_available_teal_slices()
              global_ids <- sapply(session$userData$slices_global(), `[[`, "id", simplify = FALSE)
              module_ids <- unlist(attr(session$userData$slices_global(), "mapping")[module_label])
              ids_active <- global_ids %in% module_ids
              ids_allowed <- vapply(available_slices, `[[`, character(1L), "id")
              setNames(
                ifelse(global_ids %in% ids_allowed, ids_active, NA),
                global_ids
              )
            }
          )
        )
        # Display logical values as UTF characters.
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
      # align = paste(c("l", rep("c", sum(module_labels != "Report previewer"))), collapse = ""),
      rownames = TRUE
    )

    NULL
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
#' @param module_label (`character(1)`)
#'  `shiny` module id. Should be a `label` of a `teal_module`.
#' @param module_fd (`FilteredData`)
#'   Object containing the data to be filtered in a single `teal` module.
#'
#' @return A `reactive` expression containing a `teal_slices` with the slices active in this module.
#' @keywords internal
#'
srv_module_filter_manager <- function(module_label, module_fd) {
  checkmate::assert_character(module_label, max.len = 1, any.missing = FALSE)
  checkmate::assert_class(module_fd, "reactive")

  moduleServer(module_label, function(input, output, session) {
    logger::log_trace("filter_manager_srv initializing for module: { module_label }.")
    # Track filter global and local states.
    slices_global <- session$userData$slices_global
    slices_global_module <- reactive({
      Filter(
        function(slice) {
          slice$id %in% unlist(attr(slices_global(), "mapping")[module_label])
        },
        slices_global()
      )
    })
    slices_module <- reactive(req(module_fd())$get_filter_state())

    # Set (reactively) available filters for the module.
    observeEvent(module_fd(), {
      logger::log_trace("filter_manager_srv@1 setting initial slices for module: { module_label }.")
      # Filters relevant for the module in module-specific app.
      slices <- slices_global_module()
      # Setting filter states from slices_global:
      # 1. when app initializes slices_global set to initial filters (specified by app developer)
      # 2. when data reinitializes slices_global reflects latest filter states
      module_fd()$set_filter_state(slices)

      # irrelevant filters are discarded in FilteredData$set_available_teal_slices
      # it means we don't need to subset slices_global() from filters refering to irrelevant datasets
      module_fd()$set_available_teal_slices(slices_global)

      # this needed in filter_manager_srv
      session$userData$module_slices_api[[module_label]] <- list(
        get_available_teal_slices = function() module_fd()$get_available_teal_slices()()
        # in the future we can add more methods to share information across modules (for example setting filters)
      )
    })

    # Update global state and mapping matrix when module filters change.
    observeEvent(slices_module(), {
      # if is needed as c.teal_slices recreates an object.
      # It means `c(slices)` is not identical to `slices`
      global_ids <- vapply(slices_global(), `[[`, character(1L), "id")
      new_slices <- setdiff_teal_slices(slices_module(), slices_global())
      if (length(new_slices)) {
        logger::log_trace("filter_manager_srv@2 new filter added to module: { module_label }. Updating slices_global.")
        lapply(new_slices, function(slice) {
          # In case the new state has the same id as an existing one, add a suffix
          if (slice$id %in% global_ids) {
            slice$id <- utils::tail(make.unique(c(global_ids, slice$id), sep = "_"), 1)
          }
        })
        new_slices_global <- c(slices_global(), new_slices)
        slices_global(new_slices_global)

        # because new filters has been added we need to update global_ids variable
        global_ids <- vapply(slices_global(), `[[`, character(1L), "id")
      }

      # Set ids of the filters in the mapping matrix for the module
      module_ids <- vapply(slices_module(), `[[`, character(1L), "id")
      mapping_matrix <- attr(slices_global(), "mapping")
      # if (!setequal(module_ids, mapping_matrix[[module_label]]))
      {
        logger::log_trace("filter_manager_srv@2 updating filter mapping for module: { module_label }.")
        mapping_matrix[[module_label]] <- module_ids
        new_slices_global <- slices_global()
        attr(new_slices_global, "mapping") <- mapping_matrix
        slices_global(new_slices_global)
      }
    })

    observeEvent(slices_global_module(), {
      global_vs_module <- setdiff_teal_slices(slices_global_module(), slices_module())
      module_vs_global <- setdiff_teal_slices(slices_module(), slices_global_module())
      if (length(global_vs_module) || length(module_vs_global)) {
        # Comment: (Nota Bene) Normally new filters for a module are added through module-filter-panel, and slices
        # global are updated automatically so slices_module -> slices_global_module are equal.
        # this if is valid only when a change is made on the global level so the change needs to be propagated down
        # to the module (for example through snapshot manager). If it happens both slices are different
        logger::log_trace("filter_manager_srv@3 (N.B.) global state has changed for a module:{ module_label }.")
        module_fd()$clear_filter_states()
        module_fd()$set_filter_state(slices_global_module())
      }
    })

    slices_module # returned for testing purpose
  })
}
