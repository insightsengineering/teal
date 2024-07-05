#' Manage multiple `FilteredData` objects
#'
#' @description
#' Oversee filter states across the entire application.
#'
#' @section Slices global:
#' The key role in this process is played by the single `slices_global` (`reactiveVal`) object
#' which stores all `teal_slice` objects and mapping to each module.
#' `slices_global` is created by `.make_slices_global` function which in the same time resolves
#' `attr("mapping")` to keep it consistent for filter manager:
#' - list contains elements named after modules' labels containing `id` of active slices. Initially,
#' app developer can specify mapping for some modules, and `make_slices_global` includes unspecified
#' modules in the mapping list.
#' - element `global_filters` in the mapping list is removed in favour of module slots.
#'
#' @section Filter manager:
#' Filter-manager is split into two parts:
#' 1. `ui/srv_filter_manager_panel` - Called once for the whole app. This module observes changes in
#' the filters in `slices_global` and displays them in a table utilising information from `mapping`:
#'   - &#9989; (`TRUE`) - filter is active in the module
#'   - &#10060; (`FALSE`) - filter is inactive in the module
#'   - &#128306; (`NA`) - filter is not available in the module
#' 2. `ui/srv_module_filter_manager` - Called once for each `teal_module`. Handling filter states
#' for of single module and keeping module `FilteredData` consistent with `slices_global`, so that
#' local filters are always reflected in the `slices_global` and its mapping and vice versa.
#'
#'
#' @param id (`character(1)`)
#'  `shiny` module instance id.
#'
#' @param slices_global (`reactiveVal`)
#'  containing `teal_slices`.
#'
#' @param module_fd (`FilteredData`)
#'   Object containing the data to be filtered in a single `teal` module.
#'
#' @param filter (`teal_slices`)
#'   initial `teal` filter settings.
#'
#' @return
#' Module returns a `slices_global` (`reactiveVal`) containing a `teal_slices` object with mapping.
#'
#' @name module_filter_manager
#' @aliases filter_manager slices_global
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
srv_filter_manager_panel <- function(id, slices_global) {
  checkmate::assert_string(id)
  checkmate::assert_class(slices_global, "reactiveVal")
  checkmate::assert_class(isolate(slices_global()), "teal_slices")
  moduleServer(id, function(input, output, session) {
    setBookmarkExclude(c("show_filter_manager"))
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
    srv_filter_manager("filter_manager", slices_global = slices_global)
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
srv_filter_manager <- function(id, slices_global) {
  checkmate::assert_string(id)
  checkmate::assert_class(slices_global, "reactiveVal")
  checkmate::assert_class(isolate(slices_global()), "teal_slices")

  moduleServer(id, function(input, output, session) {
    logger::log_trace("filter_manager_srv initializing.")

    # Bookmark slices global with mapping.
    session$onBookmark(function(state) {
      logger::log_trace("filter_manager_srv@onBookmark: storing filter state")
      state$values$filter_state_on_bookmark <- as.list(
        slices_global(),
        recursive = TRUE
      )
    })

    # singleton needed to receive information from modules' `FilteredData`. Needed to determine filters
    #  not available for specific modules (e.g. filters which are for datanames not used in a module)
    session$userData$module_slices_api <- list()

    mapping_table <- reactive({
      # We want this to be reactive on slices_global() only as get_available_teal_slices()
      #   is dependent on slices_global().
      is_global <- !isTRUE(attr(slices_global(), "module_specific"))
      module_labels <- setdiff(
        names(attr(slices_global(), "mapping")),
        "Report previewer"
      )
      isolate({
        mm <- as.data.frame(
          lapply(
            if (is_global) module_labels[1] else module_labels,
            function(module_label) {
              available_slices <- session$userData$module_slices_api[[module_label]]$get_available_teal_slices()
              global_ids <- sapply(slices_global(), `[[`, "id", simplify = FALSE)
              module_ids <- unlist(attr(slices_global(), "mapping")[module_label])
              ids_active <- global_ids %in% module_ids
              ids_allowed <- vapply(available_slices, `[[`, character(1L), "id")
              setNames(
                ifelse(global_ids %in% ids_allowed, ids_active, NA),
                global_ids
              )
            }
          )
        )
        colnames(mm) <- if (is_global) "Global filters" else module_labels

        mm
      })
    })

    output$slices_table <- renderTable(
      expr = {
        logger::log_trace("filter_manager_srv@1 rendering slices_table.")
        mm <- mapping_table()

        # Display logical values as UTF characters.
        mm[] <- lapply(mm, ifelse, yes = intToUtf8(9989), no = intToUtf8(10060))
        mm[] <- lapply(mm, function(x) ifelse(is.na(x), intToUtf8(128306), x))

        # Display placeholder if no filters defined.
        if (nrow(mm) == 0L) {
          mm <- data.frame(`Filter manager` = "No filters specified.", check.names = FALSE)
          rownames(mm) <- ""
        }
        mm
      },
      # align = paste(c("l", rep("c", sum(module_labels != "Report previewer"))), collapse = ""),
      rownames = TRUE
    )

    NULL
  })
}

#' @rdname module_filter_manager
#' @keywords internal
srv_module_filter_manager <- function(id, module_fd, slices_global) {
  checkmate::assert_string(id)
  checkmate::assert_class(module_fd, "reactive")
  checkmate::assert_class(slices_global, "reactiveVal")
  checkmate::assert_class(isolate(slices_global()), "teal_slices")

  moduleServer(id, function(input, output, session) {
    logger::log_trace("filter_manager_srv initializing for module: { id }.")
    # Track filter global and local states.
    slices_global_module <- reactive({
      .filter_module_slices(module_label = id, slices = slices_global())
    })
    slices_module <- reactive(req(module_fd())$get_filter_state())

    # Set (reactively) available filters for the module.
    obs1 <- observeEvent(module_fd(), {
      logger::log_trace("filter_manager_srv@1 setting initial slices for module: { id }.")
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
      session$userData$module_slices_api[[id]] <- list(
        get_available_teal_slices = module_fd()$get_available_teal_slices()
        # in the future we can add more methods to share information across modules (for example setting filters)
      )
    })

    # Update global state and mapping matrix when module filters change.
    obs2 <- observeEvent(slices_module(), {
      # if is needed as c.teal_slices recreates an object.
      # It means `c(slices)` is not identical to `slices`
      global_ids <- vapply(slices_global(), `[[`, character(1L), "id")
      new_slices <- setdiff_teal_slices(slices_module(), slices_global())
      if (length(new_slices)) {
        logger::log_trace("filter_manager_srv@2 new filter added to module: { id }. Updating slices_global.")
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

      logger::log_trace("filter_manager_srv@2 updating filter mapping for module: { id }.")
      mapping_matrix[[id]] <- module_ids
      new_slices_global <- slices_global()
      attr(new_slices_global, "mapping") <- mapping_matrix
      slices_global(new_slices_global)
    })

    obs3 <- observeEvent(slices_global_module(), {
      global_vs_module <- setdiff_teal_slices(slices_global_module(), slices_module())
      module_vs_global <- setdiff_teal_slices(slices_module(), slices_global_module())
      if (length(global_vs_module) || length(module_vs_global)) {
        # Comment: (Nota Bene) Normally new filters for a module are added through module-filter-panel, and slices
        # global are updated automatically so slices_module -> slices_global_module are equal.
        # this if is valid only when a change is made on the global level so the change needs to be propagated down
        # to the module (for example through snapshot manager). If it happens both slices are different
        logger::log_trace("filter_manager_srv@3 (N.B.) global state has changed for a module:{ id }.")
        module_fd()$clear_filter_states()
        module_fd()$set_filter_state(slices_global_module())
      }
    })

    slices_module # returned for testing purpose
  })
}

#' @rdname module_filter_manager
#' @keywords internal
.make_slices_global <- function(filter, modules) {
  # Restore filter from bookmarked state, if applicable.
  session <- shiny::getDefaultReactiveDomain()
  filter_restored <- restoreValue("filter_manager_panel-filter_manager-filter_state_on_bookmark", filter)
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
    unlist(modules, use.names = FALSE),
    simplify = FALSE,
    function(module_label) {
      unlist(attr(filter_restored, "mapping")[c(module_label, "global_filters")], use.names = FALSE)
    }
  )
  attr(filter_restored, "mapping") <- new_mapping

  reactiveVal(filter_restored)
}

#' Filter module slices
#' @param module_label (`character(1)`)
#'  label of the `teal_module`
#' @param slices (`teal_slices`)
#'  object containing all filters and mapping
#' @return
#' `teal_slices` object containing only filters for the specified module
#' @keywords internal
.filter_module_slices <- function(module_label, slices) {
  module_ids <- unlist(attr(slices, "mapping")[c(module_label, "global_filters")])
  Filter(
    function(slice) slice$id %in% module_ids,
    slices
  )
}
