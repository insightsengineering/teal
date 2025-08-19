#' Manage multiple `FilteredData` objects
#'
#' @description
#' Oversee filter states across the entire application.
#'
#' @section Slices global:
#' The key role in maintaining the module-specific filter states is played by the `.slicesGlobal`
#' object. It is a reference class that holds the following fields:
#' - `all_slices` (`reactiveVal`) - reactive value containing all filters registered in an app.
#' - `module_slices_api` (`reactiveValues`) - reactive field containing references to each modules'
#' `FilteredData` object methods. At this moment it is used only in `srv_filter_manager` to display
#' the filter states in a table combining informations from `all_slices` and from
#' `FilteredData$get_available_teal_slices()`.
#'
#' During a session only new filters are added to `all_slices` unless [`module_snapshot_manager`] is
#' used to restore previous state. Filters from `all_slices` can be activated or deactivated in a
#' module which is linked (both ways) by `attr(, "mapping")` so that:
#' - If module's filter is added or removed in its `FilteredData` object, this information is passed
#'   to `SlicesGlobal` which updates `attr(, "mapping")` accordingly.
#' - When mapping changes in a `SlicesGlobal`, filters are set or removed from module's
#'  `FilteredData`.
#'
#' @section Filter manager:
#' Filter-manager is split into two parts:
#' 1. `ui/srv_filter_manager_panel` - Called once for the whole app. This module observes changes in
#' the filters in `slices_global` and displays them in a table utilizing information from `mapping`:
#'   - (`TRUE`) - filter is active in the module
#'   - (`FALSE`) - filter is inactive in the module
#'   - (`NA`) - filter is not available in the module
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
#' @return
#' Module returns a `slices_global` (`reactiveVal`) containing a `teal_slices` object with mapping.
#'
#' @encoding UTF-8
#'
#' @name module_filter_manager
#' @rdname module_filter_manager
#'
NULL

#' @rdname module_filter_manager
ui_filter_manager_panel <- function(id) {
  ns <- NS(id)
  .expand_button(
    id = ns("show_filter_manager"),
    label = "Filter Manager",
    icon = "funnel-fill"
  )
}

#' @rdname module_filter_manager
#' @keywords internal
srv_filter_manager_panel <- function(id, slices_global) {
  checkmate::assert_string(id)
  checkmate::assert_class(slices_global, ".slicesGlobal")
  moduleServer(id, function(input, output, session) {
    setBookmarkExclude(c("show_filter_manager"))
    observeEvent(input$show_filter_manager, {
      logger::log_debug("srv_filter_manager_panel@1 show_filter_manager button has been clicked.")
      showModal(
        modalDialog(
          ui_filter_manager(session$ns("filter_manager")),
          class = "teal-filter-manager-modal",
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
ui_filter_manager <- function(id) {
  ns <- NS(id)
  actionButton(ns("filter_manager"), NULL, icon = icon("fas fa-filter"))
  tags$div(
    class = "filter_manager_content",
    tableOutput(ns("slices_table"))
  )
}

#' @rdname module_filter_manager
srv_filter_manager <- function(id, slices_global) {
  checkmate::assert_string(id)
  checkmate::assert_class(slices_global, ".slicesGlobal")

  moduleServer(id, function(input, output, session) {
    logger::log_debug("filter_manager_srv initializing.")

    # Bookmark slices global with mapping.
    session$onBookmark(function(state) {
      logger::log_debug("filter_manager_srv@onBookmark: storing filter state")
      state$values$filter_state_on_bookmark <- as.list(
        slices_global$all_slices(),
        recursive = TRUE
      )
    })

    bookmarked_slices <- restoreValue(session$ns("filter_state_on_bookmark"), NULL)
    if (!is.null(bookmarked_slices)) {
      logger::log_debug("filter_manager_srv: restoring filter state from bookmark.")
      slices_global$slices_set(bookmarked_slices)
    }

    mapping_table <- reactive({
      # We want this to be reactive on slices_global$all_slices() only as get_available_teal_slices()
      #   is dependent on slices_global$all_slices().
      module_labels <- setdiff(
        names(attr(slices_global$all_slices(), "mapping")),
        "Report previewer"
      )
      isolate({
        mm <- as.data.frame(
          sapply(
            module_labels,
            simplify = FALSE,
            function(module_label) {
              available_slices <- slices_global$module_slices_api[[module_label]]$get_available_teal_slices()
              global_ids <- sapply(slices_global$all_slices(), `[[`, "id", simplify = FALSE)
              module_ids <- sapply(slices_global$slices_get(module_label), `[[`, "id", simplify = FALSE)
              allowed_ids <- vapply(available_slices, `[[`, character(1L), "id")
              active_ids <- global_ids %in% module_ids
              setNames(nm = global_ids, ifelse(global_ids %in% allowed_ids, active_ids, NA))
            }
          ),
          check.names = FALSE
        )
        colnames(mm)[colnames(mm) == "global_filters"] <- "Global filters"

        mm
      })
    })

    output$slices_table <- renderTable(
      expr = {
        logger::log_debug("filter_manager_srv@1 rendering slices_table.")
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
      rownames = TRUE
    )

    mapping_table # for testing purpose
  })
}

#' @rdname module_filter_manager
srv_module_filter_manager <- function(id, module_fd, slices_global) {
  checkmate::assert_string(id)
  assert_reactive(module_fd)
  checkmate::assert_class(slices_global, ".slicesGlobal")

  moduleServer(id, function(input, output, session) {
    logger::log_debug("srv_module_filter_manager initializing for module: { id }.")
    # Track filter global and local states.
    slices_global_module <- reactive({
      slices_global$slices_get(module_label = id)
    })
    slices_module <- reactive(req(module_fd())$get_filter_state())

    module_fd_previous <- reactiveVal(NULL)

    # Set (reactively) available filters for the module.
    obs1 <- observeEvent(module_fd(), priority = 1, {
      logger::log_debug("srv_module_filter_manager@1 setting initial slices for module: { id }.")
      # Filters relevant for the module in module-specific app.
      slices <- slices_global_module()

      # Clean up previous filter states and refresh cache of previous module_fd with current
      if (!is.null(module_fd_previous())) module_fd_previous()$destroy()
      module_fd_previous(module_fd())

      # Setting filter states from slices_global:
      # 1. when app initializes slices_global set to initial filters (specified by app developer)
      # 2. when data reinitializes slices_global reflects latest filter states

      module_fd()$set_filter_state(slices)

      # irrelevant filters are discarded in FilteredData$set_available_teal_slices
      # it means we don't need to subset slices_global$all_slices() from filters refering to irrelevant datasets
      module_fd()$set_available_teal_slices(slices_global$all_slices)

      # this needed in filter_manager_srv
      slices_global$module_slices_api_set(
        id,
        list(
          get_available_teal_slices = module_fd()$get_available_teal_slices(),
          set_filter_state = module_fd()$set_filter_state, # for testing purpose
          get_filter_state = module_fd()$get_filter_state # for testing purpose
        )
      )
    })

    # Update global state and mapping matrix when module filters change.
    obs2 <- observeEvent(slices_module(), priority = 0, {
      this_slices <- slices_module()
      slices_global$slices_append(this_slices) # append new slices to the all_slices list
      mapping_elem <- setNames(nm = id, list(vapply(this_slices, `[[`, character(1L), "id")))
      slices_global$slices_active(mapping_elem)
    })

    obs3 <- observeEvent(slices_global_module(), {
      global_vs_module <- setdiff_teal_slices(slices_global_module(), slices_module())
      module_vs_global <- setdiff_teal_slices(slices_module(), slices_global_module())
      if (length(global_vs_module) || length(module_vs_global)) {
        # Comment: (Nota Bene) Normally new filters for a module are added through module-filter-panel, and slices
        # global are updated automatically so slices_module -> slices_global_module are equal.
        # this if is valid only when a change is made on the global level so the change needs to be propagated down
        # to the module (for example through snapshot manager). If it happens both slices are different
        logger::log_debug("srv_module_filter_manager@3 (N.B.) global state has changed for a module:{ id }.")
        module_fd()$clear_filter_states()
        module_fd()$set_filter_state(slices_global_module())
      }
    })

    slices_module # returned for testing purpose
  })
}

#' @importFrom shiny reactiveVal reactiveValues
methods::setOldClass("reactiveVal")
methods::setOldClass("reactivevalues")

#' @importFrom methods new
#' @rdname module_filter_manager
.slicesGlobal <- methods::setRefClass(".slicesGlobal", # nolint: object_name.
  fields = list(
    all_slices = "reactiveVal",
    module_slices_api = "reactivevalues"
  ),
  methods = list(
    initialize = function(slices = teal_slices(), module_labels) {
      shiny::isolate({
        checkmate::assert_class(slices, "teal_slices")
        # needed on init to not mix "global_filters" with module-specific-slots
        if (isTRUE(attr(slices, "module_specific"))) {
          old_mapping <- attr(slices, "mapping")
          new_mapping <- sapply(module_labels, simplify = FALSE, function(module_label) {
            unique(unlist(old_mapping[c(module_label, "global_filters")]))
          })
          attr(slices, "mapping") <- new_mapping
        }
        .self$all_slices <<- shiny::reactiveVal(slices)
        .self$module_slices_api <<- shiny::reactiveValues()
        .self$slices_append(slices)
        .self$slices_active(attr(slices, "mapping"))
        invisible(.self)
      })
    },
    is_module_specific = function() {
      isTRUE(attr(.self$all_slices(), "module_specific"))
    },
    module_slices_api_set = function(module_label, functions_list) {
      shiny::isolate({
        if (!.self$is_module_specific()) {
          module_label <- "global_filters"
        }
        if (!identical(.self$module_slices_api[[module_label]], functions_list)) {
          .self$module_slices_api[[module_label]] <- functions_list
        }
        invisible(.self)
      })
    },
    slices_deactivate_all = function(module_label) {
      shiny::isolate({
        new_slices <- .self$all_slices()
        old_mapping <- attr(new_slices, "mapping")

        new_mapping <- if (.self$is_module_specific()) {
          new_module_mapping <- setNames(nm = module_label, list(character(0)))
          modifyList(old_mapping, new_module_mapping)
        } else if (missing(module_label)) {
          lapply(
            attr(.self$all_slices(), "mapping"),
            function(x) character(0)
          )
        } else {
          old_mapping[[module_label]] <- character(0)
          old_mapping
        }

        if (!identical(new_mapping, old_mapping)) {
          logger::log_debug(".slicesGlobal@slices_deactivate_all: deactivating all slices.")
          attr(new_slices, "mapping") <- new_mapping
          .self$all_slices(new_slices)
        }
        invisible(.self)
      })
    },
    slices_active = function(mapping_elem) {
      shiny::isolate({
        if (.self$is_module_specific()) {
          new_mapping <- modifyList(attr(.self$all_slices(), "mapping"), mapping_elem)
        } else {
          new_mapping <- setNames(nm = "global_filters", list(unique(unlist(mapping_elem))))
        }

        if (!identical(new_mapping, attr(.self$all_slices(), "mapping"))) {
          mapping_modules <- toString(names(new_mapping))
          logger::log_debug(".slicesGlobal@slices_active: changing mapping for module(s): { mapping_modules }.")
          new_slices <- .self$all_slices()
          attr(new_slices, "mapping") <- new_mapping
          .self$all_slices(new_slices)
        }

        invisible(.self)
      })
    },
    # - only new filters are appended to the $all_slices
    # - mapping is not updated here
    slices_append = function(slices, activate = FALSE) {
      shiny::isolate({
        if (!is.teal_slices(slices)) {
          slices <- as.teal_slices(slices)
        }

        # to make sure that we don't unnecessary trigger $all_slices <reactiveVal>
        new_slices <- setdiff_teal_slices(slices, .self$all_slices())
        old_mapping <- attr(.self$all_slices(), "mapping")
        if (length(new_slices)) {
          new_ids <- vapply(new_slices, `[[`, character(1L), "id")
          logger::log_debug(".slicesGlobal@slices_append: appending new slice(s): { new_ids }.")
          slices_ids <- vapply(.self$all_slices(), `[[`, character(1L), "id")
          lapply(new_slices, function(slice) {
            # In case the new state has the same id as an existing one, add a suffix
            if (slice$id %in% slices_ids) {
              slice$id <- utils::tail(make.unique(c(slices_ids, slice$id), sep = "_"), 1)
            }
          })

          new_slices_all <- c(.self$all_slices(), new_slices)
          attr(new_slices_all, "mapping") <- old_mapping
          .self$all_slices(new_slices_all)
        }

        invisible(.self)
      })
    },
    slices_get = function(module_label) {
      if (missing(module_label)) {
        .self$all_slices()
      } else {
        module_ids <- unlist(attr(.self$all_slices(), "mapping")[c(module_label, "global_filters")])
        Filter(
          function(slice) slice$id %in% module_ids,
          .self$all_slices()
        )
      }
    },
    slices_set = function(slices) {
      shiny::isolate({
        if (!is.teal_slices(slices)) {
          slices <- as.teal_slices(slices)
        }
        .self$all_slices(slices)
        invisible(.self)
      })
    },
    show = function() {
      shiny::isolate(print(.self$all_slices()))
      invisible(.self)
    }
  )
)
