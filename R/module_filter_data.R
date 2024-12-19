#' Filter panel module in teal
#'
#' Creates filter panel module from `teal_data` object and returns `teal_data`. It is build in a way
#' that filter panel changes and anything what happens before (e.g. [`module_init_data`]) is triggering
#' further reactive events only if something has changed and if the module is visible. Thanks to
#' this special implementation all modules' data are recalculated only for those modules which are
#' currently displayed.
#'
#' @return A `eventReactive` containing `teal_data` containing filtered objects and filter code.
#' `eventReactive` triggers only if all conditions are met:
#'  - tab is selected (`is_active`)
#'  - when filters are changed (`get_filter_expr` is different than previous)
#'
#' @inheritParams module_teal_module
#' @param active_datanames (`reactive` returning `character`) this module's data names
#' @name module_filter_data
#' @keywords internal
NULL

#' @rdname module_filter_data
ui_filter_data <- function(id) {
  ns <- shiny::NS(id)
  uiOutput(ns("panel"))
}

#' @rdname module_filter_data
srv_filter_data <- function(id, datasets, active_datanames, data, is_active) {
  assert_reactive(datasets)
  moduleServer(id, function(input, output, session) {
    active_corrected <- reactive(intersect(active_datanames(), datasets()$datanames()))

    output$panel <- renderUI({
      req(inherits(datasets(), "FilteredData"))
      isolate({
        # render will be triggered only when FilteredData object changes (not when filters change)
        # technically it means that teal_data_module needs to be refreshed
        logger::log_debug("srv_filter_panel rendering filter panel.")
        if (length(active_corrected())) {
          datasets()$srv_active("filters", active_datanames = active_corrected)
          datasets()$ui_active(session$ns("filters"), active_datanames = active_corrected)
        }
      })
    })

    trigger_data <- .observe_active_filter_changed(datasets, is_active, active_corrected, data)

    eventReactive(trigger_data(), {
      .make_filtered_teal_data(modules, data = data(), datasets = datasets(), datanames = active_corrected())
    })
  })
}

#' @rdname module_filter_data
.make_filtered_teal_data <- function(modules, data, datasets = NULL, datanames) {
  data <- eval_code(
    data,
    paste0(
      ".raw_data <- list2env(list(",
      toString(sprintf("%1$s = %1$s", sapply(datanames, as.name))),
      "))\n",
      "lockEnvironment(.raw_data) # @linksto .raw_data" # this is environment and it is shared by qenvs. CAN'T MODIFY!
    )
  )
  filtered_code <- .get_filter_expr(datasets = datasets, datanames = datanames)
  filtered_teal_data <- .append_evaluated_code(data, filtered_code)
  filtered_datasets <- sapply(datanames, function(x) datasets$get_data(x, filtered = TRUE), simplify = FALSE)
  filtered_teal_data <- .append_modified_data(filtered_teal_data, filtered_datasets)
  filtered_teal_data
}

#' @rdname module_filter_data
.observe_active_filter_changed <- function(datasets, is_active, active_datanames, data) {
  previous_signature <- reactiveVal(NULL)
  filter_changed <- reactive({
    req(inherits(datasets(), "FilteredData"))
    new_signature <- c(
      teal.code::get_code(data()),
      .get_filter_expr(datasets = datasets(), datanames = active_datanames())
    )
    if (!identical(previous_signature(), new_signature)) {
      previous_signature(new_signature)
      TRUE
    } else {
      FALSE
    }
  })

  trigger_data <- reactiveVal(NULL)
  observe({
    if (isTRUE(is_active() && filter_changed())) {
      isolate({
        if (is.null(trigger_data())) {
          trigger_data(0)
        } else {
          trigger_data(trigger_data() + 1)
        }
      })
    }
  })

  trigger_data
}

#' @rdname module_filter_data
.get_filter_expr <- function(datasets, datanames) {
  if (length(datanames)) {
    teal.slice::get_filter_expr(datasets = datasets, datanames = datanames)
  } else {
    NULL
  }
}
