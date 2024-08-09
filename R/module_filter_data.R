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
srv_filter_data <- function(id, datasets, active_datanames, data_rv, is_active) {
  checkmate::assert_class(datasets, "reactive")
  moduleServer(id, function(input, output, session) {
    output$panel <- renderUI({
      req(inherits(datasets(), "FilteredData"))
      isolate({
        # render will be triggered only when FilteredData object changes (not when filters change)
        # technically it means that teal_data_module needs to be refreshed
        logger::log_debug("srv_filter_panel rendering filter panel.")
        if (length(active_datanames())) {
          datasets()$srv_active("filters", active_datanames = active_datanames)
          # todo: make sure to bump the `teal.slice` version. Please use the branch `669_insertUI@main` in `teal.slice`.
          datasets()$ui_active(session$ns("filters"), active_datanames = active_datanames)
        }
      })
    })

    trigger_data <- .observe_active_filter_changed(datasets, is_active, active_datanames, data_rv)

    eventReactive(trigger_data(), {
      .make_filtered_teal_data(modules, data = data_rv(), datasets = datasets(), datanames = active_datanames())
    })
  })
}

#' @rdname module_filter_data
.make_filtered_teal_data <- function(modules, data, datasets = NULL, datanames) {
  data <- eval_code(data, sprintf("%1$s._raw_ <- %1$s", datanames))
  filtered_code <- teal.slice::get_filter_expr(datasets = datasets, datanames = datanames)
  filtered_teal_data <- .append_evaluated_code(data, filtered_code)
  filtered_datasets <- sapply(datanames, function(x) datasets$get_data(x, filtered = TRUE), simplify = FALSE)
  filtered_teal_data <- .append_modified_data(filtered_teal_data, filtered_datasets)
  filtered_teal_data
}

#' @rdname module_filter_data
.observe_active_filter_changed <- function(datasets, is_active, active_datanames, data_rv) {
  previous_signature <- reactiveVal(NULL)
  filter_changed <- reactive({
    req(inherits(datasets(), "FilteredData"))
    new_signature <- c(
      teal.data::get_code(data_rv()),
      teal.slice::get_filter_expr(datasets = datasets(), datanames = active_datanames())
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
