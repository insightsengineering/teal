#' Filter panel module in teal
#'
#' Reactive filter panel module in teal
#'
#' @inheritParams module_nested_tabs
#' @param active_datanames (`reactive` returning `character`) this module's datanames
#' @name filter_panel
#' @keywords internal
NULL

#' @rdname filter_panel
ui_filter_panel <- function(id) {
  ns <- shiny::NS(id)
  uiOutput(ns("panel"))
}

#' @rdname filter_panel
srv_filter_panel <- function(id, datasets, active_datanames, data_rv, is_active) {
  checkmate::assert_class(datasets, "reactive")
  moduleServer(id, function(input, output, session) {
    output$panel <- renderUI({
      req(inherits(datasets(), "FilteredData"))
      isolate({
        # render will be triggered only when FilteredData object changes (not when filters change)
        # technically it means that teal_data_module needs to be refreshed
        logger::log_trace("srv_filter_panel rendering filter panel.")
        filtered_data <- datasets()
        filtered_data$srv_active("filters", active_datanames = active_datanames)
        # todo: make sure to bump the `teal.slice` version. Please use the branch `669_insertUI@main` in `teal.slice`.
        filtered_data$ui_active(session$ns("filters"), active_datanames = active_datanames)
      })
    })

    trigger_data <- .observe_active_filter_changed(datasets, is_active, active_datanames)

    eventReactive(trigger_data(), {
      .make_teal_data(modules, data = data_rv(), datasets = datasets(), datanames = active_datanames())
    })
  })
}

#' @rdname filter_panel
.make_teal_data <- function(modules, data, datasets = NULL, datanames) {
  new_datasets <- c(
    # Filtered data
    sapply(datanames, function(x) datasets$get_data(x, filtered = TRUE), simplify = FALSE),
    # Raw (unfiltered data)
    stats::setNames(
      lapply(datanames, function(x) datasets$get_data(x, filtered = FALSE)),
      sprintf("%s_raw", datanames)
    )
  )

  data_code <- teal.data::get_code(data, datanames = datanames)
  hashes_code <- .get_hashes_code(datasets = datasets, datanames)
  raw_data_code <- sprintf("%1$s_raw <- %1$s", datanames)
  filter_code <- get_filter_expr(datasets = datasets, datanames = datanames)

  all_code <- paste(unlist(c(data_code, "", hashes_code, raw_data_code, "", filter_code)), collapse = "\n")
  tdata <- do.call(
    teal.data::teal_data,
    c(
      list(code = trimws(all_code, which = "right")),
      list(join_keys = teal.data::join_keys(data)),
      new_datasets
    )
  )

  tdata@verified <- data@verified
  teal.data::datanames(tdata) <- datanames
  tdata
}

#' Get code that tests the integrity of the reproducible data
#'
#' @param datasets (`FilteredData`) object holding the data
#' @param datanames (`character`) names of datasets
#'
#' @return A character vector with the code lines.
#' @keywords internal
#'
.get_hashes_code <- function(datasets = NULL, datanames) {
  # todo: this should be based on data_rv object not on datasets
  vapply(
    datanames,
    function(dataname, datasets) {
      hash <- rlang::hash(datasets$get_data(dataname, filtered = FALSE))
      sprintf(
        "stopifnot(%s == %s)",
        deparse1(bquote(rlang::hash(.(as.name(dataname))))),
        deparse1(hash)
      )
    },
    character(1L),
    datasets = datasets,
    USE.NAMES = FALSE
  )
}

#' Trigger only active module when filter is changed
#'
#' Creates a trigger to limit reactivity between filter-panel and modules. We want to recalculate
#'  only active (currently visible) modules. `trigger_data` triggers only if all conditions are met:
#'  - tab is selected (is_active)
#'  - when filters are changed (get_filter_expr is different than previous)

#' @return A `reactiveVal` which is triggered when filter is changed and this module is selected.
#' @keywords internal
.observe_active_filter_changed <- function(datasets, is_active, active_datanames) {
  previous_filter <- reactiveVal(NULL)
  filter_changed <- reactive({
    req(inherits(datasets(), "FilteredData"))
    new_filter <- get_filter_expr(datasets = datasets(), datanames = active_datanames())
    if (!identical(previous_filter(), new_filter)) {
      previous_filter(new_filter)
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
