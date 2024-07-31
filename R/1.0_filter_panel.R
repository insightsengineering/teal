#' Filter panel module in teal
#'
#' Creates filter panel module from `teal_data` object and returns `teal_data`. It is build in a way
#' that filter panel changes and anything what happens before (e.g. [`module_data`]) is triggering
#' further reactive events only if something has changed and if the module is visible. Thanks to
#' this special implementation all modules' data are recalculated only for those modules which are
#' currently displayed.
#'
#' @return A `eventReactive` which triggers only if all conditions are met:
#'  - tab is selected (`is_active`)
#'  - when filters are changed (`get_filter_expr` is different than previous)
#'
#' @inheritParams module_teal_module
#' @param active_datanames (`reactive` returning `character`) this module's data names
#' @name module_filter_panel
#' @keywords internal
NULL

#' @keywords internal
#' @rdname module_filter_panel
ui_filter_panel <- function(id) {
  ns <- shiny::NS(id)
  uiOutput(ns("panel"))
}

#' @keywords internal
#' @rdname module_filter_panel
srv_filter_panel <- function(id, datasets, active_datanames, data_rv, is_active) {
  checkmate::assert_class(datasets, "reactive")
  moduleServer(id, function(input, output, session) {
    output$panel <- renderUI({
      req(inherits(datasets(), "FilteredData"))
      isolate({
        # render will be triggered only when FilteredData object changes (not when filters change)
        # technically it means that teal_data_module needs to be refreshed
        logger::log_debug("srv_filter_panel rendering filter panel.")
        filtered_data <- datasets()
        filtered_data$srv_active("filters", active_datanames = active_datanames)
        # todo: make sure to bump the `teal.slice` version. Please use the branch `669_insertUI@main` in `teal.slice`.
        filtered_data$ui_active(session$ns("filters"), active_datanames = active_datanames)
      })
    })

    trigger_data <- .observe_active_filter_changed(datasets, is_active, active_datanames, data_rv)

    eventReactive(trigger_data(), {
      .make_filtered_teal_data(modules, data = data_rv(), datasets = datasets(), datanames = active_datanames())
    })
  })
}

#' @keywords internal
#' @rdname module_filter_panel
.make_filtered_teal_data <- function(modules, data, datasets = NULL, datanames) {
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
  raw_data_code <- sprintf("%1$s_raw <- %1$s", datanames)
  filter_code <- get_filter_expr(datasets = datasets, datanames = datanames)

  all_code <- paste(unlist(c(data_code, raw_data_code, "", filter_code)), collapse = "\n")
  tdata <- do.call(
    teal.data::teal_data,
    c(
      list(code = trimws(all_code, which = "right")),
      list(join_keys = teal.data::join_keys(data)),
      new_datasets
    )
  )
  tdata@verified <- data@verified
  # we want to keep same datanames that app dev initially set with respect to new teal_data's @env
  teal.data::datanames(tdata) <- intersect(teal.data::datanames(data), teal_data_ls(tdata))
  tdata
}

#' @rdname module_filter_panel
#' @keywords internal
.observe_active_filter_changed <- function(datasets, is_active, active_datanames, data_rv) {
  previous_signature <- reactiveVal(NULL)
  filter_changed <- reactive({
    req(inherits(datasets(), "FilteredData"))
    new_signature <- c(
      get_code(data_rv()),
      get_filter_expr(datasets = datasets(), datanames = active_datanames())
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
