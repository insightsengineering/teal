#' Module for the right filter panel in the teal app
#' with a filter overview panel and a filter variable panel.
#'
#' This panel contains info about the number of observations left in
#' the (active) datasets and allows to filter the datasets.
#'
#' @param id module id
#' @param datanames datanames to create empty UIs for (which will be populated
#'   in the server)
#'
#' @examples
#' # Example with ADSL and ADAE dataset
#' library(random.cdisc.data)
#' library(dplyr)
#'
#' ADSL <- radsl(cached = TRUE)
#' attr(ADSL, "keys") <- get_cdisc_keys("ADSL")
#' ADAE <- radlb(cached = TRUE)
#' attr(ADAE, "keys") <- get_cdisc_keys("ADAE")
#' ADRS <- radrs(cached = TRUE)
#' attr(ADRS, "keys") <- get_cdisc_keys("ADRS")
#'
#' datasets <- teal:::FilteredData$new()
#' isolate({
#'   datasets$set_data("ADSL", ADSL)
#'   datasets$set_filter_state("ADSL", varname = NULL, list(
#'     AGE = list(range = c(33, 44), keep_na = FALSE),
#'     SEX = list(choices = "M", keep_na = TRUE)
#'   ))
#'   datasets$set_data("ADAE", ADAE)
#'   datasets$set_filter_state("ADAE", varname = NULL, list(
#'     CHG = list(range = c(20, 35), keep_na = FALSE)
#'   ))
#'   datasets$set_data("ADRS", ADRS)
#' })
#'
#' app <- shinyApp(ui = function() {
#'   tagList(
#'     include_teal_css_js(),
#'     selectInput("datanames", "Display for datasets:",
#'        choices = c("ADSL", "ADAE", "ADRS"),
#'        selected = c("ADSL", "ADAE", "ADRS"), multiple = TRUE),
#'     teal:::ui_filter_panel("filter_panel", c("ADSL", "ADAE", "ADRS"))
#'   )
#' }, server = function(input, output, session) {
#'   shinyjs::showLog()
#'   callModule(
#'     teal:::srv_filter_panel, "filter_panel", datasets,
#'     active_datanames = reactive(input$datanames)
#'   )
#' })
#' \dontrun{
#' runApp(app)
#' }
ui_filter_panel <- function(id, datanames) {
  stopifnot(
    is_character_vector(datanames)
  )

  ns <- NS(id)
  div(
    id = ns("teal_filter_panel_whole"), # used for hiding / showing

    div(
      id = ns("teal_filters_overview"), # not used, can be used to customize CSS behavior
      class = "well",
      ui_filtered_data_overview(ns("teal_filters_info")),
    ),

    div(
      id = ns("teal_filter_active_vars"), # not used, can be used to customize CSS behavior
      class = "well",
      tags$label("Active Filter Variables", class = "text-primary", style = "margin-bottom: 15px;"),
      tagList(
        lapply(datanames, function(dataname) {
          id <- ns(paste0("teal_filters_", dataname))
          # add span with same id to show / hide
          return(span(id = id, ui_filter_items(id, dataname)))
        })
      )
    ),

    div(
      id = ns("teal_filter_add_vars"), # not used, can be used to customize CSS behavior
      class = "well",
      tags$label("Add Filter Variables", class = "text-primary", style = "margin-bottom: 15px;"),
      tagList(
        lapply(datanames, function(dataname) {
          id <- ns(paste0("teal_add_", dataname, "_filter"))
          # add span with same id to show / hide
          return(span(id = id, ui_add_filter_variable(id, dataname)))
        })
      )
    )

  )
}

#' Server function for filter panel
#'
#' @md
#' @inheritParams srv_shiny_module_arguments
#' @param active_datanames `reactive` returning datanames that
#'   should be shown on the filter panel, must be a subset of the
#'   `datanames` argument provided to the UI function
#'
srv_filter_panel <- function(input, output, session, datasets, active_datanames = function() "all") {
  stopifnot(
    is(datasets, "FilteredData"),
    is.function(active_datanames)
  )

  # as the reactive is only evaluated later, we cannot reassign to the same as the constructed reactive
  # depends on the old value
  unhandled_active_datanames <- active_datanames
  active_datanames <- reactive({
    handle_active_datanames(datasets, unhandled_active_datanames())
  })

  callModule(srv_filtered_data_overview, "teal_filters_info", datasets, datanames = active_datanames)

  # use isolate because we assume that the number of datasets does not change over the course of the teal app
  # alternatively, one can proceed as in modules_filter_items to dynamically insert, remove UIs
  isol_datanames <- list_adsl_first(isolate(datasets$datanames()))
  # should not use for-loop as variables are otherwise only bound by reference and last dataname would be used
  lapply(
    isol_datanames,
    function(dataname) callModule(srv_filter_items, paste0("teal_filters_", dataname), datasets, dataname)
  )

  lapply(
    isol_datanames,
    function(dataname) callModule(
      srv_add_filter_variable, paste0("teal_add_", dataname, "_filter"),
      datasets, dataname,
      omit_vars = reactive(if (dataname == "ADSL") character(0) else names(datasets$get_data("ADSL", filtered = FALSE)))
    )
  )

  # we keep anything that may be selected to add (happens when the variable is not available for filtering)
  # lapply(isol_datanames, function(dataname) paste0("teal_add_", dataname, "_filter")) #nolintr
  setBookmarkExclude(names = c(
    # these will be regenerated dynamically
    lapply(isol_datanames, function(dataname) paste0("teal_filters_", dataname))
  ))

  # rather than regenerating the UI dynamically for the dataset filtering,
  # we instead choose to hide/show the elements
  # the filters for this dataset are just hidden from the UI, but still applied
  observeEvent(active_datanames(), {
    if (length(active_datanames()) == 0) {
      # hide whole module UI
      shinyjs::hide("teal_filter_panel_whole")
    } else {
      shinyjs::show("teal_filter_panel_whole")

      # selectively hide / show to only show `active_datanames` out of all datanames
      lapply(
        datasets$datanames(),
        function(dataname) {
          id_add_filter <- paste0("teal_add_", dataname, "_filter")
          id_filter_dataname <- paste0("teal_filters_", dataname)
          if (dataname %in% active_datanames()) {
            # shinyjs takes care of the namespace around the id
            shinyjs::show(id_add_filter)
            shinyjs::show(id_filter_dataname)
          } else {
            shinyjs::hide(id_add_filter)
            shinyjs::hide(id_filter_dataname)
          }
        }
      )
    }
  }, ignoreNULL = FALSE)

}
