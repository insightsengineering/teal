#' Module for the right filtering panel in the teal app
#' with a filter info panel and a filter variable panel
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
#' })
#'
#' shinyApp(ui = function() {
#'   tagList(
#'     include_teal_css_js(),
#'     filter_panel_ui("filter_panel", c("ADSL", "ADAE"))
#'   )
#' }, server = function(input, output, session) {
#'   shinyjs::showLog()
#'   callModule(
#'     filter_panel_srv, "filter_panel", datasets,
#'     active_datanames = reactive(c("ADSL", "ADAE"))
#'   )
#' }) %>% invisible() # invisible so it does not run
filter_panel_ui <- function(id, datanames) {
  stopifnot(
    is_character_vector(datanames)
  )

  ns <- NS(id)
  div(
    # we provide these ids although we may not use them so that users
    # can customize the CSS behavior
    id = ns("teal_filter_panel"), # used for hiding / showing
    div(
      id = ns("teal_filter_active_vars"), # id not used elsewhere
      class = "well",
      tags$label("Active Filter Variables", class = "text-primary", style = "margin-bottom: 15px;"),
      tagList(
        ui_filter_info(ns("teal_filters_info"))
      ),
      tagList(
        lapply(datanames, function(dataname) {
          id <- ns(paste0("teal_filters_", dataname))
          # add span with same id to show / hide
          return(span(id = id, ui_filter_items(id, dataname)))
        })
      )
    ),
    div(
      id = ns("teal_filter_add_vars"), # id not used elsewhere
      class = "well",
      tags$label("Add Filter Variables", class = "text-primary", style = "margin-bottom: 15px;"),
      tagList(
        lapply(datanames, function(dataname) {
          id <- ns(paste0("teal_add_", dataname, "_filter"))
          # add span with same id to show / hide
          return(span(id = id, ui_add_filter_variable(id, dataname)))
        })
      ),
      p("Note that variables that cannot be filtered are excluded.")
    )
  )
}

filter_panel_srv <- function(input, output, session, datasets, active_datanames) {
  stopifnot(
    is(datasets, "FilteredData"),
    is.function(active_datanames)
  )

  callModule(srv_filter_info, "teal_filters_info", datasets, datanames = reactive(active_datanames()))

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
      shinyjs::hide("teal_filter_panel")
    } else {
      shinyjs::show("teal_filter_panel")

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
  })

}
