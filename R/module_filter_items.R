#' Creates UI to show currently selected filters that can be applied to
#' the dataset, i.e. all columns selected for filtering are shown.
#'
#' @param id module id
#' @param dataname `character` name of dataset that is filtered
#' @md
#'
ui_filter_items <- function(id, dataname) {
  ns <- NS(id)
  div(
    id = ns(character(0)), # needed to assign an id, so filter can be shown / hidden
    uiOutput(ns("filters"))
  )
}

#' @importFrom shinyWidgets pickerInput pickerOptions
srv_filter_items <- function(input, output, session, datasets, dataname, container = div) {
  # This whole function is complicated because the UI must be created dynamically from the
  # selection of filters.
  # Any observers previously registered during dynamic creation must be deleted on the
  # next dynamic registration of observers.
  # When a UI element with an id is added and removed, its input element still stays there
  # When it is readded, the input elements still are the old ones before it was removed,
  # e.g. the button input starts at the number of clicks on it before it was removed.
  # When the filter is changed from the UI, this sets the state in the FilteredData.
  # This in turns triggers the UI regeneration which reflects the new state (and should
  # leave the UI in the same state because nothing changed, unless the state does not capture
  # the full UI state). This triggers the `set_filter_state` function again, but stops there
  # because the underlying reactiveVal only triggers when its value changes (unlike reactive).

  # dynamic ui part ----
  output$filters <- renderUI({
    .log("generating ui filters for data", dataname)
    filter_infos <- datasets$get_filter_info(dataname)
    var_states <- datasets$get_filter_state(dataname)

    ns <- session$ns
    return(do.call(container, unname(Map(
      function(varname, filter_info, filter_state) {
        ui_single_filter_item(
          id = ns(paste0("filter_", varname)), filter_info = filter_info, filter_state = filter_state,
          prelabel = paste0(dataname, ".", varname)
        )
      },
      varname = names(filter_infos), # also used as id
      filter_info = filter_infos,
      filter_state = var_states
    ))))
  })

  # dynamic server part for UI ----

  # we need to keep a list of current observers so we can destroy them when the UI is regenerated dynamically
  # must destroy the observer so that it does not keep deleting when the var is added again (which
  # would then immediately be deleted again)
  active_observers <- list()
  observe({
    .log("generating server part for filters for data", dataname)

    lapply(active_observers, function(o) o$destroy())
    active_observers <<- list()

    # over all filtered variables
    lapply(names(datasets$get_filter_state(dataname)), function(varname) {
      active_observers <<- c(
        active_observers,
        callModule(srv_single_filter_item, paste0("filter_", varname), datasets, dataname, varname)
      )
    })
  })

  # returning observers so you can cancel them in a similar fashion as in this module when integrating this module
  # into another dynamic module
  return(active_observers)
}
