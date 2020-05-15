# todo: provide example

#' Creates UI to show currently selected filters that can be applied to
#' the dataset, i.e. all columns selected for filtering are shown.
#'
#' @param id module id
#' @md
#'
ui_filter_items <- function(id) {
  ns <- NS(id)
  # todo: put id into top class in init_ui
  div(
    id = ns(character(0)) # needed to assign an id, so filter can be shown / hidden
  )
}

#' @importFrom shinyWidgets pickerInput pickerOptions
srv_filter_items <- function(input, output, session, datasets, dataname) {
  # todo (for Max): insert discussion about dynamic UI from chat with Adrian

  # We here choose to create a UI that listens to channges in the filter variables and
  # calls `insertUI` for any variables that were added to filter and `removeUI` for variables
  # that should no longer be filtered.
  # When we insert a UI with an associated server function, we must delete the observers
  # registered in the server function when we remove it again as they will otherwise keep
  # listening. This becomes problematic when the UI is added again. Not only will the observers
  # execute twice, but also will the input elements not be reset, e.g. a previously clicked button
  # will keep its value (equal to the number of clicks).

  # currently shown variable filters in the order they were added to the UI along
  # with the observers that need to be destroyed to properly remove this element
  shown_vars_observers <- NULL
  filtered_vars <- reactive(names(datasets$get_filter_state(dataname))) # variables to filter according to datasets state
  filter_id_for_var <- function(varname) paste0("filter_", varname)

  observeEvent(filtered_vars(), {
    # this block has an effect whenever the shown variable filters differ from the datasets state
    .log("regenerating ui filters for data", dataname)

    # add variables not shown currently
    lapply(setdiff(filtered_vars(), names(shown_vars_observers)), function(varname) {
      filter_id <- session$ns(filter_id_for_var(varname))
      insertUI(
        selector = paste0("#", session$ns(character(0))),
        where = "beforeEnd",
        # add span to be removable
        ui = span(id = filter_id, ui_single_filter_item(
          id = filter_id,
          filter_info = datasets$get_filter_info(dataname, varname),
          filter_state = datasets$get_filter_state(dataname, varname),
          prelabel = paste0(dataname, ".", varname)
        ))
        #immediate = TRUE # todo: check
      )
      shown_vars_observers <<- c(
        shown_vars_observers,
        setNames(
          list(callModule(srv_single_filter_item, filter_id_for_var(varname), datasets, dataname, varname)$observers),
          varname
        )
      )
    })
    # remove variables that should not be shown anymore
    lapply(setdiff(names(shown_vars_observers), filtered_vars()), function(varname) {
      removeUI(selector = paste0("#", session$ns(filter_id_for_var(varname))))
      lapply(shown_vars_observers[[varname]], function(obs) obs$destroy())
      shown_vars_observers[[varname]] <<- NULL
    })

    stopifnot(setequal(filtered_vars(), names(shown_vars_observers)))
  })

  # return observers so you can cancel them in a similar fashion as in this module when integrating this module
  # into another dynamic module
  return(shown_vars_observers)
}
