# sodo3: add example

#' UI to select among the column names of a dataset to add as a filter variable
#'
#' Once something is selected, it puts it to the top right panel (see `\link{module_filter_items}`),
#' so you can adjust filtering for that variable. The selection is then undone and the choices
#' is updated, so other variables can be added for filtering.
#' When a variable cannot be selected for filtering, a warning is displayed.
#'
#' Whenever a new filter is added in the lower right panel, the `FilteredData` object
#' is called to add the filter. Through reactivity, the upper panel then updates
#' itself.
#'
#' @md
#' @param id module id
#' @param dataname name of dataset whose columns should be filtered
#'
#' @importFrom shinyWidgets pickerOptions
ui_add_filter_variable <- function(id, dataname) {
  stopifnot(
    is_character_single(dataname)
  )

  ns <- NS(id)

  div(
    id = ns(character(0)), # needed to assign an id, so filter can be shown / hidden
    optionalSelectInput(
      ns("new_filter_var"),
      label = dataname,
      choices = NULL,
      options = pickerOptions(
        liveSearch = TRUE,
        noneSelectedText = "Select a variable"
      )
    ),
  )
}

srv_add_filter_variable <- function(input, output, session, datasets, dataname, omit_vars) {
  stopifnot(
    is(datasets, "FilteredData"),
    is_character_single(dataname),
    is.function(omit_vars)
  )

  # currently active filter vars for this dataset
  active_filter_vars <- reactive({
    names(datasets$get_filter_state(dataname))
  })

  # observe input$new_filter_var: update the filter state of the datasets
  # this will update active_filter_vars, which then triggers an update of the choices
  observeEvent(input$new_filter_var, {
    # if NULL, it was just reset (to select a new variable to filter); at startup, this is also called with NULL
    var_to_add <- input$new_filter_var
    if (!is.null(var_to_add)) {
      stopifnot(datasets$is_filterable(dataname, var_to_add))
      .log("add filter variable", var_to_add)
      datasets$restore_filter(dataname, varname = var_to_add)
      datasets$.__enclos_env__$private$validate() # sodo3: remove or keep in checking mode?
    }
  })

  # remove selected option from choices and set again to unselected, so a new
  # variable can be selected
  # reacts both when data changed and when active_filter_vars is updated
  observe({
    # we add this dependency here so that the choices update once the UI is set up (which triggers an event)
    # even though the new_filter_var is set to NULL with `updateOptionalSelectInput`, this only happens once all
    # observers were executed, so the above that adds it to the filtered variables still has its non-NULL value
    input$new_filter_var

    .log("updating choices to add filter variables for", dataname)
    choices <- setdiff(
      names(datasets$get_data(dataname, filtered = FALSE)),
      c(active_filter_vars(), omit_vars())
    )
    choices <- choices[
      vapply(choices, function(varname) datasets$is_filterable(dataname, varname = varname), logical(1))
    ]
    updateOptionalSelectInput(
      session,
      "new_filter_var",
      choices = choices,
      selected = NULL # unselect option
    )
  })

  return(NULL)
}
