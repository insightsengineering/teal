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
  ns <- NS(id)

  div(
    id = ns(character(0)), # needed to properly show / hide filters
    optionalSelectInput(
      ns("new_filter_var"),
      label = dataname,
      choices = NULL,
      options = pickerOptions(
        liveSearch = TRUE,
        noneSelectedText = "Select a variable"
      )
    ),
    uiOutput(ns("warning"))
  )
}

srv_add_filter_variable <- function(input, output, session, datasets, dataname, omit_vars = NULL) {
  # currently active filter vars for this dataset
  active_filter_vars <- reactive({
    names(datasets$get_filter_state(dataname))
  })
  # warning message to display if a filter variable cannot be displayed
  warning_message <- reactiveVal(NULL)

  # observe input$new_filter_var: update the filter state of the datasets
  # this will update active_filter_vars, which then triggers an update of the choices
  observeEvent(input$new_filter_var, {
    # if NULL, it was just reset (to select a new variable to filter); at startup, this is also called with NULL
    var <- input$new_filter_var
    if (!is.null(var)) {
      if (datasets$can_be_filtered(dataname, var)) {
        warning_message(NULL) # remove previously displayed warnings
        .log("add filter variable", var)
        datasets$restore_filter(dataname, varname = var)
        datasets$validate_temp() # todo1: remove
      } else {
        warning_message <- paste(
          "variable",
          paste(dataname, var, sep = "."),
          "can't be currently used as a filter variable."
        )
      }
    }
  })
  # remove selected option from choices and set again to unselected, so a new
  # variable can be selected
  # reacts both when data changed and when active_filter_vars is updated
  observe({
    .log("updating choices to add filter variables for", dataname)
    choices <- setdiff(
      names(datasets$get_data(dataname)),
      c(active_filter_vars(), omit_vars)
    )

    updateOptionalSelectInput(
      session,
      "new_filter_var",
      choices = choices,
      selected = NULL # unselect option
    )
  })

  # display warning message
  output$warning <- renderUI({
    msg <- warning_message()

    if (is.null(msg) || msg == "") {
      div(style = "display: none;")
    } else {
      div(class = "text-warning", style = "margin-bottom: 15px;", msg)
    }
  })

  return(NULL)
}
