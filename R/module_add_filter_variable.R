#' UI to select among the column names of a dataset to add as a filter variable
#'
#' Once something is selected, it puts it to the top right panel, so you can adjust
#' filtering for that variable. The selection is then undone, so other columns can
#' be added for filtering.
#'
#' @param id module id
#' @param dataname name of dataset whose columns should be filtered
#'
#' @importFrom shinyWidgets pickerOptions
ui_add_filter_variable <- function(id, dataname) {

  ns <- NS(id)

  div(
    id = ns(character(0)), # needed to properly show / hide filters
    optionalSelectInput(
      ns("variables"),
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
  # todo: this force is bad style and not needed (conceptually bad)
  # have to force arguments
  force(datasets)
  force(dataname)
  force(omit_vars)

  observe({
    fs <- datasets$get_filter_state(dataname)
    df <- datasets$get_data(dataname, filtered = FALSE)

    # names(NULL) is NULL
    vars <- setdiff(names(df), c(names(fs), omit_vars))
    vars <- if_not_empty(vars, c("", vars))
    choices <- variable_choices(df, vars)

    .log("update add filter variables", dataname)
    updateOptionalSelectInput(
      session,
      "variables",
      choices = choices,
      selected = NULL
    )
  })

  # todo: what is i needed for
  warning_messages <- reactiveValues(varinfo = "", i = 0)

  observeEvent(input$variables, {

    var <- input$variables
    df <- datasets$get_data(dataname)

    validate(need(var, "need valid variable"))

    .log("add filter variable", var)

    if (var %in% names(df)) {
      if (datasets$get_filter_type(dataname, var) != "unknown") {
        # todo: rather than calling set_default_filter_state, call set_filter_state and get th default filter from thee add_filter_variable module
        datasets$set_default_filter_state(dataname, var)
        warning_messages$varinfo <- ""
      } else {
        warning_messages$varinfo <- paste(
          "variable",
          paste(dataname, var, sep = "."),
          "can't be currently used as a filter variable."
        )
      }
      warning_messages$i <- warning_messages$i + 1
    }
  })

  output$warning <- renderUI({
    warning_messages$i
    msg <- warning_messages$varinfo

    if (is.null(msg)  || msg == "") {
      div(style = "display: none;")
    } else {
      div(class = "text-warning", style = "margin-bottom: 15px;", msg)
    }
  })

  NULL
}
