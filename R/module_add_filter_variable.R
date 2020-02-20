#' @importFrom shinyWidgets pickerOptions
ui_add_filter_variable <- function(id, dataname) {

  ns <- NS(id)

  div(
    id = paste0("teal_filter_", dataname),
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
  # have to force arguments
  force(datasets)
  force(dataname)
  force(omit_vars)

  observe({
    fs <- datasets$get_filter_state(dataname, reactive = TRUE)
    df <- datasets$get_data(dataname, filtered = FALSE, reactive = TRUE)

    vars <- if (is.null(df)) {
      NULL
    } else if (is.null(fs)) {
      setdiff(names(df), omit_vars)
    } else {
      setdiff(names(df), c(names(fs), omit_vars))
    }
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

  warning_messages <- reactiveValues(varinfo = "", i = 0)

  observeEvent(input$variables, {

    var <- input$variables
    df <- datasets$get_data(dataname)

    validate(need(var, "need valid variable"))

    .log("add filter variable", var)

    if (var %in% names(df)) {
      if (datasets$get_filter_type(dataname, var) != "unknown") {
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
