ui_add_filter_variable <- function(id, dataname) {

  ns <- NS(id)

  div( class = paste0("teal_filter_", dataname),
       selectInput(ns("variables"), label=dataname , choices=NULL),
       uiOutput(ns("warning"))
  )


  #    actionButton(ns("add"), label=NULL, icon=icon("plus"))

}



srv_add_filter_variable <- function(input, output, session, datasets, dataname, omit_vars=NULL) {


  observe({
    fs <- datasets$get_filter_state(dataname, reactive = TRUE)
    df <- datasets$get_data(dataname, filtered = FALSE, reactive = TRUE)

    choices <- if (is.null(df)) {
      NULL
    } else if (is.null(fs)) {
      setdiff(names(df), omit_vars)
    } else {
      setdiff(names(df), c(names(fs), omit_vars))
    }

    .log("update add filter variables", dataname)
    updateSelectInput(session, "variables", choices = c("", choices), selected=NULL)
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
        warning_messages$varinfo <- paste("variable", paste(dataname, var, sep="."), "can currently not be used as a filter variable.")
      }
      warning_messages$i <- warning_messages$i + 1
    }
  })

  output$warning <- renderUI({
    warning_messages$i
    msg <- warning_messages$varinfo

    if (is.null(msg)  || msg == "" ) {
      div(style="display: none;")
    } else {
      div(class="text-warning", style="margin-bottom: 15px;", msg)
    }
  })

  NULL
}
