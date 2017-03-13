
srv_add_filter_variable <- function(input, output, session, datasets, dataname) {


  observe({
    fs <- datasets$get_filter_state(dataname, reactive = TRUE)
    df <- datasets$get_data(dataname, filtered = FALSE, reactive = TRUE)

    choices <- if (is.null(df)) {
      NULL
    } else if (is.null(fs)) {
      names(df)
    } else {
      setdiff(names(df), names(fs))
    }

    .log("update add filter variables", dataname)
    updateSelectInput(session, "variables", choices = c("", choices), selected=NULL)
  })


  observeEvent(input$variables, {

   var <- input$variables
   df <- datasets$get_data(dataname)

   validate(need(var, "need valid variable"))

   .log("add filter variable", var)

   if (var %in% names(df) && datasets$get_filter_type(dataname, var) != "unknown") {
     datasets$set_default_filter_state(dataname, var)
   }


  })

}
