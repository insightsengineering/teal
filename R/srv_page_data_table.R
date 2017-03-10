
srv_page_data_table <- function(input, output, session, datasets) {


  cache_selected <- list()

  observe({

    dataname <- input$dataset


    validate(
      need(dataname, "need valid dataset name"),
      need(dataname %in% datasets$datanames(), paste("data", dataname, "was not specified"))
    )

    df <- datasets$get_data(dataname, filtered = FALSE, reactive = FALSE)
    choices <- names(df)

    vo <- cache_selected[[dataname]]

    selected <- if (is.null(vo)) head(choices, 6) else intersect(vo, choices)
    choices <- c(selected, setdiff(choices, selected))

    .log("data table, update variables for", dataname)

    updateSelectInput(session, "variables",
                      choices = choices,
                      selected = selected)

    cache_selected[[dataname]] <- selected

  })


  output$tbl <- DT::renderDataTable({

    dataname <- input$dataset
    is.filtered <- input$dataraworfiltered == "filtered"
    distinct <- input$distinct


    validate(need(dataname, "need valid dataname"))

    variables <- input$variables

    validate(need(variables, "need valid variable names"))

    .log("data table update", dataname)

    df <- datasets$get_data(dataname, filtered = is.filtered, reactive = TRUE)

    validate(need(df, paste("data", dataname, "is empty")))
    validate(need(all(variables %in% names(df)), "not all selected variables exist"))

    df_s <- if (distinct) count_(df, variables) else df[,variables]

    # filter = 'top'
    datatable(
      df_s,
      options = list(
        searching = FALSE,
        pageLength = 30,
        lengthMenu = c(5, 15, 30, 100)
      )
    )

  })

}
