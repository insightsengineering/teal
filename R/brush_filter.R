# todo: this can't be in teal - it should be in teal.widgets, but... teal.widgets doesn't depend
#       on teal.slice nor teal.transform. It is a mess and there is no easy solution to this now.
#' @export
ui_brush_filter <- function(id) {
  ns <- NS(id)
  div(
    tags$h1(id = ns("title"), tags$strong("Selected points:"), class = "text-center font-150p"),
    teal.widgets::get_dt_rows(ns("data_table"), ns("data_table_rows")),
    actionButton(ns("apply_brush_filter"), "Apply filter"),
    DT::dataTableOutput(ns("data_table"), width = "100%")
  )
}

#' @export
srv_brush_filter <- function(id, brush, dataset, filter_panel_api, selectors = list(), table_dec = 4) {
  moduleServer(id, function(input, output, session) {
    # selector_list <- isolate(selectors())

    observeEvent(brush(), ignoreNULL = FALSE, {
      if (is.null(brush())) {
        shinyjs::hide("title")
        shinyjs::hide("data_table")
      } else {
        shinyjs::show("title")
        shinyjs::show("data_table")
      }
    })

    brushed_table <- reactive({
      req(brush())
      if (is.null(brush())) {
        return(NULL)
      }
      teal.widgets::clean_brushedPoints(isolate(dataset()), brush())
    })

    output$data_table <- DT::renderDataTable(server = TRUE, {
      brushed_df <- req(brushed_table())
      if (is.null(brushed_df)) {
        return(NULL)
      }
      numeric_cols <- names(brushed_df)[
        vapply(brushed_df, function(x) is.numeric(x) && !is.integer(x), FUN.VALUE = logical(1))
      ]
      if (length(numeric_cols) > 0) {
        DT::formatRound(
          DT::datatable(brushed_df,
            rownames = FALSE,
            options = list(scrollX = TRUE, pageLength = input$data_table_rows)
          ),
          numeric_cols,
          table_dec
        )
      } else {
        DT::datatable(brushed_df, rownames = FALSE, options = list(scrollX = TRUE, pageLength = input$data_table_rows))
      }
    })

    observeEvent(input$data_table_rows_selected, ignoreNULL = FALSE, {
      if (is.null(input$data_table_rows_selected)) {
        shinyjs::hide("apply_brush_filter")
      } else {
        shinyjs::show("apply_brush_filter")
      }
    })

    observeEvent(input$apply_brush_filter, {
      if (is.null(input$data_table_rows_selected)) {
        return(NULL)
      }
      # isolate({
      #   foo1(brush, selector_list)
      # })
      brushed_df <- brushed_table()[input$data_table_rows_selected, ]
      # todo: when added another time then it is duplicated
      slice <- teal_slices(teal_slice(
        dataname = "ADSL",
        varname = "USUBJID",
        selected = unique(brushed_df$USUBJID), # todo: this needs to be parametrised or based on join_keys
        id = "brush_filter"
      ))
      shinyjs::hide("apply_brush_filter")
      set_filter_state(filter_panel_api, slice)
    })
  })
}

#' get axis dataname, varname and ranges
foo1 <- function(brush, selector_list) {
  lapply(names(brush()$mapping), function(selector) {
    list(
      dataname = selector_list[[selector]]()$dataname,
      varname = brush()$mapping[[selector]],
      values = unlist(brush()[paste0(selector, c("min", "max"))])
    )
  })
}
