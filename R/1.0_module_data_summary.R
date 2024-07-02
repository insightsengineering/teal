ui_data_summary = function(id) {
  ns <- NS(id)
  tags$div(
    id = id, # not used, can be used to customize CSS behavior
    class = "well",
    tags$div(
      class = "row",
      tags$div(
        class = "col-sm-9",
        tags$label("Active Filter Summary", class = "text-primary mb-4")
      ),
      tags$div(
        class = "col-sm-3",
        actionLink(
          ns("minimise_filter_overview"),
          label = NULL,
          icon = icon("angle-down", lib = "font-awesome"),
          title = "Minimise panel",
          class = "remove pull-right"
        )
      )
    ),
    tags$div(
      id = ns("filters_overview_contents"),
      tags$div(
        class = "teal_active_summary_filter_panel",
        tableOutput(ns("table"))
      )
    )
  )
}

#' @description
#' Server function to display the number of records in the filtered and unfiltered data.
#'
#' @param id (`character(1)`)
#'   `shiny` module instance id.
#' @param active_datanames (`character`) names of datasets
#' @inheritParams module_tabs_with_filters
#' @return `NULL`.
srv_data_summary = function(id, datasets, active_datanames) {

  moduleServer(
    id = id,
    function(input, output, session) {
      logger::log_trace("srv_data_summary initializing")

      observeEvent(input$minimise_filter_overview, {
        shinyjs::toggle("filters_overview_contents")
        toggle_icon(session$ns("minimise_filter_overview"), c("fa-angle-right", "fa-angle-down"))
        toggle_title(session$ns("minimise_filter_overview"), c("Restore panel", "Minimise Panel"))
      })

      output$table <- renderUI({
        logger::log_trace("srv_data_summary updating counts")
        if (length(active_datanames) == 0) {
          return(NULL)
        }

        datasets_df <- get_filter_overview(datanames = active_datanames, datasets = datasets)

        attr(datasets_df$dataname, "label") <- "Data Name"

        if (!is.null(datasets_df$obs)) {
          # some datasets (MAE colData) doesn't return obs column
          datasets_df <- transform(
            datasets_df,
            obs_str_summary = ifelse(
              !is.na(obs),
              sprintf("%s/%s", obs_filtered, obs),
              ""
            )
          )
          attr(datasets_df$obs_str_summary, "label") <- "Obs"
        }


        if (!is.null(datasets_df$subjects)) {
          # some datasets (without keys) doesn't return subjects
          datasets_df <- transform(
            datasets_df,
            subjects_summary = ifelse(
              !is.na(subjects),
              sprintf("%s/%s", subjects_filtered, subjects),
              ""
            )
          )
          attr(datasets_df$subjects_summary, "label") <- "Subjects"
        }

        all_names <- c("dataname", "obs_str_summary", "subjects_summary")
        datasets_df <- datasets_df[, colnames(datasets_df) %in% all_names]

        body_html <- apply(
          datasets_df,
          1,
          function(x) {
            tags$tr(
              tagList(
                tags$td(
                  if (all(x[-1] == "")) {
                    icon(
                      name = "exclamation-triangle",
                      title = "Unsupported dataset",
                      `data-container` = "body",
                      `data-toggle` = "popover",
                      `data-content` = "object not supported by the filter panel"
                    )
                  },
                  x[1]
                ),
                lapply(x[-1], tags$td)
              )
            )
          }
        )

        header_labels <- vapply(
          seq_along(datasets_df),
          function(i) {
            label <- attr(datasets_df[[i]], "label")
            ifelse(!is.null(label), label, names(datasets_df)[[i]])
          },
          character(1)
        )
        header_html <- tags$tr(tagList(lapply(header_labels, tags$td)))

        table_html <- tags$table(
          class = "table custom-table",
          tags$thead(header_html),
          tags$tbody(body_html)
        )
        logger::log_trace("srv_data_summary updated counts")
        table_html
      })
      logger::log_trace("srv_data_summary initialized")
      NULL
    }
  )
}

get_filter_overview = function(datanames, datasets) {
  rows <- lapply(
    datanames,
    function(dataname) {
      get_filter_overview(get_filtered_dataset(dataname, datasets))
    }
  )
  unssuported_idx <- vapply(rows, function(x) all(is.na(x[-1])), logical(1))
  dplyr::bind_rows(c(rows[!unssuported_idx], rows[unssuported_idx]))
}

get_filtered_dataset = function(dataname = character(0), datasets) {
  if (length(dataname) == 0) {
    datasets
  } else {
    datasets$get_data(dataname, filtered = TRUE)
  }
}
