#' Data summary
#' @description
#' Module and its utils to display the number of rows and subjects in the filtered and unfiltered data.
#'
#' @details Handling different data classes:
#' `get_filter_overview()` is a pseudo S3 method which has variants for:
#' - `array` (`data.frame`, `DataFrame`, `array`, `Matrix` and `SummarizedExperiment`): Method variant
#' can be applied to any two-dimensional objects on which [ncol()] can be used.
#' - `MultiAssayExperiment`: for which summary contains counts for `colData` and all `experiments`.
#' - For other data types module displays data name with warning icon and no more details.
#'
#' Module includes also "Show/Hide unsupported" button to toggle rows of the summary table
#' containing datasets where number of observations are not calculated.
#'
#' @param id (`character(1)`) `shiny` module instance id.
#' @param teal_data (`reactive` returning `teal_data`)
#'
#' @name module_data_summary
#' @rdname module_data_summary
#' @keywords internal
#' @return `NULL`.
NULL

#' @rdname module_data_summary
ui_data_summary <- function(id) {
  ns <- NS(id)
  content_id <- ns("filters_overview_contents")
  tags$div(
    id = id,
    class = "well",
    tags$div(
      class = "row",
      tags$div(
        class = "col-sm-9",
        tags$label("Active Filter Summary", class = "text-primary mb-4")
      ),
      tags$div(
        class = "col-sm-3",
        tags$i(
          class = "remove pull-right fa fa-angle-down",
          style = "cursor: pointer;",
          title = "fold/expand data summary panel",
          onclick = sprintf("togglePanelItems(this, '%s', 'fa-angle-right', 'fa-angle-down');", content_id)
        )
      )
    ),
    tags$div(
      id = content_id,
      tags$div(
        class = "teal_active_summary_filter_panel",
        tableOutput(ns("table")),
        actionLink(
          ns("toggle_unsupported"),
          label = "Show unsupported"
        )
      )
    )
  )
}

#' @rdname module_data_summary
srv_data_summary <- function(id, teal_data) {
  assert_reactive(teal_data)
  moduleServer(
    id = id,
    function(input, output, session) {
      logger::log_debug("srv_data_summary initializing")

      summary_table <- reactive({
        req(inherits(teal_data(), "teal_data"))
        if (!length(ls(teal.code::get_env(teal_data())))) {
          return(NULL)
        }
        get_filter_overview_wrapper(teal_data)
      })

      observeEvent(summary_table(), {
        any_unsupported <- any(apply(summary_table(), 1, function(x) all(is.na(x[-1]))))
        if (any_unsupported) {
          shinyjs::show("toggle_unsupported")
        } else {
          shinyjs::hide("toggle_unsupported")
        }
      })

      hide_unsupported <- reactive(isTRUE(input$toggle_unsupported %% 2 == 0))
      observeEvent(hide_unsupported(), {
        if (hide_unsupported()) {
          updateActionLink(inputId = "toggle_unsupported", label = "Show unsupported")
        } else {
          updateActionLink(inputId = "toggle_unsupported", label = "Hide unsupported")
        }
      })

      output$table <- renderUI({
        summary_table_out <- try(summary_table(), silent = TRUE)
        if (inherits(summary_table_out, "try-error")) {
          # Ignore silent shiny error
          if (!inherits(attr(summary_table_out, "condition"), "shiny.silent.error")) {
            stop("Error occurred during data processing. See details in the main panel.")
          }
        } else if (is.null(summary_table_out)) {
          "no datasets to show"
        } else {
          summary_table_out[is.na(summary_table_out)] <- ""
          unsupported_icon <- icon(
            name = "fas fa-exclamation-triangle",
            title = "Unsupported dataset",
            `data-container` = "body",
            `data-toggle` = "popover",
            `data-content` = "object not supported by the data_summary module"
          )
          body_html <- apply(
            summary_table_out,
            1,
            function(x) {
              is_supported <- !all(x[-1] == "")
              elem <- tags$tr(
                tagList(
                  tags$td(
                    if (!is_supported) unsupported_icon,
                    x[1]
                  ),
                  lapply(x[-1], tags$td)
                )
              )
              if (!is_supported && hide_unsupported()) {
                shinyjs::hidden(elem)
              } else {
                elem
              }
            }
          )

          header_labels <- tools::toTitleCase(names(summary_table_out))
          header_labels[header_labels == "Dataname"] <- "Data Name"
          header_html <- tags$tr(tagList(lapply(header_labels, tags$td)))

          table_html <- tags$table(
            class = "table custom-table",
            tags$thead(header_html),
            tags$tbody(body_html)
          )
          table_html
        }
      })

      NULL
    }
  )
}

#' @rdname module_data_summary
get_filter_overview_wrapper <- function(teal_data) {
  datanames <- teal.data::datanames(teal_data())
  joinkeys <- teal.data::join_keys(teal_data())

  current_data_objs <- sapply(
    datanames,
    function(name) teal.code::get_var(teal_data(), name),
    simplify = FALSE
  )
  initial_data_objs <- teal.code::get_var(teal_data(), ".raw_data")

  out <- lapply(
    datanames,
    function(dataname) {
      parent <- teal.data::parent(joinkeys, dataname)
      # todo: what should we display for a parent dataset?
      #     - Obs and Subjects
      #     - Obs only
      #     - Subjects only
      # todo (for later): summary table should be displayed in a way that child datasets
      #       are indented under their parent dataset to form a tree structure
      subject_keys <- if (length(parent) > 0) {
        names(joinkeys[dataname, parent])
      } else {
        joinkeys[dataname, dataname]
      }
      get_filter_overview(
        current_data = current_data_objs[[dataname]],
        initial_data = initial_data_objs[[dataname]],
        dataname = dataname,
        subject_keys = subject_keys
      )
    }
  )

  do.call(.smart_rbind, out)
}


#' @rdname module_data_summary
#' @param current_data (`object`) current object (after filtering and transforming).
#' @param initial_data (`object`) initial object.
#' @param dataname (`character(1)`)
#' @param subject_keys (`character`) names of the columns which determine a single unique subjects
get_filter_overview <- function(current_data, initial_data, dataname, subject_keys) {
  if (inherits(current_data, c("data.frame", "DataFrame", "array", "Matrix", "SummarizedExperiment"))) {
    get_filter_overview_array(current_data, initial_data, dataname, subject_keys)
  } else if (inherits(current_data, "MultiAssayExperiment")) {
    get_filter_overview_MultiAssayExperiment(current_data, initial_data, dataname)
  } else {
    data.frame(dataname = dataname)
  }
}

#' @rdname module_data_summary
get_filter_overview_array <- function(current_data,
                                      initial_data,
                                      dataname,
                                      subject_keys) {
  if (length(subject_keys) == 0) {
    data.frame(
      dataname = dataname,
      obs = if (!is.null(initial_data)) {
        sprintf("%s/%s", nrow(current_data), nrow(initial_data))
      } else {
        nrow(current_data)
      }
    )
  } else {
    data.frame(
      dataname = dataname,
      obs = if (!is.null(initial_data)) {
        sprintf("%s/%s", nrow(current_data), nrow(initial_data))
      } else {
        nrow(current_data)
      },
      subjects = if (!is.null(initial_data)) {
        sprintf("%s/%s", nrow(unique(current_data[subject_keys])), nrow(unique(initial_data[subject_keys])))
      } else {
        nrow(unique(current_data[subject_keys]))
      }
    )
  }
}

#' @rdname module_data_summary
get_filter_overview_MultiAssayExperiment <- function(current_data, # nolint: object_length, object_name.
                                                     initial_data,
                                                     dataname) {
  experiment_names <- names(current_data)
  mae_info <- data.frame(
    dataname = dataname,
    subjects = if (!is.null(initial_data)) {
      sprintf("%s/%s", nrow(current_data@colData), nrow(initial_data@colData))
    } else {
      nrow(current_data@colData)
    }
  )

  experiment_obs_info <- do.call("rbind", lapply(
    experiment_names,
    function(experiment_name) {
      transform(
        get_filter_overview(
          current_data[[experiment_name]],
          initial_data[[experiment_name]],
          dataname = experiment_name,
          subject_keys = join_keys() # empty join keys
        ),
        dataname = paste0(" - ", experiment_name)
      )
    }
  ))

  get_experiment_keys <- function(mae, experiment) {
    sample_subset <- mae@sampleMap[mae@sampleMap$colname %in% colnames(experiment), ]
    length(unique(sample_subset$primary))
  }

  experiment_subjects_info <- do.call("rbind", lapply(
    experiment_names,
    function(experiment_name) {
      data.frame(
        subjects = if (!is.null(initial_data)) {
          sprintf(
            "%s/%s",
            get_experiment_keys(current_data, current_data[[experiment_name]]),
            get_experiment_keys(current_data, initial_data[[experiment_name]])
          )
        } else {
          get_experiment_keys(current_data, current_data[[experiment_name]])
        }
      )
    }
  ))

  experiment_info <- cbind(experiment_obs_info, experiment_subjects_info)
  .smart_rbind(mae_info, experiment_info)
}
