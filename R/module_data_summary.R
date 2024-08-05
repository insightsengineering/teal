#' Data summary
#' @description
#' Module and its utils to display the number of rows and subjects in the filtered and unfiltered data.
#'
#' @details Handling different data classes:
#' `get_object_filter_overview()` is a pseudo S3 method which has variants for:
#' - `array` (`data.frame`, `DataFrame`, `array`, `Matrix` and `SummarizedExperiment`): Method variant
#' can be applied to any two-dimensional objects on which [ncol()] can be used.
#' - `MultiAssayExperiment`: for which summary contains counts for `colData` and all `experiments`.
#'
#' @param id (`character(1)`)
#'  `shiny` module instance id.
#' @param teal_data (`reactive` returning `teal_data`)
#'
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
        tags$i(
          class = "remove pull-right fa fa-angle-down",
          style = "cursor: pointer;",
          title = "fold/expand data summary panel",
          onclick = sprintf("togglePanelItem(this, '%s', 'fa-angle-right', 'fa-angle-down');", content_id)
        )
      )
    ),
    tags$div(
      id = content_id,
      tags$div(
        class = "teal_active_summary_filter_panel",
        tableOutput(ns("table"))
      )
    )
  )
}

#' @rdname module_data_summary
srv_data_summary <- function(id, teal_data) {
  checkmate::check_class(teal_data, "reactive")
  moduleServer(
    id = id,
    function(input, output, session) {
      logger::log_debug("srv_data_summary initializing")

      summary_table <- reactive({
        req(inherits(teal_data(), "teal_data"))

        if (length(teal.data::datanames(teal_data())) == 0) {
          return(NULL)
        }

        filter_overview <- get_filter_overview(teal_data)
        names(filter_overview)[[1]] <- "Data Name"

        filter_overview$Obs <- ifelse(
          !is.na(filter_overview$obs),
          sprintf("%s/%s", filter_overview$obs_filtered, filter_overview$obs),
          ifelse(!is.na(filter_overview$obs_filtered), sprintf("%s", filter_overview$obs_filtered), "")
        )

        filter_overview$Subjects <- ifelse(
          !is.na(filter_overview$subjects),
          sprintf("%s/%s", filter_overview$subjects_filtered, filter_overview$subjects),
          ""
        )

        filter_overview <- filter_overview[, colnames(filter_overview) %in% c("Data Name", "Obs", "Subjects")]
        Filter(function(col) !all(col == ""), filter_overview)
      })

      output$table <- renderUI({
        body_html <- apply(
          summary_table(),
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
                      `data-content` = "object not supported by the data_summary module"
                    )
                  },
                  x[1]
                ),
                lapply(x[-1], tags$td)
              )
            )
          }
        )

        header_labels <- names(summary_table())
        header_html <- tags$tr(tagList(lapply(header_labels, tags$td)))

        table_html <- tags$table(
          class = "table custom-table",
          tags$thead(header_html),
          tags$tbody(body_html)
        )
        table_html
      })

      summary_table # testing purpose
    }
  )
}

#' @rdname module_data_summary
get_filter_overview <- function(teal_data) {
  datanames <- teal.data::datanames(teal_data())
  joinkeys <- teal.data::join_keys(teal_data())
  filtered_data_objs <- sapply(
    datanames, function(name) teal.code::get_env(teal_data())[[name]],
    simplify = FALSE
  )
  unfiltered_data_objs <- sapply(
    datanames, function(name) teal.code::get_env(teal_data())[[paste0(name, "_raw")]],
    simplify = FALSE
  )

  rows <- lapply(
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
      get_object_filter_overview(
        filtered_data = filtered_data_objs[[dataname]],
        unfiltered_data = unfiltered_data_objs[[dataname]],
        dataname = dataname,
        subject_keys = subject_keys
      )
    }
  )

  unssuported_idx <- vapply(rows, function(x) all(is.na(x[-1])), logical(1)) # this is mainly for vectors
  do.call(rbind, c(rows[!unssuported_idx], rows[unssuported_idx]))
}

#' @rdname module_data_summary
#' @param filtered_data (`list`) of filtered objects
#' @param unfiltered_data (`list`) of unfiltered objects
#' @param dataname (`character(1)`)
get_object_filter_overview <- function(filtered_data, unfiltered_data, dataname, subject_keys) {
  if (inherits(filtered_data, c("data.frame", "DataFrame", "array", "Matrix", "SummarizedExperiment"))) {
    get_object_filter_overview_array(filtered_data, unfiltered_data, dataname, subject_keys)
  } else if (inherits(filtered_data, "MultiAssayExperiment")) {
    get_object_filter_overview_MultiAssayExperiment(filtered_data, unfiltered_data, dataname)
  } else {
    data.frame(
      dataname = dataname,
      obs = NA,
      obs_filtered = NA,
      subjects = NA,
      subjects_filtered = NA
    )
  }
}

#' @rdname module_data_summary
get_object_filter_overview_array <- function(filtered_data, # nolint: object_length.
                                             unfiltered_data,
                                             dataname,
                                             subject_keys) {
  if (length(subject_keys) == 0) {
    data.frame(
      dataname = dataname,
      obs = ifelse(!is.null(nrow(unfiltered_data)), nrow(unfiltered_data), NA),
      obs_filtered = nrow(filtered_data),
      subjects = NA,
      subjects_filtered = NA
    )
  } else {
    data.frame(
      dataname = dataname,
      obs = ifelse(!is.null(nrow(unfiltered_data)), nrow(unfiltered_data), NA),
      obs_filtered = nrow(filtered_data),
      subjects = nrow(unique(unfiltered_data[subject_keys])),
      subjects_filtered = nrow(unique(filtered_data[subject_keys]))
    )
  }
}

#' @rdname module_data_summary
get_object_filter_overview_MultiAssayExperiment <- function(filtered_data, # nolint: object_length, object_name.
                                                            unfiltered_data,
                                                            dataname) {
  experiment_names <- names(unfiltered_data)
  mae_info <- data.frame(
    dataname = dataname,
    obs = NA,
    obs_filtered = NA,
    subjects = nrow(unfiltered_data@colData),
    subjects_filtered = nrow(filtered_data@colData)
  )

  experiment_obs_info <- do.call("rbind", lapply(
    experiment_names,
    function(experiment_name) {
      transform(
        get_object_filter_overview(
          filtered_data[[experiment_name]],
          unfiltered_data[[experiment_name]],
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
        subjects = get_experiment_keys(filtered_data, unfiltered_data[[experiment_name]]),
        subjects_filtered = get_experiment_keys(filtered_data, filtered_data[[experiment_name]])
      )
    }
  ))

  experiment_info <- cbind(experiment_obs_info[, c("dataname", "obs", "obs_filtered")], experiment_subjects_info)
  rbind(mae_info, experiment_info)
}
