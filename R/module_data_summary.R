#' Data summary
#' @description
#' Module and its utils to display the number of rows and subjects in the filtered and unfiltered data.
#'
#' @details Handling different data classes:
#' `get_filter_overview()` is a pseudo S3 method which has variants for:
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
        tableOutput(ns("supported")),
        tags$details(
          tags$summary("Other datasets"),
          tableOutput(ns("unsupported"))
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

      filter_overview_table <- reactive({
        req(inherits(teal_data(), "teal_data"))
        if (!length(ls(teal.code::get_env(teal_data())))) {
          return(NULL)
        }
        get_filter_overview_wrapper(teal_data)
      })

      supported_table <- reactive({
        filter_overview <- do.call(
          smart_rbind,
          Filter(function(x) !inherits(x, "unsupported"), filter_overview_table())
        )
        if ("dataname" %in% names(filter_overview)) {
          names(filter_overview)[names(filter_overview) == "dataname"] <- "Data Name"
        }
        if ("obs" %in% names(filter_overview)) {
          names(filter_overview)[names(filter_overview) == "obs"] <- "Obs"
        }
        if ("subjects" %in% names(filter_overview)) {
          names(filter_overview)[names(filter_overview) == "subjects"] <- "Subjects"
        }
        filter_overview[is.na(filter_overview)] <- ""
        Filter(function(col) !all(col == ""), filter_overview)
      })

      unsupported_table <- reactive(
        do.call(rbind, Filter(function(x) inherits(x, "unsupported"), filter_overview_table()))
      )

      output$supported <- renderUI({
        summary_table_out <- try(supported_table(), silent = TRUE)
        if (inherits(summary_table_out, "try-error")) {
          # Ignore silent shiny error
          if (!inherits(attr(summary_table_out, "condition"), "shiny.silent.error")) {
            stop("Error occurred during data processing. See details in the main panel.")
          }
        } else if (is.null(summary_table_out)) {
          "no datasets to show"
        } else {
          body_html <- apply(
            summary_table_out,
            1,
            function(x) {
              tags$tr(
                tagList(
                  tags$td(
                    if (all(x[-1] == "")) {
                      icon(
                        name = "fas fa-exclamation-triangle",
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

          header_labels <- names(supported_table())
          header_html <- tags$tr(tagList(lapply(header_labels, tags$td)))

          table_html <- tags$table(
            class = "table custom-table",
            tags$thead(header_html),
            tags$tbody(body_html)
          )
          table_html
        }
      })

      output$unsupported <- renderTable(unsupported_table())

      filter_overview_table
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

  lapply(
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
}

#' Get filter overview
#'
#' Method to create a single entry in [`module_data_summary`]. Method returns `data.frame` containing:
#' - `dataname`: name of the dataset.
#' - `obs`: number of observations in form of a text `filtered/initial`.
#' - `subjects`: number of subjects in form of a text `filtered/initial`. Applicable when a dataset
#'   has multiple entries for a single subject.
#'
#' # Extending for other data types
#' `teal` supports data summary for `data.frame` and `MultiAssayExperiment`. Other datasets
#' are producing only `dataname` and `class` informations (they don't fail application).
#' To extend the functionality by other data type one need to create `get_filter_overview.<custom class>`.
#' New method needs to return a `data.frame` containing `dataname` column. In general, `teal` supports
#' any type of the information contained in this `data.frame` and results will be merged.
#'
#'
#' ```
#' get_filter_overview.data.frame <- function(current_data, initial_data, dataname, subject_keys) {
#'   data.frame(
#'     dataname = dataname,
#'     obs = if (!is.null(initial_data)) {
#'       sprintf("%s/%s", nrow(current_data), nrow(initial_data))
#'     } else {
#'       # when dataset is added in transform
#'       # then only current data is available
#'       nrow(current_data)
#'     }
#'   )
#' }
#' ```
#'
#'
#' @param current_data (`object`) current object (after filtering and transforming).
#' @param initial_data (`object`) initial object.
#' @param dataname (`character(1)`)
#' @return `data.frame`
get_filter_overview <- function(current_data, initial_data, dataname, subject_keys) {
  UseMethod("get_filter_overview")
}

#' @export
get_filter_overview.default <- function(current_data, initial_data, dataname, subject_keys) {
  if (inherits(current_data, c("data.frame", "DataFrame", "array", "Matrix", "SummarizedExperiment"))) {
    get_filter_overview_array(current_data, initial_data, dataname, subject_keys)
  } else if (inherits(current_data, "MultiAssayExperiment")) {
    get_filter_overview_MultiAssayExperiment(current_data, initial_data, dataname)
  } else {
    structure(
      data.frame(
        dataname = dataname,
        type = class(current_data)
      ),
      class = "unsupported"
    )
  }
}

# pseudo S3 handled in get_filter_overview.default
# Reason is to avoid registering S3 method so it is easier for external users to override
get_filter_overview_array <- function(current_data, # nolint: object_length.
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

get_filter_overview_MultiAssayExperiment <- function(current_data, # nolint: object_length, object_name.
                                                     initial_data,
                                                     dataname) {
  experiment_names <- names(initial_data)
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
  smart_rbind(mae_info, experiment_info)
}


#' Smart `rbind`
#'
#' Combine `data.frame` objects which have different columns
#'
#' @param ... (`data.frame`)
#' @examples
#' df1 <- data.frame(A = 1:3, B = letters[1:3])
#' df2 <- data.frame(A = 4:5, C = c("x", "y"))
#' df3 <- data.frame(B = letters[4:5], C = c("u", "v"))
#' smart_rbind(df1, df2, df3)
#' @keywords internal
smart_rbind <- function(...) {
  checkmate::assert_list(list(...), "data.frame")
  Reduce(
    x = list(...),
    function(x, y) {
      all_columns <- union(colnames(x), colnames(y))
      x[setdiff(all_columns, colnames(x))] <- NA
      y[setdiff(all_columns, colnames(y))] <- NA
      rbind(x, y)
    }
  )
}
