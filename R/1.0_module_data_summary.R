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
#' @name module_data_summary
#' @rdname module_data_summary
#' @return `NULL`.
NULL

#' @rdname module_data_summary
#' @keywords internal
ui_data_summary <- function(id) {
  ns <- NS(id)
  tags$div(
    id = id, # not used, can be used to customize CSS behavior
    class = "well",
    tags$div(
      class = "row",
      tags$div(
        class = "col-sm-9",
        tags$label("Active Filter Summary - teal", class = "text-primary mb-4")
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

#' @rdname module_data_summary
#' @keywords internal
srv_data_summary <- function(id, teal_data) {
  checkmate::check_class(teal_data, "reactive")
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
        req(inherits(teal_data(), "teal_data"))
        logger::log_trace("srv_data_summary updating counts")

        if (length(datanames(teal_data())) == 0) {
          return(NULL)
        }

        filter_overview <- get_filter_overview(teal_data)

        attr(filter_overview$dataname, "label") <- "Data Name"

        if (!is.null(filter_overview$obs)) {
          # some datasets (MAE colData) doesn't return obs column
          filter_overview <- transform(
            filter_overview,
            obs_str_summary = ifelse(
              !is.na(obs),
              sprintf("%s/%s", obs_filtered, obs),
              ""
            )
          )
          attr(filter_overview$obs_str_summary, "label") <- "Obs"
        }


        if (!is.null(filter_overview$subjects)) {
          # some datasets (without keys) doesn't return subjects
          filter_overview <- transform(
            filter_overview,
            subjects_summary = ifelse(
              !is.na(subjects),
              sprintf("%s/%s", subjects_filtered, subjects),
              ""
            )
          )
          attr(filter_overview$subjects_summary, "label") <- "Subjects"
        }

        all_names <- c("dataname", "obs_str_summary", "subjects_summary")
        filter_overview <- filter_overview[, colnames(filter_overview) %in% all_names]

        body_html <- apply(
          filter_overview,
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

        header_labels <- vapply(
          seq_along(filter_overview),
          function(i) {
            label <- attr(filter_overview[[i]], "label")
            ifelse(!is.null(label), label, names(filter_overview)[[i]])
          },
          character(1)
        )
        header_html <- tags$tr(tagList(lapply(header_labels, tags$td)))

        table_html <- tags$table(
          class = "table custom-table",
          tags$thead(header_html),
          tags$tbody(body_html)
        )
        table_html
      })
      NULL
    }
  )
}

#' @rdname module_data_summary
#' @keywords internal
get_filter_overview <- function(teal_data) {
  logger::log_trace("srv_data_overiew-get_filter_overview initialized")
  datanames <- teal.data::datanames(teal_data())
  joinkeys <- teal.data::join_keys(teal_data())
  filtered_data_objs <- sapply(datanames, function(name) teal_data()@env[[name]])
  unfiltered_data_objs <- sapply(datanames, function(name) teal_data()@env[[paste0(name, "_raw")]])

  rows <- lapply(
    datanames,
    function(dataname) {
      get_object_filter_overview(
        filtered_data = filtered_data_objs[[dataname]],
        unfiltered_data = unfiltered_data_objs[[dataname]],
        dataname = dataname,
        joinkeys = joinkeys
      )
    }
  )
  logger::log_trace("srv_data_overiew-get_filter_overview finalized")

  unssuported_idx <- vapply(rows, function(x) all(is.na(x[-1])), logical(1)) # this is mainly for vectors
  dplyr::bind_rows(c(rows[!unssuported_idx], rows[unssuported_idx]))
}

#' @rdname module_data_summary
#' @keywords internal
get_object_filter_overview <- function(filtered_data, unfiltered_data, dataname, joinkeys) {
  if (inherits(filtered_data, c("data.frame", "DataFrame", "array", "Matrix", "SummarizedExperiment"))) {
    get_object_filter_overview_array(filtered_data, unfiltered_data, dataname, joinkeys)
  } else if (inherits(filtered_data, "MultiAssayExperiment")) {
    get_object_filter_overview_MultiAssayExperiment(filtered_data, unfiltered_data, dataname)
  } else {
    data.frame(
      dataname = dataname,
      obs = NA,
      obs_filtered = NA
    )
  }
}

#' @rdname module_data_summary
#' @keywords internal
get_object_filter_overview_array <- function(filtered_data, unfiltered_data, dataname, joinkeys) {
  subject_keys <- Reduce(intersect, joinkeys[[dataname]])

  if (length(subject_keys) == 0) {
    data.frame(
      dataname = dataname,
      obs = nrow(unfiltered_data),
      obs_filtered = nrow(filtered_data)
    )
  } else {
    data.frame(
      dataname = dataname,
      obs = nrow(unfiltered_data),
      obs_filtered = nrow(filtered_data),
      subjects = nrow(unique(unfiltered_data[subject_keys])),
      subjects_filtered = nrow(unique(filtered_data[subject_keys]))
    )
  }
}

#' @rdname module_data_summary
#' @keywords internal
get_object_filter_overview_MultiAssayExperiment <- function(filtered_data, unfiltered_data, dataname) {
  experiment_names <- names(unfiltered_data)
  mae_info <- data.frame(
    dataname = dataname,
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
          joinkeys = join_keys() # empty join keys
        ),
        dataname = paste0(" - ", experiment_name)
      )
    }
  ))

  get_experiment_keys <- function(mae, experiment) {
    sample_subset <- subset(mae@sampleMap, subset = colname %in% colnames(experiment))
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

  experiment_info <- cbind(experiment_obs_info, experiment_subjects_info)
  dplyr::bind_rows(mae_info, experiment_info)
}
