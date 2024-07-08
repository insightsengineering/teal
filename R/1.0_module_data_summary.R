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

#' @description
#' Server function to display the number of records in the filtered and unfiltered data.
#'
#' @param id (`character(1)`)
#'   `shiny` module instance id.
#' @param filtered_teal_data (`reactive`) (`teal_data`) an output of `.make_teal_data()`
#' @return `NULL`.
srv_data_summary <- function(id, filtered_teal_data) {
  checkmate::check_class(filtered_teal_data, "reactive")
  moduleServer(
    id = id,
    function(input, output, session) {
      # shinyjs::hide(id = "teal-main_ui-filter_panel-overview") # this doesnt hide filter-panel-overiw from teal.slice YET
      logger::log_trace("srv_data_summary initializing")

      observeEvent(input$minimise_filter_overview, {
        shinyjs::toggle("filters_overview_contents")
        toggle_icon(session$ns("minimise_filter_overview"), c("fa-angle-right", "fa-angle-down"))
        toggle_title(session$ns("minimise_filter_overview"), c("Restore panel", "Minimise Panel"))
      })

      output$table <- renderUI({
        req(inherits(filtered_teal_data(), "teal_data"))
        logger::log_trace("srv_data_summary updating counts")

        if (length(datanames(filtered_teal_data())) == 0) {
          return(NULL)
        }

        filter_overview <- get_filter_overview(filtered_teal_data)

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
        logger::log_trace("srv_data_summary updated counts")
        table_html
      })
      logger::log_trace("srv_data_summary initialized")
      NULL
    }
  )
}

get_filter_overview <- function(filtered_teal_data) {
  rows <- lapply(
    datanames(filtered_teal_data()),
    get_object_filter_overview,
    filtered_teal_data = filtered_teal_data
  )
  unssuported_idx <- vapply(rows, function(x) all(is.na(x[-1])), logical(1))
  dplyr::bind_rows(c(rows[!unssuported_idx], rows[unssuported_idx]))
}

get_object_filter_overview <- function(filtered_teal_data, dataname, experiment_name = NULL) {
  object <- extract_data(filtered_teal_data, dataname, experiment_name)$raw

  # not a regular S3 method, so we do not need to have dispatch for df/array/Matrix separately
  if (inherits(object, c("data.frame", "DataFrame", "array", "Matrix", "SummarizedExperiment"))) {
    get_object_filter_overview_array(filtered_teal_data, dataname, experiment_name)
  } else if (inherits(object, "MultiAssayExperiment")) {
    get_object_filter_overview_MultiAssayExperiment(filtered_teal_data, dataname, experiment_name)
  } else {
    data.frame(
      dataname = dataname,
      obs = NA,
      obs_filtered = NA
    )
  }
}

get_object_filter_overview_array <- function(filtered_teal_data, dataname, experiment_name) {
  logger::log_trace("srv_data_overiew-get_filter_overview initialized")

  subject_keys <- if (!is.null(experiment_name)) {
    join_keys()
  } else {
    ## if (length(parent(filtered_teal_data(), dataname)) > 0) {
    # if (dataname %in% names(join_keys(filtered_teal_data()))) {
    #   join_keys(filtered_teal_data())[[dataname]][[dataname]]
    # } else {
    #   character(0) # was self$get_keys() before
    # }
    join_keys(filtered_teal_data())[[dataname]][[dataname]]
  }

  data <- extract_data(filtered_teal_data, dataname, experiment_name)

  if (length(subject_keys) == 0) {
    data.frame(
      dataname = dataname,
      obs = nrow(data$raw),
      obs_filtered = nrow(data$filtered)
    )
  } else {
    data.frame(
      dataname = dataname,
      obs = nrow(data$raw),
      obs_filtered = nrow(data$filtered),
      subjects = nrow(unique(data$raw[subject_keys])),
      subjects_filtered = nrow(unique(data$filtered[subject_keys]))
    )
  }
}

extract_data <- function(filtered_teal_data, dataname, experiment_name = NULL) {
  data <- filtered_teal_data()@env[[paste0(dataname, "_raw")]]
  data_filtered <- filtered_teal_data()@env[[dataname]]

  if (!is.null(experiment_name)) {
    data <- data[[experiment_name]]
    data_filtered <- data_filtered[[experiment_name]]
  }

  return(
    list(
      raw = data,
      filtered = data_filtered
    )
  )
}

get_object_filter_overview_MultiAssayExperiment <- function(filtered_teal_data, dataname, experiment_name) {
  data <- extract_data(filtered_teal_data, dataname, experiment_name)

  experiment_names <- names(data$raw)
  mae_info <- data.frame(
    dataname = dataname,
    subjects = nrow(SummarizedExperiment::colData(data$raw)),
    subjects_filtered = nrow(SummarizedExperiment::colData(data$filtered))
  )

  experiment_obs_info <- do.call("rbind", lapply(
    experiment_names,
    function(expr_name) {
      transform(
        get_object_filter_overview(
          filtered_teal_data,
          dataname,
          expr_name
        ),
        dataname = paste0(" - ", expr_name)
      )
    }
  ))

  get_experiment_keys <- function(mae, experiment) {
    sample_subset <- subset(MultiAssayExperiment::sampleMap(mae), subset = colname %in% colnames(experiment))
    length(unique(sample_subset$primary))
  }

  experiment_subjects_info <- do.call("rbind", lapply(
    experiment_names,
    function(experiment_name) {
      data.frame(
        subjects = get_experiment_keys(data$filtered, data$raw[[experiment_name]]),
        subjects_filtered = get_experiment_keys(data$filtered, data$filtered[[experiment_name]])
      )
    }
  ))

  experiment_info <- cbind(experiment_obs_info, experiment_subjects_info)
  dplyr::bind_rows(mae_info, experiment_info)
}
