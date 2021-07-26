#' Module for the right filter panel in the teal app
#' with a filter overview panel and a filter variable panel.
#'
#' This panel contains info about the number of observations left in
#' the (active) datasets and allows to filter the datasets.
#'
#' @param id (`character(1)`)\cr
#'   module id
#' @param datasets (`FilteredData`)\cr
#'  object which stores datasets and manages filters.
#' @param datanames (`character`)\cr
#'  datanames to create empty UIs for (which will be populated in the server)
ui_filter_panel <- function(id, datasets, datanames) {
  stopifnot(
    is(datasets, "FilteredData"),
    is_character_vector(datanames)
  )

  ns <- NS(id)
  div(
    id = ns("filter_panel_whole"), # used for hiding / showing
    div(
      id = ns("filters_overview"), # not used, can be used to customize CSS behavior
      class = "well",
      tags$div(
        class = "row",
        tags$div(
          class = "col-sm-9",
          tags$label("Active Filter Summary", class = "text-primary", style = "margin-bottom: 15px;")
        ),
        tags$div(
          class = "col-sm-3",
          tags$a(
            href = "javascript:void(0)",
            class = "remove pull-right",
            onclick = sprintf(
              "$('#%s').toggle();",
              ns("teal_filters_overview_contents")
            ),
            title = "minimise panel",
            tags$span(icon("minus-circle", lib = "font-awesome"))
          )
        )
      ),
      tags$br(),
      div(
        id = ns("filters_overview_contents"),
        ui_filter_overview(ns("teal_filters_info"))
      )
    ),

    div(
      id = ns("filter_active_vars"), # not used, can be used to customize CSS behavior
      class = "well",
      tags$div(
        class = "row",
        tags$div(
          class = "col-sm-6",
          tags$label("Active Filter Variables", class = "text-primary", style = "margin-bottom: 15px;")
        ),
        tags$div(
          class = "col-sm-6",
          actionLink(
            ns("remove_all_filters"),
            "",
            icon("times-circle", lib = "font-awesome"),
            title = "remove active filters",
            class = "remove_all pull-right"
          ),
          tags$a(
            href = "javascript:void(0)",
            class = "remove pull-right",
            onclick = sprintf(
              "$('#%s').toggle();",
              ns("teal_filter_active_vars_contents")
            ),
            title = "minimise panel",
            tags$span(icon("minus-circle", lib = "font-awesome"))
          )
        )
      ),

      div(
        id = ns("filter_active_vars_contents"),
        tagList(
          lapply(
            datanames,
            function(dataname) {
              dataset_filters <- datasets$get_filtered_datasets(dataname)
              dataset_filters$ui(
                id = ns(sprintf("%s_filters", dataname))
              )
            }
          )
        )
      )
    ),

    div(
      id = ns("filter_add_vars"), # not used, can be used to customize CSS behavior
      class = "well",
      tags$div(
        class = "row",
        tags$div(
          class = "col-sm-9",
          tags$label("Add Filter Variables", class = "text-primary", style = "margin-bottom: 15px;")
        ),
        tags$div(
          class = "col-sm-3",
          tags$a(
            href = "javascript:void(0)",
            class = "remove pull-right",
            onclick = sprintf("$('#%s').toggle();", ns("teal_filter_add_vars_contents")),
            title = "minimise panel",
            tags$span(icon("minus-circle", lib = "font-awesome"))
          )
        )
      ),
      div(
        id = ns("filter_add_vars_contents"),
        tagList(
          lapply(
            datanames,
            function(dataname) {
              dataset_filters <- datasets$get_filtered_datasets(dataname)
              id <- ns(sprintf("add_%s_filter", dataname))
              # add span with same id to show / hide
              return(
                span(
                  id = id,
                  dataset_filters$ui_add_filter_state(id)
                )
              )
            }
          )
        )
      )
    )
  )
}

#' Server function for filter panel
#'
#' @inheritParams srv_shiny_module_arguments
#' @param active_datanames `function / reactive` returning datanames that
#'   should be shown on the filter panel,
#'   must be a subset of the `datanames` argument provided to `ui_filter_panel`;
#'   if the function returns `NULL` (as opposed to `character(0)`), the filter
#'   panel will be hidden
#'
srv_filter_panel <- function(input, output, session, datasets, active_datanames = function() "all") {
  stopifnot(
    is(datasets, "FilteredData"),
    is.function(active_datanames) || is.reactive(active_datanames)
  )
  callModule(
    srv_filter_overview,
    "teal_filters_info",
    datasets = datasets,
    datanames = active_datanames
  )

  # use isolate because we assume that the number of datasets does not change over the course of the teal app
  # alternatively, one can proceed as in modules_filter_items to dynamically insert, remove UIs
  isol_datanames <- isolate(datasets$datanames()) # they are already ordered
  # should not use for-loop as variables are otherwise only bound by reference and last dataname would be used
  lapply(
    isol_datanames,
    function(dataname) {
      dataset_filters <- datasets$get_filtered_datasets(dataname)
      callModule(
        module = dataset_filters$server,
        id = sprintf("%s_filters", dataname)
      )
    }
  )

  lapply(
    isol_datanames,
    function(dataname) {
      dataset_filters <- datasets$get_filtered_datasets(dataname)
      callModule(
        module = dataset_filters$srv_add_filter_state,
        id = sprintf("add_%s_filter", dataname)
      )
    }
  )


  # we keep anything that may be selected to add (happens when the variable is not available for filtering)
  # lapply(isol_datanames, function(dataname) paste0("teal_add_", dataname, "_filter")) #nolint
  setBookmarkExclude(names = c(
    # these will be regenerated dynamically
    lapply(isol_datanames, function(dataname) paste0(dataname, "filters"))
  ))

  # rather than regenerating the UI dynamically for the dataset filtering,
  # we instead choose to hide/show the elements
  # the filters for this dataset are just hidden from the UI, but still applied
  # optimization: we set `priority = 1` to execute it before the other
  # observers (default priority 0), so that they are not computed if they are hidden anyways
  observeEvent(active_datanames(), priority = 1, {
    if (length(active_datanames()) == 0 || is.null(active_datanames())) {
      # hide whole module UI when no datasets or when NULL
      shinyjs::hide("filter_panel_whole")
    } else {
      shinyjs::show("filter_panel_whole")

      # selectively hide / show to only show `active_datanames` out of all datanames
      lapply(
        datasets$datanames(),
        function(dataname) {
          id_add_filter <- sprintf("add_%s_filter", dataname)
          id_filter_dataname <- sprintf("%s_filters", dataname)

          if (dataname %in% active_datanames()) {
            # shinyjs takes care of the namespace around the id
            shinyjs::show(id_add_filter)
            shinyjs::show(id_filter_dataname)
          } else {
            shinyjs::hide(id_add_filter)
            shinyjs::hide(id_filter_dataname)
          }
        }
      )
    }
  }, ignoreNULL = FALSE)

  observeEvent(input$remove_all_filters, {
    .log("removing all active filters from filter panel")
    lapply(datasets$datanames(), function(dataname) {
      dataset_filter <- datasets$get_filtered_datasets(dataname = dataname)
      dataset_filter$queues_empty()
    })
  })
}
