#' @rdname module_filter_manager_modal
filter_manager_modal_ui <- function(id) {
  ns <- NS(id)

  # todo: menu with several options such as:
  #       - save filter state (initial should be saved by default)
  #       - restore saved filter states
  #       - ...
  tags$button(
    id = ns("show"),
    class = "btn action-button filter_manager_button",
    title = "Show filters manager modal",
    icon("gear")
  )
}

#' Filter manager modal
#'
#' Filter manager modal
#' @rdname module_filter_manager_modal
#' @inheritParams filter_manager_srv
#' @examples
#' fd1 <- teal.slice::init_filtered_data(list(iris = list(dataset = iris)))
#' fd2 <- teal.slice::init_filtered_data(list(iris = list(dataset = iris), mtcars = list(dataset = mtcars)))
#' fd3 <- teal.slice::init_filtered_data(list(iris = list(dataset = iris), women = list(dataset = women)))
#' filter <- teal::teal_filters(
#'   teal.slice::filter_var(dataname = "iris", varname = "Sepal.Length"),
#'   teal.slice::filter_var(dataname = "iris", varname = "Species"),
#'   teal.slice::filter_var(dataname = "mtcars", varname = "mpg"),
#'   teal.slice::filter_var(dataname = "women", varname = "height"),
#'   mapping = list(
#'     module2 = c("mtcars_mpg"),
#'     module3 = c("women_height"),
#'     global_filters = "iris_Species"
#'   )
#' )
#'
#' if (interactive()) {
#'   shinyApp(
#'     ui = fluidPage(
#'       filter_manager_modal_ui("manager")
#'     ),
#'     server = function(input, output, session) {
#'       filter_manager_modal_srv(
#'         "manager",
#'         filtered_data_list = list(module1 = fd1, module2 = fd2, module3 = fd3),
#'         filter = filter
#'       )
#'     }
#'   )
#' }
#'
filter_manager_modal_srv <- function(id, filtered_data_list, filter) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$show, {
      logger::log_trace("filter_manager_modal_srv@1 show button has been clicked.")
      showModal(
        modalDialog(
          filter_manager_ui(session$ns("filter_manager")),
          size = "l",
          easyClose = TRUE
        )
      )
    })

    filter_manager_srv("filter_manager", filtered_data_list, filter)
  })
}

#' @rdname module_filter_manager
filter_manager_ui <- function(id) {
  ns <- NS(id)
  div(
    tableOutput(ns("slices_table")),
    verbatimTextOutput(ns("filter_output"))

  )
}

#' Manage multiple `FilteredData` objects
#'
#' Manage multiple `FilteredData` objects
#'
#' @rdname module_filter_manager
#' @details
#' This module observes the changes of the filters in each `FilteredData` object
#' and keeps track of all filters used. Map of the filters is kept in so called
#' `slices_map` object where each `FilteredData` is linked with its active filters.
#' This map is represented in the UI as a matrix where rows are ids of the filters and
#' columns are names of the `filtered_data_list` (named after teal modules).
#'
#' @param id (`character(1)`)\cr
#'  `shiny` module id.
#' @param filtered_data_list (`list` of `FilteredData`)\cr
#'  Names of the list should be the same as `teal_module$label`.
#' @inheritParams init
#' @keywords internal
filter_manager_srv <- function(id, filtered_data_list, filter) {
  moduleServer(id, function(input, output, session) {
    logger::log_trace("filter_manager_srv initializing for: { paste(names(filtered_data_list), collapse = ', ')}.")
    # set initial teal_slices for each module-FilteredData
    shiny::isolate(
      lapply(names(filtered_data_list), function(module_name) {
        slices_module <- Filter(x = filter, f = function(x) {
          x$id %in% unique(unlist(attr(filter, "mapping")[c(module_name, "global_filters")]))
        })
        if (length(slices_module)) {
          filtered_data_list[[module_name]]$set_filter_state(slices_module)
        }
      })

    )

    # create a reactive map between modules and filters
    slices_map <- sapply(
      names(filtered_data_list),
      function(module_name) {
        shiny::reactiveVal(attr(filter, "mapping")[[module_name]])
      }
    )

    # global list of slices (all available teal_slice)
    slices_global <- reactiveVal(filter)

    lapply(names(filtered_data_list), function(module_name) {
      module_fd <- filtered_data_list[[module_name]]
      slices_module <- reactive(module_fd$get_filter_state())

      global_state_ids <- reactive(vapply(slices_global(), `[[`, character(1), "id"))
      current_state_ids <- reactive(vapply(slices_module(), `[[`, character(1), "id"))
      previous_state_ids <- reactiveVal(NULL)
      previous_slices_map <- reactiveVal(NULL)

      added_state_ids <- reactiveVal(NULL) # added on the module level
      removed_state_ids <- reactiveVal(NULL) # removed in the module
      activated_state_ids <- reactiveVal(NULL) # activated in the slices map
      deactivated_state_ids <- reactiveVal(NULL) # deactivated in the slices map

      observeEvent(current_state_ids(), {
        logger::log_trace("filter_manager_srv@1 detecting states deltas in module: { module_name }.")
        added <- setdiff(current_state_ids(), previous_state_ids())
        removed <- setdiff(previous_state_ids(), current_state_ids())
        if (length(added)) added_state_ids(added)
        if (length(removed)) removed_state_ids(removed)
        previous_state_ids(current_state_ids())
      })

      observeEvent(added_state_ids(), ignoreNULL = TRUE, {
        logger::log_trace("filter_manager_srv@2 detected new module filter: { module_name }.")
        if (any(!added_state_ids() %in% global_state_ids())) {
          slices_global_new <- c(slices_global(), slices_module())
          slices_global(slices_global_new)
        }
        if (!setequal(current_state_ids(), slices_map[[module_name]]())) {
          slices_map[[module_name]](current_state_ids())
        }
        added_state_ids(NULL)
      })

      observeEvent(removed_state_ids(), ignoreNULL = TRUE, {
        logger::log_trace("filter_manager_srv@3 detected removal of module filter: { module_name }.")
        # todo: trigger only if removed state is activa
        if (any(removed_state_ids() %in% slices_map[[module_name]]())) {
          new_slices_map <- setdiff(slices_map[[module_name]](), removed_state_ids())
          slices_map[[module_name]](new_slices_map)
        }
        removed_state_ids(NULL)
      })

      observeEvent(slices_map[[module_name]], {
        logger::log_trace("filter_manager_srv@4 detecting states deltas in slices_map of module: { module_name }.")
        added <- setdiff(
          setdiff(slices_map[[module_name]](), previous_slices_map()),
          current_state_ids()
        )
        removed <- setdiff(previous_slices_map(), slices_map[[module_name]]())
        if (length(added)) activated_state_ids(added)
        if (length(removed)) deactivated_state_ids(removed)
        previous_slices_map(current_state_ids())
      })

      observeEvent(activated_state_ids(), ignoreNULL = TRUE, {
        logger::log_trace("filter_manager_srv@5 detected activation of the filter for module: { module_name }.")
        activated_slices <- Filter(function(slice) slice$id %in% activated_state_ids(), slices_global())
        module_fd$set_filter_state(activated_slices)
        activated_state_ids(NULL)
      })

      observeEvent(deactivated_state_ids(), ignoreNULL = TRUE, {
        logger::log_trace("filter_manager_srv@6 detected deactivation of the filter for module: { module_name }.")
        deactivated_slices <- Filter(function(slice) slice$id %in% deactivated_state_ids(), slices_global())
        module_fd$remove_filter_state(deactivated_slices)
        deactivated_state_ids(NULL)
      })
    })

    mapping_matrix <- reactive({
      module_names <- names(filtered_data_list)
      filter_names <- vapply(X = slices_global(), `[[`, character(1), "id")
      mapping_matrix <- matrix(
        FALSE,
        nrow = length(filter_names),
        ncol = length(module_names),
        dimnames = list(filter_names, module_names)
      )
      for (i in module_names) {
        mapping_matrix[slices_map[[i]](), i] <- TRUE
      }
      mapping_matrix
    })

    output$slices_table <- renderTable(rownames = TRUE, {
      as.data.frame(mapping_matrix())
    })
    output$filter_output <- renderText(
      format(slices_global())
    )
  })
}