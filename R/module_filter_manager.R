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

filter_manager_ui <- function(id) {
  ns <- NS(id)
  div(
    tableOutput(ns("filters_map")),
    verbatimTextOutput(ns("filter_output"))

  )
}

filter_manager_srv <- function(id, filtered_data_list, filter) {
  moduleServer(id, function(input, output, session) {
    logger::log_trace("filter_manager_srv initializing for: { paste(names(filtered_data_list), collapse = ', ')}.")

    # set initial filters
    lapply(names(attr(filter, "mapping")), function(module_name) {
      initial_module_slices <- Filter(x = filter, f = function(x) {
        get_teal_slice_id(x) %in% attr(filter, "mapping")[[module_name]]
      })
      filtered_data_list[[module_name]]$set_filter_state(initial_module_slices)
    })


    # reactive map filters <-> modules
    filters_map <- sapply(
      names(filtered_data_list),
      function(module_name) {
        shiny::reactiveVal(attr(filter, "mapping")[[module_name]])
      }
    )

    # copy of all filters kept in one (global) place
    all_filters <- reactiveVal(filter)
    lapply(names(filtered_data_list), function(module_name) {
      module_fd <- filtered_data_list[[module_name]]
      module_states <- reactive(module_fd$get_filter_state())

      global_state_ids <- reactive(vapply(all_filters(), get_teal_slice_id, character(1)))
      current_state_ids <- reactive(vapply(module_states(), get_teal_slice_id, character(1)))
      previous_state_ids <- reactiveVal(NULL)
      added_state_ids <- reactiveVal(NULL)
      removed_state_ids <- reactiveVal(NULL)

      observeEvent(current_state_ids(), {
        logger::log_trace("filter_manager_srv@1 detecting states for module: { module_name }.")
        added <- setdiff(current_state_ids(), previous_state_ids())
        removed <- setdiff(previous_state_ids(), current_state_ids())
        if (length(added)) added_state_ids(added)
        if (length(removed)) removed_state_ids(removed)
        previous_state_ids(current_state_ids())
      })

      observeEvent(added_state_ids(), ignoreNULL = TRUE, {
        logger::log_trace("filter_manager_srv@2 updating global list of states for module: { module_name }.")
        if (any(!added_state_ids() %in% global_state_ids())) {
          all_filters_new <- c(all_filters(), module_states())
          all_filters(all_filters_new)
          filters_map[[module_name]](current_state_ids())
          added_state_ids(NULL)
        }
      })

      observeEvent(removed_state_ids(), ignoreNULL = TRUE, {
        logger::log_trace("filter_manager_srv@2 removing state from the module: { module_name }.")
        filters_map[[module_name]](removed_state_ids())
        removed_state_ids(NULL)
      })
    })

    reactive_mapping_matrix <- reactive({
      module_names <- names(filtered_data_list)
      filter_names <- vapply(X = all_filters(), FUN = get_teal_slice_id, FUN.VALUE = character(1))
      mapping_matrix <- matrix(
        FALSE,
        nrow = length(filter_names),
        ncol = length(module_names),
        dimnames = list(filter_names, module_names)
      )
      for (i in module_names) {
        mapping_matrix[filters_map[[i]](), i] <- TRUE
      }
      mapping_matrix
    })

    output$filters_map <- renderTable(rownames = TRUE, {
      as.data.frame(reactive_mapping_matrix())
    })
    output$filter_output <- renderText(
      format(all_filters())
    )

    # todo:
    #  - mapping matrix should be editable
    #  - we need to observe these changes and set/remove corresponding filters
  })
}
