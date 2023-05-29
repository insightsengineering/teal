filter_manager_modal_ui <- function(id) {
  ns <- NS(id)
  tags$button(
    id = ns("show"),
    class = "btn action-button filter_manager_button",
    title = "Show filters manager modal",
    icon("gear")
  )
}

filter_manager_modal_srv <- function(id, filtered_data_list) {
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

    filter_manager_srv("filter_manager", filtered_data_list)
  })
}

filter_manager_ui <- function(id) {
  ns <- NS(id)
  tableOutput(ns("filters_map"))
}

filter_manager_srv <- function(id, filtered_data_list) {
  moduleServer(id, function(input, output, session) {
    logger::log_trace("filter_manager_srv initializing for: { paste(names(filtered_data_list), collapse = ', ')}.")

    slices_global <- sapply(names(filtered_data_list), function(x) shiny::reactiveVal(NULL))

    lapply(
      names(filtered_data_list),
      function(i) {
        filter_states <- reactive(filtered_data_list[[i]]$get_filter_state())
        observeEvent(
          filter_states(),
          {
            logger::log_trace("filter_manager_srv@1 detected changes in filters collection denoted by '{ i }'.")

            # add relevant external_id to the teal_slice objects
            slices <- lapply(filter_states(), function(x) {
              x$external_id <- union(x$external_id, i)
              x
            })
            slices_global[[i]](slices)
          }
        )
      }
    )
    reactive_mapping <- reactive({
      config <- lapply(slices_global, function(x) {
        unlist(sapply(x(), get_teal_slice_id))
      })
      external_names <- names(slices_global)
      filter_ids <- unique(unlist(config))
      mapping <- matrix(
        FALSE,
        nrow = length(filter_ids),
        ncol = length(external_names),
        dimnames = list(filter_ids, external_names)
      )
      for (i in seq_along(config)) {
        mapping[config[[i]], i] <- TRUE
      }
      mapping
    })


    output$filters_map <- renderTable(rownames = TRUE, {
      as.data.frame(reactive_mapping())
    })
  })
}
