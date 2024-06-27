ui_data <- function(id, data, title, header, footer) {
  ns <- shiny::NS(id)

  shiny::div(
    style = "display: inline-block;",
    if (inherits(data, "teal_data_module")) {
      actionButton(ns("open_teal_data_module"), NULL, icon = icon("database"))
    } else if (inherits(data, "teal_data")) {
      div("")
    }
  )
}

srv_data <- function(id, data, modules, filter) {
  checkmate::assert_character(id, max.len = 1, any.missing = FALSE)
  checkmate::assert_multi_class(data, c("teal_data", "teal_data_module"))
  checkmate::assert_class(modules, "teal_modules")
  checkmate::assert_class(filter, "teal_slices")

  moduleServer(id, function(input, output, session) {
    logger::log_trace("srv_teal_with_splash initializing module with data.")

    if (getOption("teal.show_js_log", default = FALSE)) {
      shinyjs::showLog()
    }

    # teal_data_rv contains teal_data object
    # either passed to teal::init or returned from teal_data_module
    teal_data_rv <- if (inherits(data, "teal_data_module")) {
      data$server(id = "teal_data_module")
    } else if (inherits(data, "teal_data")) {
      reactiveVal(data)
    }

    teal_data_rv_validate <- validate_reactive_teal_data(teal_data_rv)

    output$response <- renderUI({
      data <- teal_data_rv_validate()
      if (!is.null(data)) {
        showNotification("Data loaded successfully.", duration = 5)
        shinyjs::enable(selector = "#root_module-active_tab.nav-tabs a")
        removeModal()
      }
      NULL
    })

    observeEvent(input$open_teal_data_module, {
      showModal(
        modalDialog(
          tags$div(
            data$ui(session$ns("teal_data_module")),
            uiOutput(session$ns("response"))
          ),
          footer = NULL
        )
      )
    })

    if (inherits(data, "teal_data_module")) {
      print("Yes, clicking the modal")
      shinyjs::disable(selector = "#root_module-active_tab.nav-tabs a")
      shinyjs::click(id = "open_teal_data_module")
    }

    teal_data_rv_validate
  })
}
