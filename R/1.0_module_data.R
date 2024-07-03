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
    logger::log_trace("srv_data initializing.")

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
    # todo: teal as a module (requested by: @chlebowa, @kpagacz)
    # if we want to support teal as a module we need a following (we need to open srv_teal for data being a reactive)
    # } else if (inherits(data, c("reactive", "reactiveVal"))) {
    #  data
    # }

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

    setBookmarkExclude("open_teal_data_module")

    # Excluding the ids using full namespace and global shiny app session.
    app_session <- .subset2(shiny::getDefaultReactiveDomain(), "parent")
    setBookmarkExclude(extract_ids(data$ui(session$ns("teal_data_module"))), session = app_session)

    observeEvent(input$open_teal_data_module, {
      if (input$open_teal_data_module > 1) {
        footer <- modalButton("Dismiss")
        easy_close <- TRUE
      } else {
        footer <- NULL
        easy_close <- FALSE
      }
      showModal(
        modalDialog(
          class = ifelse(easy_close, "blur_background", "hide_background"),
          tags$div(
            data$ui(session$ns("teal_data_module")),
            uiOutput(session$ns("response"))
          ),
          footer = footer,
          easyClose = easy_close
        )
      )
    })

    if (inherits(data, "teal_data_module")) {
      shinyjs::disable(selector = "#root_module-active_tab.nav-tabs a")
      shinyjs::click(id = "open_teal_data_module")
    }

    data_rv <- reactiveVal(NULL)
    observeEvent(teal_data_rv_validate(), {
      data_rv(teal_data_rv_validate())
    })

    data_rv
  })
}


#' @param ui (`shiny.tag`) UI object to extract ids from
#' @keywords internal
#'
extract_ids <- function(ui) {
  ids <- c()

  if (is.list(ui)) {
    if (!is.null(ui$id)) {
      ids <- c(ids, ui$id)
    }
    for (element in ui) {
      ids <- c(ids, extract_ids(element))
    }
  }
  ids
}
