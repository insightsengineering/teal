.ui_call_module <- function(id, ui, ...) {
  ns <- NS(id)
  display_fun <- if (is.null(ui)) shinyjs::hidden else function(x) x

  display_fun(
    tags$div(
      id = ns("validation_panel"),
      class = "validation-panel",
      tags$div( # visibility controlled via css selector (visible when .validation-panel[disabled="disabled"])
        class = "disabled-info",
        title = "Disabled until data becomes valid",
        bsicons::bs_icon("info-circle"),
        "Disabled until data becomes valid. Check your inputs."
      ),
      tags$div(
        id = "module_wrapper",
        if (is.null(ui)) {
          return(NULL)
        } else {
          do.call(
            ui,
            args = c(
              list(id = ns("module_content")),
              list(...)
            )[names(formals(ui))],
            quote = FALSE
          )
        },
        div(
          id = ns("validate_messages"),
          class = "teal_validated",
          uiOutput(ns("error"))
        )
      )
    )
  )
}


.srv_call_module <- function(id, data_previous, server, datanames_required = list(), ...) {
  moduleServer(id, function(input, output, session) {
    data_previous_handled <- reactive(tryCatch(data_previous(), error = function(e) e))
    logger::log_debug("srv_transform_teal_data@1 initializing module for { id }.")
    data_out <- reactiveVal(errorCondition("", class = "shiny.silent.error"))

    # Disable all elements if input data is not teal_data
    observeEvent(data_previous_handled(), {
      shinyjs::toggleState(
        "validation_panel",
        condition = inherits(data_previous_handled(), "teal_data")
      )
    })

    .call_once_when(inherits(data_previous_handled(), "teal_data"), {
      logger::log_debug("srv_teal_transform_teal_data@2 triggering a transform module call for { id }.")
      data_unhandled <- do.call(
        server,
        args = c(
          list(id = "module_content", data = data_previous),
          list(...)
        )[names(formals(server))],
        quote = FALSE
      )
      data_handled <- reactive(tryCatch(data_unhandled(), error = function(e) e))

      observeEvent(
        {
          data_handled()
          data_previous_handled()
        },
        {
          if (inherits(data_previous_handled(), "condition")) {
            data_out(data_previous_handled())
          } else if (inherits(data_handled(), "teal_data")) {
            if (!identical(data_handled(), data_out())) {
              data_out(data_handled())
            }
          } else {
            data_out(data_handled()) # todo: what if it returns error?
          }
        }
      )

      is_previous_failed <- reactive(!inherits(data_previous_handled(), "teal_data"))

      srv_validate_error("silent_error", data_handled, validate_shiny_silent_error = FALSE)
      srv_check_class_teal_data("class_teal_data", data_handled)
      srv_check_required_datanames("datanames_warning", data_handled, datanames_required = datanames_required)

      # todo: When there is no UI (`ui = NULL`) it should still show the errors
      # It is a 1-way operation as there is no UI to correct the state
      # observe({
      #   if (!inherits(data_handled(), "teal_data") && !is_previous_failed()) {
      #     shinyjs::show("wrapper")
      #   }
      # })

      output$error <- renderUI({
        if (is_previous_failed()) {
          shinyjs::disable("module_wrapper")
          tags$div(
            "One of previous transformators failed. Please check its inputs.",
            class = "teal-output-warning"
          )
        } else if (inherits(data_previous_handled(), "teal_data")) {
          shinyjs::enable("module_wrapper")
          shiny::tagList(
            ui_validate_error(session$ns("silent_error")),
            ui_check_class_teal_data(session$ns("class_teal_data")),
            ui_check_required_datanames(session$ns("datanames_warning"))
          )
        }
      })
    })

    # Ignoring unwanted reactivity breaks during initialization
    reactive({
      validate(need(!inherits(data_out(), "condition"), message = data_out()$message)) # rethrow message
      data_out()
    })
  })
}
