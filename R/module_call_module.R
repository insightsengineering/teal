.ui_call_teal_module <- function(id, ui, args) {
  ns <- NS(id)
  display_fun <- if (is.null(ui)) shinyjs::hidden else function(x) x

  display_fun(
    tags$div(
      id = ns("validation_panel"),
      class = "validation-container",
      tags$div( # visibility controlled via css selector (visible when .validation-container[disabled="disabled"])
        class = "validation-input-info",
        title = "Disabled until data becomes valid",
        tags$span(bsicons::bs_icon("info-circle"), "Disabled until data becomes valid. Check your inputs.")
      ),
      tags$div(
        id = "module_container",
        if (is.null(ui)) {
          return(NULL)
        } else {
          .do_call_teal_module(
            ui,
            args = c(list(id = ns("module_content")), args)
          )
        },
        div(
          id = ns("validate_messages"),
          class = "validation-output-info",
          uiOutput(ns("error"))
        )
      )
    )
  )
}

.srv_call_teal_module <- function(id, data, server, datanames_required, args = list()) {
  moduleServer(id, function(input, output, session) {
    data_in_handled <- reactive(tryCatch(data(), error = function(e) e))
    logger::log_debug(".srv_call_teal_module@1 initializing module for { id }.")
    data_out <- reactiveVal(errorCondition("", class = "shiny.silent.error"))

    # Disable all elements if input data is not teal_data
    observeEvent(data_in_handled(), {
      shinyjs::toggleState(
        "validation_panel",
        condition = inherits(data_in_handled(), "teal_data")
      )
    })

    output$error <- renderUI({
      if (inherits(data_in_handled(), "teal_data")) {
        shiny::tagList(
          ui_validate_error(session$ns("silent_error")),
          ui_check_class_teal_data(session$ns("class_teal_data")),
          ui_check_required_datanames(session$ns("datanames_warning"))
        )
      }
    })

    .call_once_when(inherits(data_in_handled(), "teal_data"), {
      logger::log_debug(".srv_call_teal_module@2 triggering a module call for { id }.")
      args$id <- "module_content"
      args$data <- data
      data_unhandled <- .do_call_teal_module(server, args)
      if (is.reactive(data_unhandled)) {
        data_handled <- reactive(tryCatch(data_unhandled(), error = function(e) e))

        observeEvent(
          {
            data_handled()
            data_in_handled()
          },
          {
            if (inherits(data_in_handled(), "condition")) {
              logger::log_debug(".srv_call_teal_module@3 handing over error for module { id }.")
              data_out(data_in_handled())
            } else if (
              inherits(data_in_handled(), c("condition", "teal_data")) && !identical(data_handled(), data_out())
            ) {
              logger::log_debug(".srv_call_teal_module@3 module's output changes for module { id }.")
              data_out(data_handled())
            }
          }
        )

        srv_validate_error("silent_error", data_handled, validate_shiny_silent_error = FALSE)
        srv_check_class_teal_data("class_teal_data", data_handled)
        srv_check_required_datanames("datanames_warning", data_handled, datanames_required = datanames_required)


        # todo: When there is no UI (`ui = NULL`) it should still show the errors
        # is_previous_failed <- reactive(!inherits(data_in_handled(), "teal_data"))
        # It is a 1-way operation as there is no UI to correct the state
        # observe({
        #   if (!inherits(data_handled(), "teal_data") && !is_previous_failed()) {
        #     shinyjs::show("wrapper")
        #   }
        # })
      } else {
        data_out(data_unhandled)
      }
    })

    # Ignoring unwanted reactivity breaks during initialization
    reactive({
      validate(need(!inherits(data_out(), "condition"), message = data_out()$message)) # rethrow message
      data_out()
    })
  })
}
