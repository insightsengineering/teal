#' @noRd
verbatim_popup_srv <- teal.widgets::verbatim_popup_srv

#' @noRd
verbatim_popup_ui <- function(id) {
  teal.widgets::action_button_with_busy(
    shiny::NS(id, "button"), "Show R code", type = "primary", outline = TRUE
  )
}

#' @noRd
ui_source_code <- function(id) {
  ns <- shiny::NS(id)
  if (getOption("teal.show_src", TRUE)) {
    bslib::tooltip(
      id = ns("source_code_tooltip"),
      trigger = shinyjs::disabled(
        shiny::tags$div(
          id = ns("source_code_wrapper"),
          class = "cursor-helper",
          shiny::uiOutput(ns("source_code_body"))
        )
      ),
      shiny::uiOutput(ns("source_code_reason"))
    )
  }
  # shiny::uiOutput(shiny::NS(id, "source_code_container"))
}

#' @noRd
srv_source_code <- function(id, module_out) {
  moduleServer(id, function(input, output, session) {
    mod_out_r <- reactive({
      if (!is.null(module_out) && is.reactive(module_out)) {
        tryCatch(module_out(), error = function(e) e)
      }
    })

    code_out <- reactive({
      teal_data_handled <- mod_out_r()
      if (inherits(teal_data_handled, "qenv")) {
        teal.code::get_code(teal_data_handled)
      }
    })

    reason_r <- reactive({
      if (is.null(mod_out_r())) {
        "No source code is available from this module."
      } else if (isFALSE(attr(mod_out_r(), "teal.enable_src"))) {
        "The show source code functionality is disabled for this module."
      } else if (inherits(mod_out_r(), "error")) {
        "The module returned an error, check it for errors."
      } else if (is.null(code_out())) {
        "The module does not support source code functionality"
      }
    })

    output$source_code_body <- shiny::renderUI({
      if (getOption("teal.show_src", TRUE)) {
        reason <- trimws(reason_r() %||% "")
        verbatim_popup_ui(session$ns("source_code"))
      }
    })
    output$source_code_container <- shiny::renderUI({
      if (getOption("teal.show_src", TRUE)) {
        reason <- trimws(reason_r() %||% "")
        new_ui <- verbatim_popup_ui(session$ns("source_code"))
        if (!identical(reason, "")) {
          new_ui <- bslib::tooltip(
            id = session$ns("source_code_tooltip"),
            trigger = shinyjs::disabled(
              shiny::tags$div(
                id = session$ns("source_code_wrapper"),
                class = "cursor-helper",
                new_ui
              )
            ),
            reason
          )
        }
        new_ui
      }
    })

    verbatim_popup_srv(
      id = "source_code", verbatim_content = code_out, title = "Show R Code for Response"
    )
  })
}

#' Disable the "Show R Code" global button in the UI
#'
#' Convenience function that disables the user's ability to see the code of the module.
#' @param x (`teal_module`) a `teal_module` object.
#' @return modified data object that indicates that it should not show the "Show R Code"
#' button in the UI.
#' @examples
#' app <- init(
#'   data = within(teal_data(), iris <- iris),
#'   modules = modules(
#'     example_module(label = "example teal module") |> disable_src()
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#' @export
disable_src <- function(x) {
  checkmate::assert_multi_class(x, c("teal_module", "teal_modules"))
  after(x, server = function(data) {
    attr(data, "teal.enable_src") <- FALSE
    data
  })
}
