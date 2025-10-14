#' @noRd
verbatim_popup_srv <- teal.widgets::verbatim_popup_srv

#' @noRd
verbatim_popup_ui <- function(id) {
  # we replace teal.widgets::verbarbatim_popup_ui with own button. `inputId="button"` is necessary
  .action_button_busy(
    shiny::NS(id, "button"), "Show R code",
    type = "primary", outline = TRUE
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
          verbatim_popup_ui(ns("source_code"))
        )
      ),
      shiny::div(
        "Click here to show R code that generated this module's output.",
        shiny::uiOutput(ns("source_code_reason"))
      )
    )
  }
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
        "No source code is available for this module."
      } else if (isFALSE(attr(mod_out_r(), "teal.enable_src"))) {
        "The source code functionality is disabled for this module."
      } else if (inherits(mod_out_r(), "error")) {
        "The module returned an error, check it for errors."
      } else if (is.null(code_out())) {
        "The module does not support source code functionality."
      }
    })

    if (getOption("teal.show_src", TRUE)) {
      output$source_code_reason <- shiny::renderUI({
        reason <- trimws(reason_r())
        if (length(reason)) {
          icon <- if (grepl("error", reason)) "exclamation-triangle-fill" else "info-circle-fill"
          shiny::div(
            class = if (grepl("error", reason)) "text-danger" else "text-info",
            style = "padding-top: 0.5em;",
            bsicons::bs_icon(name = icon),
            reason
          )
        }
      })

      verbatim_popup_srv(
        id = "source_code",
        verbatim_content = code_out,
        title = "Show R Code",
        disabled = reactive(!is.null(reason_r()))
      )
    }
  })
}

#' Disable the "Show R Code" global button in the UI
#'
#' Convenience function that disables the user's ability to see the code of the module.
#'
#' This is equivalent to setting the attribute `teal.enable_src` to `FALSE` on the
#' data object returned by the module.
#' @param x (`teal_module`) a `teal_module` object.
#' @return modified data object that indicates that it should not show the "Show R Code"
#' button in the UI.
#' @seealso [disable_report()]
#' @examplesShinylive
#' library(teal)
#' interactive <- function() TRUE
#' {{ next_example }}
#' @examples
#' # Disabling source on a single module
#' app <- init(
#'   data = within(teal_data(), iris <- iris),
#'   modules = modules(
#'     example_module(label = "example teal module") |> disable_src()
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#' @examplesShinylive
#' library(teal)
#' interactive <- function() TRUE
#' {{ next_example }}
#' @examples
#' # Multiple modules
#' app <- init(
#'   data = within(teal_data(), iris <- iris),
#'   modules = modules(
#'     example_module(label = "example 1"),
#'     example_module(label = "example 2")
#'   ) |> disable_src()
#' )
#'
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
