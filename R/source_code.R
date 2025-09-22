#' @noRd
ui_source_code <- function(id) uiOutput(NS(id, "source_code_container"))

#' @noRd
verbatim_popup_ui <- function(id, title) {
  shiny::tagList(
    htmltools::htmlDependency(
      name = "teal-reporter-busy-disable",
      version = utils::packageVersion("teal.reporter"),
      package = "teal.reporter",
      src = "js",
      script = "busy-disable.js"
    ),
    shiny::actionButton(
      shiny::NS(id, "button"),
      "Show R code",
      class = "primary teal outline-button teal-busy-disable",
      title = title
    )
  )
}

#' @noRd
verbatim_popup_srv <- teal.widgets::verbatim_popup_srv

#' @noRd
ui_source_button <- function(id, title = NULL) {
  verbatim_popup_ui(shiny::NS(id, "source_code"), title)
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

    # Global option that doesn't show button
    is_button_showed_r <- shiny::reactive(getOption("teal.show_src", TRUE))

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

    output$source_code_container <- renderUI({
      if (is_button_showed_r()) {
        bslib::tooltip(
          id = session$ns("source_code_tooltip"),
          trigger = shiny::tags$div(
            id = session$ns("source_code_wrapper"),
            class = "cursor-helper",
            ui_source_button(session$ns(NULL))
          ),
          ""
        )
      }
    })

    teal.widgets::verbatim_popup_srv(
      id = "source_code", verbatim_content = code_out, title = "Show R Code for Response"
    )

    observeEvent(code_out(), ignoreNULL = FALSE, {
      reason <- trimws(reason_r() %||% "")
      if (is.null(reason) || identical(reason, "")) {
        session$sendCustomMessage("disable-tooltip", session$ns("source_code_wrapper"))
      } else {
        session$sendCustomMessage("enable-tooltip", session$ns("source_code_wrapper"))
        bslib::update_tooltip(id = "source_code_tooltip", reason)
      }

      shinyjs::toggleState(
        "source_code_wrapper",
        condition = !is.null(code_out()) &&
          checkmate::test_multi_class(mod_out_r(), c("qenv", "qenv.error")) &&
          !isFALSE(attr(mod_out_r(), "teal.enable_src")) # Only forcibly disable when value is explicitly FALSE
      )
    })
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
