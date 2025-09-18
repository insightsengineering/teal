#' @noRd
ui_source_code <- function(id) uiOutput(NS(id, "source_code_container"))

#' @noRd
ui_source_button <- function(id) {
  shiny::tagList(
    htmltools::htmlDependency(
      name = "teal-reporter-busy-disable",
      version = utils::packageVersion("teal.reporter"),
      package = "teal.reporter",
      src = "js",
      script = "busy-disable.js"
    ),
    shiny::actionButton(
      shiny::NS(id, shiny::NS("source_code", "button")),
      "Show R code",
      class = "primary teal outline-button teal-busy-disable"
    )
  )
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
      teal_data_handled <- module_out()
      if (inherits(teal_data_handled, "qenv")) {
        teal.code::get_code(teal_data_handled)
      }
    })

    is_button_showed_r <- shiny::reactive({
      getOption("teal.show_src", TRUE) &&
        inherits(mod_out_r(), "qenv") &&
        !isFALSE(attr(mod_out_r()@code, "teal.show_src")) # Only hide when value is explicitly FALSE
    })

    output$source_code_container <- renderUI({
      if (is_button_showed_r()) {
        shinyjs::toggleState("source_code_container", condition = !is.null(code_out()))
        result <- ui_source_button(session$ns(NULL))
      }
    })
    teal.widgets::verbatim_popup_srv(
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
    attr(data@code, "teal.show_src") <- FALSE
    data
  })
}
