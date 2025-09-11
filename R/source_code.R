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
      class = "primary outline-button teal-busy-disable"
    )
  )
}

#' @noRd
srv_source_code <- function(id, module_out) {
  moduleServer(id, function(input, output, session) {
    mod_out_r <- reactive({
      req(module_out)
      if (is.reactive(module_out)) {
        module_out()
      }
    })

    doc_out <- reactive({
      req(mod_out_r())
      teal_data_handled <- tryCatch(mod_out_r(), error = function(e) e)
      tcard <- if (inherits(teal_data_handled, "qenv")) {
        teal.code::get_code(teal_data_handled)
      }
    })

    is_button_showed_r <- shiny::reactive({
      getOption("teal.show_r_code", TRUE) &&
        inherits(mod_out_r(), "qenv") &&
        !isFALSE(attr(mod_out_r()@code, "teal.show_r_code")) # Only hide when value is explicitly FALSE
    })

    .call_once_when(!is.null(doc_out()), {
      output$source_code_container <- renderUI({
        if (is_button_showed_r()) {
          ui_source_button(session$ns(NULL))
        }
      })
      teal.widgets::verbatim_popup_srv(
        id = "source_code", verbatim_content = doc_out, title = "Show R Code for Response"
      )
    })
    observeEvent(doc_out(), ignoreNULL = FALSE, {
      shinyjs::toggleState("source_code_container", condition = !is.null(doc_out()))
    })
  })
}

#' Disable the "Show R Code" global button in the UI
#'
#' Convenience function that disables the user's ability to see the code of the module.
#' @param x (`teal_module`) a `teal_module` object.
#' @return modified data object that indicates that it should not show the "Show R Code"
#' button in the UI.
#' @export
#' @examples
#' app <- init(
#'   data = within(teal_data(), iris <- iris),
#'   modules = modules(
#'     example_module(label = "example teal module") |> disable_show_r_code()
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
disable_show_r_code <- function(x) {
  checkmate::assert_class(x, "teal_module")
  after(x, server = function(data) {
    attr(data@code, "teal.show_r_code") <- FALSE
    data
  })
}
