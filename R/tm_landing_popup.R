#' Landing Popup Module
#'
#' @description This module creates a landing welcome popup for `teal` applications.
#'
#' @details If you use this module in `teal::init(modules = )`, it will not be wrapped in a tab in `teal` application.
#'
#' @param label `character(1)` the label of the module.
#' @param title `character(1)` the text to be displayed as a title of the popup.
#' @param content `character(1)` the content of the popup. Passed to `...` of `shiny::modalDialog`. Can be a `character`
#' or a text input control (like `textInput`) or a list of `shiny` tags. See examples.
#' @param buttons `shiny` tag or a list of tags (`tagList`). Typically a `modalButton` or `actionButton`. See examples.
#'
#' @return A `teal_module` (extended with `teal_landing_module` class) to be used in `teal` applications.
#'
#' @examples
#' app1 <- teal::init(
#'   data = teal.data::dataset("iris", iris),
#'   modules = teal::modules(
#'     teal::tm_landing_popup(
#'       content = "A place for the welcome message or a disclaimer statement.",
#'       buttons = modalButton("Proceed")
#'     ),
#'     module(
#'       label = "example module",
#'       server = function(input, output, session, data) {},
#'       ui = function(id, ...) div(p("Example text"))
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app1$ui, app1$server)
#' }
#'
#' app2 <- teal::init(
#'   data = teal.data::dataset("iris", iris),
#'   modules = teal::modules(
#'     teal::tm_landing_popup(
#'       title = "Welcome",
#'       content = tags$b("A place for the welcome message or a disclaimer statement.", style = "color: red;"),
#'       buttons = tagList(
#'         modalButton("Proceed"),
#'         actionButton("close", "Read more", onclick = "window.open('http://google.com', '_blank')")
#'       )
#'     ),
#'     module(
#'       label = "example module",
#'       server = function(input, output, session, data) {},
#'       ui = function(id, ...) div(p("Example text"))
#'     )
#'   )
#' )
#'
#' if (interactive()) {
#'   shinyApp(app2$ui, app2$server)
#' }
#'
#' @export
tm_landing_popup <-
  function(label = "Landing Popup",
           title = NULL,
           content = NULL,
           buttons = modalButton("Accept")) {
    checkmate::assert_string(label)
    checkmate::assert_string(title, null.ok = TRUE)
    checkmate::assert_multi_class(
      content,
      classes = c("character", "shiny.tag", "shiny.tag.list", "html"), null.ok = TRUE
    )
    checkmate::assert_multi_class(buttons, classes = c("shiny.tag", "shiny.tag.list"))

    logger::log_info("Initializing tm_landing_popup")

    module <- module(
      label = label,
      server = function(id) {
        moduleServer(id, function(input, output, session) {
          showModal(
            modalDialog(
              id = "landingpopup",
              title = title,
              content,
              footer = buttons
            )
          )
        })
      }
    )
    class(module) <- c("teal_module_landing", class(module))
    module
  }
