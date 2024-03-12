#' Landing popup module
#'
#' @description Creates a landing welcome popup for `teal` applications.
#'
#' This module is used to display a popup dialog when the application starts.
#' The dialog blocks access to the application and must be closed with a button before the application can be viewed.
#'
#' @param label (`character(1)`) Label of the module.
#' @param title (`character(1)`) Text to be displayed as popup title.
#' @param content (`character(1)`, `shiny.tag` or `shiny.tag.list`) with the content of the popup.
#'  Passed to `...` of `shiny::modalDialog`. See examples.
#' @param buttons (`shiny.tag` or `shiny.tag.list`) Typically a `modalButton` or `actionButton`. See examples.
#'
#' @return A `teal_module` (extended with `teal_landing_module` class) to be used in `teal` applications.
#'
#' @examples
#' app1 <- init(
#'   data = teal_data(iris = iris),
#'   modules = modules(
#'     landing_popup_module(
#'       content = "A place for the welcome message or a disclaimer statement.",
#'       buttons = modalButton("Proceed")
#'     ),
#'     example_module()
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app1$ui, app1$server)
#' }
#'
#' app2 <- init(
#'   data = teal_data(iris = iris),
#'   modules = modules(
#'     landing_popup_module(
#'       title = "Welcome",
#'       content = tags$b(
#'         "A place for the welcome message or a disclaimer statement.",
#'         style = "color: red;"
#'       ),
#'       buttons = tagList(
#'         modalButton("Proceed"),
#'         actionButton("read", "Read more",
#'           onclick = "window.open('http://google.com', '_blank')"
#'         ),
#'         actionButton("close", "Reject", onclick = "window.close()")
#'       )
#'     ),
#'     example_module()
#'   )
#' )
#'
#' if (interactive()) {
#'   shinyApp(app2$ui, app2$server)
#' }
#'
#' @export
landing_popup_module <- function(label = "Landing Popup",
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

  logger::log_info("Initializing landing_popup_module")

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
