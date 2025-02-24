#' Landing popup module
#'
#' @description `r lifecycle::badge("deprecated")` Creates a landing welcome popup for `teal` applications.
#'
#' This module is used to display a popup dialog when the application starts.
#' The dialog blocks access to the application and must be closed with a button before the application can be viewed.
#' This function is deprecated, please use `add_landing_modal()` on the teal app object instead.
#'
#' @param label (`character(1)`) Label of the module.
#' @param title (`character(1)`) Text to be displayed as popup title.
#' @param content (`character(1)`, `shiny.tag` or `shiny.tag.list`) with the content of the popup.
#'  Passed to `...` of `shiny::modalDialog`. See examples.
#' @param buttons (`shiny.tag` or `shiny.tag.list`) Typically a `modalButton` or `actionButton`. See examples.
#'
#' @return A `teal_module` (extended with `teal_landing_module` class) to be used in `teal` applications.
#'
#' @export
landing_popup_module <- function(label = "Landing Popup",
                                 title = NULL,
                                 content = NULL,
                                 buttons = modalButton("Accept")) {
  lifecycle::deprecate_soft(
    when = "0.16.0",
    what = "landing_popup_module()",
    details = paste(
      "landing_popup_module() is deprecated.",
      "Use add_landing_modal() on the teal app object instead."
    )
  )
  checkmate::assert_string(label)
  checkmate::assert_string(title, null.ok = TRUE)
  checkmate::assert_multi_class(
    content,
    classes = c("character", "shiny.tag", "shiny.tag.list", "html"), null.ok = TRUE
  )
  checkmate::assert_multi_class(buttons, classes = c("shiny.tag", "shiny.tag.list"))

  message("Initializing landing_popup_module")

  module <- module(
    label = label,
    datanames = NULL,
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
