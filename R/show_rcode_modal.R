#' Show `R` code modal
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Use the [shiny::showModal()] function to show the `R` code inside.
#'
#' @param title (`character(1)`)
#'  Title of the modal, displayed in the first comment of the `R` code.
#' @param rcode (`character`)
#'  vector with `R` code to show inside the modal.
#' @param session (`ShinySession` optional)
#'  `shiny` session object, if missing then [shiny::getDefaultReactiveDomain()] is used.
#'
#' @references [shiny::showModal()]
#' @export
show_rcode_modal <- function(title = NULL, rcode, session = getDefaultReactiveDomain()) {
  rcode <- paste(rcode, collapse = "\n")

  ns <- session$ns
  showModal(modalDialog(
    tagList(
      tags$div(
        actionButton(ns("copyRCode"), "Copy to Clipboard", `data-clipboard-target` = paste0("#", ns("r_code"))),
        modalButton("Dismiss"),
        style = "mb-4"
      ),
      tags$div(tags$pre(id = ns("r_code"), rcode)),
    ),
    title = title,
    footer = tagList(
      actionButton(ns("copyRCode"), "Copy to Clipboard", `data-clipboard-target` = paste0("#", ns("r_code"))),
      modalButton("Dismiss")
    ),
    size = "l",
    easyClose = TRUE
  ))
}
