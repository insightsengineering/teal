#' Show R Code Modal
#'
#' @export
#' @description `r lifecycle::badge("stable")`
#' Use the [shiny::showModal()] function to show the R code inside.
#'
#' @param title (`character(1)`)\cr
#'  Title of the modal, displayed in the first comment of the R-code.
#' @param rcode (`character`)\cr
#'  vector with R code to show inside the modal. You can use [teal.code::get_code()] to derive this R
#'  code inside a module.
#' @param session (`ShinySession` optional)\cr
#'  `shiny` Session object, if missing then [shiny::getDefaultReactiveDomain()] is used.
#'
#' @references [shiny::showModal()] [teal.code::get_code()]
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

  return(NULL)
}
