#' Show R Code Modal
#'
#' @param title modal title
#' @param rcode character string with R code
#'
#' @export
#'
showRCodeModal <- function(title, rcode) {

  rcode <- paste(rcode, collapse = "\n")


  showModal(modalDialog(
    tags$pre(id = "r_code", rcode),
    title = title,
    footer = tagList(
      actionButton('copyRCode', 'Copy to Clipboard', `data-clipboard-target` = "#r_code"),
      modalButton("Dissmiss")
    ),
    size = "l",
    easyClose = TRUE
  ))

}
