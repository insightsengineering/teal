#' Show R Code Modal
#'
#' @param rcode character string with R code
#'
#' @export
#'
showRCodeModal <- function(title, rcode) {

  rcode <- paste(rcode, collapse = "\n")


  showModal(modalDialog(
    title = title,
    tags$pre(tags$code(class="R", rcode)),
    easyClose = TRUE,
    size = "l"
  ))

}
