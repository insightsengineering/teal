#' Get object keys
#'
#'
#' @param x any R object
#' @return keys attribute from given object
#'
#' @export
#'
#' @examples
#' library(random.cdisc.data)
#' ASL <- radsl()
#'
#' keys(ASL) <- c("USUBJID", "STUDYID")
#' keys(ASL)

keys <- function(x) {
  attr(x, "keys")
}

#' Set object keys
#'
#'
#' @param x any R object
#' @param value (\code{character}) with keys
#'
#' @export
#'
#' @examples
#' library(random.cdisc.data)
#' ASL <- radsl()
#'
#' keys(ASL) <- c("USUBJID", "STUDYID")
#' keys(ASL)
`keys<-` <- function(x, value) {
  attr(x, "keys") <- value
  x
}
