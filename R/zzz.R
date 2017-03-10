

.onLoad <- function(libname = find.package("teal"), pkgname = "teal") {
  #utils::data(cell.ext, package = "gmailR")

  if (is.null(options()$teal_logging)) {
    options(teal_logging = TRUE)
  }
}

