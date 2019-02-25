.onLoad <- function(libname = find.package("teal"), pkgname = "teal") {
  if (is.null(options()$teal_logging)) {
    options(teal_logging = TRUE)
  }
}

.onAttach <- function(libname, pkgname) {

  packageStartupMessage("\nteal version ",
                        utils::packageDescription(pkg = pkgname,
                                                  lib.loc = libname,
                                                  fields = "Version"))
}
