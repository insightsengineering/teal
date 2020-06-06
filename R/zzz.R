.onLoad <- function(libname = find.package("teal"), pkgname = "teal") { # nolint
  if (is.null(options()$teal_logging)) {
    options(teal_logging = TRUE)
    options(teal_show_js_log = FALSE)
  }
}

.onAttach <- function(libname, pkgname) { # nolint
  packageStartupMessage("\nteal version ",
                        utils::packageDescription(pkg = pkgname,
                                                  lib.loc = libname,
                                                  fields = "Version"))
}
