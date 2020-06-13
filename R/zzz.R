.onLoad <- function(libname, pkgname) { #nolintr
  # adapted from https://github.com/r-lib/devtools/blob/master/R/zzz.R
  teal_default_options <- list(
    teal_logging = TRUE,
    teal_show_js_log = FALSE
  )

  op <- options()
  toset <- !(names(teal_default_options) %in% names(op))
  if (any(toset)) options(teal_default_options[toset])

  return(invisible())
}

.onAttach <- function(libname, pkgname) { # nolint
  packageStartupMessage(
    "\nteal version ",
    utils::packageDescription(
      pkg = pkgname,
      lib.loc = libname,
      fields = "Version"
    )
  )
}
