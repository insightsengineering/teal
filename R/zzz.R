.onLoad <- function(libname, pkgname) { #nolint
  # adapted from https://github.com/r-lib/devtools/blob/master/R/zzz.R
  teal_default_options <- list(
    teal_logging = TRUE,
    teal_show_js_log = FALSE
  )

  op <- options()
  toset <- !(names(teal_default_options) %in% names(op))
  if (any(toset)) options(teal_default_options[toset])

  options("shiny.sanitize.errors" = FALSE)

  # expose default CDISC dataset names
  # copy from excel file
  default_cdisc_keys <- yaml::yaml.load_file(
    utils.nest::get_package_file("teal", "cdisc_datasets/cdisc_datasets.yaml")) #nolint
  assign("default_cdisc_keys", default_cdisc_keys, envir = parent.env(environment()))

  # Fixes https://github.com/insightsengineering/teal/issues/210
  # R versions < 4.1 don't have the access to the updated version of S4Vectors
  # hence we need to copy the fix explicitly
  normSubsetIndex <- function(i) { # nolint
    i <- try(as.logical(i), silent = TRUE)
    if (inherits(i, "try-error"))
      stop("'subset' must be coercible to logical")
    i & !is.na(i)
  }
  evalqForSubset <- function(expr, envir, ...) { # nolint
    if (methods::missingArg(substitute(expr), parent.frame())) {
      rep(TRUE, NROW(envir))
    } else {
      i <- evalArg(substitute(expr), envir, ..., where = parent.frame())
      normSubsetIndex(i)
    }
  }
  R.utils::reassignInPackage(name = "evalqForSubset", pkgname = "S4Vectors", value = evalqForSubset())

  return(invisible())
}

.onAttach <- function(libname, pkgname) { # nolint
  packageStartupMessage(
    "\nYou are using teal version ",
    # `system.file` uses the `shim` of `system.file` by `teal`
    # we avoid `desc` dependency here to get the version
    read.dcf(system.file("DESCRIPTION", package = "teal"))[, "Version"]
  )
}
