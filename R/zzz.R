.onLoad <- function(libname, pkgname) { # nolint
  # adapted from https://github.com/r-lib/devtools/blob/master/R/zzz.R
  teal_default_options <- list(
    teal.show_js_log = FALSE,
    teal.threshold_slider_vs_checkboxgroup = 5
  )

  op <- options()
  toset <- !(names(teal_default_options) %in% names(op))
  if (any(toset)) options(teal_default_options[toset])

  options("shiny.sanitize.errors" = FALSE)

  # expose default CDISC dataset names
  # copy from excel file
  default_cdisc_keys <- yaml::yaml.load_file(
    get_package_file("teal", "cdisc_datasets/cdisc_datasets.yaml")
  ) # nolint
  assign("default_cdisc_keys", default_cdisc_keys, envir = parent.env(environment()))

  # Set up the teal logger instance
  register_logger("teal")

  invisible()
}

.onAttach <- function(libname, pkgname) { # nolint
  packageStartupMessage(
    "\nYou are using teal version ",
    # `system.file` uses the `shim` of `system.file` by `teal`
    # we avoid `desc` dependency here to get the version
    read.dcf(system.file("DESCRIPTION", package = "teal"))[, "Version"]
  )
}
