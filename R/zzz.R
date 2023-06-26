.onLoad <- function(libname, pkgname) { # nolint
  # adapted from https://github.com/r-lib/devtools/blob/master/R/zzz.R
  teal_default_options <- list(teal.show_js_log = FALSE)

  op <- options()
  toset <- !(names(teal_default_options) %in% names(op))
  if (any(toset)) options(teal_default_options[toset])

  options("shiny.sanitize.errors" = FALSE)

  # Set up the teal logger instance
  teal.logger::register_logger("teal")

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

# Use non-exported function(s) from teal.slice.
# This is a temporary measure and will be removed two release cycles from now (now meaning 0.13.0).
as.teal_slices <- getFromNamespace("as.teal_slices", "teal.slice") # nolint
# This one is here because there are numerous instances of getting all ids from teal_slices.
# The alternative is to always call vapply(<X>, `[[`, character(1), "id").
slices_field <- getFromNamespace("slices_field", "teal.slice") # nolint
