#' Include `CSS` files from `/inst/css/` package directory to application header
#'
#' `system.file` should not be used to access files in other packages, it does
#' not work with `devtools`. Therefore, we redefine this method in each package
#' as needed. Thus, we do not export this method.
#'
#' @param pattern (`character`) pattern of files to be included
#'
#' @return HTML code that includes `CSS` files.
#' @keywords internal
include_css_files <- function(pattern = "*") {
  css_files <- list.files(
    system.file("css", package = "teal", mustWork = TRUE),
    pattern = pattern, full.names = TRUE
  )

  singleton(
    tags$head(lapply(css_files, includeCSS))
  )
}

#' Code to include `teal` `CSS` and `JavaScript` files
#'
#' This is useful when you want to use the same `JavaScript` and `CSS` files that are
#' used with the `teal` application.
#' This is also useful for running standalone modules in `teal` with the correct
#' styles.
#' Also initializes `shinyjs` so you can use it.
#'
#' Simply add `include_teal_css_js()` as one of the UI elements.
#' @return A `shiny.tag.list`.
#' @keywords internal
include_teal_css_js <- function() {
  tagList(
    shinyjs::useShinyjs(),
    include_css_files()
  )
}
