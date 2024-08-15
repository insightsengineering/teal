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

#' Include `JS` files from `/inst/js/` package directory to application header
#'
#' `system.file` should not be used to access files in other packages, it does
#' not work with `devtools`. Therefore, we redefine this method in each package
#' as needed. Thus, we do not export this method
#'
#' @param pattern (`character`) pattern of files to be included, passed to `system.file`
#' @param except (`character`) vector of basename filenames to be excluded
#'
#' @return HTML code that includes `JS` files.
#' @keywords internal
include_js_files <- function(pattern = NULL, except = NULL) {
  checkmate::assert_character(except, min.len = 1, any.missing = FALSE, null.ok = TRUE)
  js_files <- list.files(system.file("js", package = "teal", mustWork = TRUE), pattern = pattern, full.names = TRUE)
  js_files <- js_files[!(basename(js_files) %in% except)] # no-op if except is NULL

  singleton(lapply(js_files, includeScript))
}

#' Run `JS` file from `/inst/js/` package directory
#'
#' This is triggered from the server to execute on the client
#' rather than triggered directly on the client.
#' Unlike `include_js_files` which includes `JavaScript` functions,
#' the `run_js` actually executes `JavaScript` functions.
#'
#' `system.file` should not be used to access files in other packages, it does
#' not work with `devtools`. Therefore, we redefine this method in each package
#' as needed. Thus, we do not export this method.
#'
#' @param files (`character`) vector of filenames.
#'
#' @return `NULL`, invisibly.
#' @keywords internal
run_js_files <- function(files) {
  checkmate::assert_character(files, min.len = 1, any.missing = FALSE)
  lapply(files, function(file) {
    shinyjs::runjs(paste0(readLines(system.file("js", file, package = "teal", mustWork = TRUE)), collapse = "\n"))
  })
  invisible(NULL)
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
    include_css_files(),
    # init.js is executed from the server
    include_js_files(except = "init.js"),
    shinyjs::hidden(icon("fas fa-gear")), # add hidden icon to load font-awesome css for icons
  )
}
