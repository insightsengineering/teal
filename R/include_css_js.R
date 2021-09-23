#' Include `CSS` files from `/inst/css/` package directory to application header
#'
#' `system.file` should not be used to access files in other packages, it does
#' not work with `devtools`. Therefore, we redefine this method in each package
#' as needed. Thus, we do not export this method
#'
#' @param pattern (`character`) pattern of files to be included
#' @param package (`character`) name of package that contains the files to be included
#'
#' @return HTML code that includes `CSS` files
#'
#' @export
include_css_files <- function(pattern = "*", package = "teal") {
  css_files <- list.files(
    system.file("css", package = package, mustWork = TRUE),
    pattern = pattern,
    full.names = TRUE
  )
  return(singleton(lapply(css_files, includeCSS)))
}


#' Include `JS` files from `/inst/js/` package directory to application header
#'
#' `system.file` should not be used to access files in other packages, it does
#' not work with `devtools`. Therefore, we redefine this method in each package
#' as needed. Thus, we do not export this method
#'
#' @param pattern (`character`) pattern of files to be included, passed to `system.file`
#' @param package (`character`) package name of files to be included
#' @param except (`character`) vector of basename filenames to be excluded
#'
#' @return HTML code that includes `JS` files
#'
#' @export
include_js_files <- function(pattern = "*", package = "teal", except = NULL) {
  stopifnot(is.null(except) || is_character_vector(except))

  js_files <- list.files(system.file("js", package = package, mustWork = TRUE), pattern = pattern, full.names = TRUE)
  js_files <- js_files[!(basename(js_files) %in% except)] # no-op if except is NULL

  return(singleton(lapply(js_files, includeScript)))
}

#' Run `JS` file from `/inst/js/` package directory
#'
#' This is triggered from the server to execute on the client
#' rather than triggered directly on the client.
#' Unlike `include_js_files` which includes Javascript functions,
#' the `run_js` actually executes Javascript functions.
#'
#' `system.file` should not be used to access files in other packages, it does
#' not work with `devtools`. Therefore, we redefine this method in each package
#' as needed. Thus, we do not export this method
#'
#' @param files (`character`) vector of filenames
run_js_files <- function(files) {
  stopifnot(is_character_vector(files))

  lapply(files, function(file) {
    shinyjs::runjs(paste0(readLines(system.file("js", file, package = "teal", mustWork = TRUE)), collapse = "\n"))
  })
  return(invisible(NULL))
}

#' Code to include teal CSS and Javascript files
#'
#' This is useful when you want to use the same Javascript and CSS files that are
#' used with the teal application.
#' This is also useful for running standalone modules in teal with the correct
#' styles.
#' Also initializes `shinyjs` so you can use it.
#'
#' @return HTML code to include
#' @examples
#' shiny_ui <- tagList(
#'   teal:::include_teal_css_js(),
#'   p("Hello")
#' )
include_teal_css_js <- function() {
  tagList(
    shinyjs::useShinyjs(),
    include_css_files(),
    # init.js is executed from the server
    include_js_files(except = "init.js"),
    shinyjs::hidden(icon("cog")), # add hidden icon to load font-awesome css for icons
  )
}
