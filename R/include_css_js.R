#' Include \code{CSS} files from \code{/inst/css/} package directory to application header
#'
#' @param package (\code{character}) package name
#' @param pattern (\code{character}) pattern of files to be included
#'
#' @rturn HTML code that includes css files
#'
#' @export
include_css_files <- function(package = "teal", pattern = "*") {
  stopifnot(is_character_single(package))

  css_files <- list.files(system.file("css", package = package, mustWork = TRUE), pattern = pattern, full.names = TRUE)

  lapply(
    css_files,
    includeCSS
  )
}

#' Include \code{JS} files from \code{/inst/js/} package directory to application header
#'
#' @param package (\code{character}) package name
#' @param pattern (\code{character}) pattern of files to be included
#' @param except (\code{character}) vector of files to be excluded, matched via grep
#'
#' @export
include_js_files <- function(package = "teal", pattern = "*", except = NULL) {
  stopifnot(is_character_single(package))
  stopifnot(is.null(except) || is_character_vector(except))

  js_files <- list.files(system.file("js", package = package, mustWork = TRUE), pattern = pattern, full.names = TRUE)

  if (!is.null(except)) {
    for (file in except) {
      js_files <- grep(file, js_files, value = TRUE, invert = TRUE)
    }
  }

  # optionally put into `shiny::singleton`
  lapply(js_files, includeScript)
}

#' Run \code{JS} file from \code{/inst/js/} package directory
#'
#' This is triggered from the server to execute on the client
#' rather than triggered directly on the client.
#' Unlike `include_js_files` which includes Javascript functions,
#' the `run_js` actually executes Javascript functions.
#'
#' @param file (\code{character}) vector of file names
#' @param package (\code{character}) package name
#'
#' @export
run_js_files <- function(files, package = "teal") {
  stopifnot(is_character_vector(files))
  stopifnot(is_character_single(package))

  for (file in files) {
    # throws an error if file not found
    shinyjs::runjs(paste0(readLines(system.file("js", file, package = package, mustWork = TRUE)), collapse = "\n"))
  }
}

#' Code to include teal CSS and JS files
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
#'   include_teal_css_js(),
#'   p("Hello")
#' )
#' @export
include_teal_css_js <- function() {
  tagList(
    shinyjs::useShinyjs(),
    include_css_files(package = "teal"),
    # init.js is executed from the server
    include_js_files(package = "teal", except = "init.js"),
    shinyjs::hidden(icon("cog")), # add hidden icon to load font-awesome css for icons
  )
}
