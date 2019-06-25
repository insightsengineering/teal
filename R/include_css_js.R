#' Include \code{CSS} files from \code{/inst/css/} package directory to application header
#'
#' @param package (\code{character}) package name
#' @param pattern (\code{character}) pattern of files to be included
#'
#' @export
include_css_files <- function(package = "teal", pattern = "*") {
  stopifnot(is.character.single(package))

  list_files <- list.files(file.path(system.file(package = package), "css"), pattern = pattern, full.names = TRUE)

  lapply(
    list_files,
    includeCSS
  )
}

#' Include \code{JS} files from \code{/inst/js/} package directory to application header
#'
#' @param package (\code{character}) package name
#' @param pattern (\code{character}) pattern of files to be included
#' @param except (\code{character}) vector of files to be excluded
#'
#' @export
include_js_files <- function(package = "teal", pattern = "*", except = NULL) {
  stopifnot(is.character.single(package))
  stopifnot(is.null(except) || is.character.vector(except))

  list_files <- list.files(file.path(system.file(package = package), "js"), pattern = pattern, full.names = TRUE)

  if (!is.null(except)) {
    for (i in except) {
      list_files <- grep(i, list_files, value = TRUE, invert = TRUE)
    }
  }

  lapply(list_files, includeScript)
}

#' Run \code{JS} file from \code{/inst/js/} package directory
#'
#' @param file (\code{character}) vector of file names
#' @param package (\code{character}) package name
#'
#' @export
run_js_file <- function(file, package = "teal") {
  stopifnot(is.character.vector(file))
  stopifnot(is.character.single(package))

  for (i in file) {
    shinyjs::runjs(paste0(readLines(system.file("js", i, package = package)), collapse = "\n"))
  }
}
