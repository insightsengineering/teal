#' Load current package from proper folder before running tests for shiny app using \code{shinytest} package
#'
#' @param package_name (\code{character}) name of the package where the test runs ins
#' @return An expression
#' @export
#'
#' @examples
#' if (interactive()) {
#'   # start of shinytest script
#'   eval(shinytest_load_pckg())
#'   # continue with shinytest script
#' }
#'
shinytest_load_pckg <- function(package_name = NULL) {
  pth_source <- file.path("..", "..", "..")
  pckg_dir <- file.path(pth_source, "00_pkg_src")
  if (dir.exists(pckg_dir)) {
    # for devtools::check (i.e. R CMD CHECK)
    pckg_path_name <- list.files(pckg_dir, full.names = TRUE)
    str <- paste0("\"", pckg_path_name, "\"")
    out <- parse(
      text = paste0("devtools::load_all(path = ", str, ", export_all = FALSE)"),
      keep.source = FALSE
    )
  } else if (file.exists(file.path(pth_source, "DESCRIPTION"))) {
    # for devtools::test, devtools::test_file, testthat::...
    out <- parse(
      text = paste0("devtools::load_all(\"", pth_source, "\", export_all = FALSE)"),
      keep.source = FALSE
    )
  } else {
    # for interactive() usage
    if (is.null(package_name)) {
      out <- parse(
        text = "library(devtools::as.package(\".\")$package, character.only = T)",
        keep.source = FALSE
      )
    } else {
      out <- parse(
        text = paste0("library(", package_name, ")"),
        keep.source = FALSE
      )
    }
  }
  return(out)
}
