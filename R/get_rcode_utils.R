#' Generates library calls from current session info
#'
#' Function to create multiple library calls out of current session info to ensure reproducible code works.
#'
#' @return Character vector of `library(<package>)` calls.
#' @keywords internal
get_rcode_libraries <- function() {
  libraries <- vapply(
    utils::sessionInfo()$otherPkgs,
    function(x) {
      paste0("library(", x$Package, ")")
    },
    character(1)
  )
  paste0(paste0(rev(libraries), sep = "\n"), collapse = "")
}


#' @noRd
#' @keywords internal
get_rcode_str_install <- function() {
  code_string <- getOption("teal.load_nest_code")
  if (is.character(code_string)) {
    code_string
  } else {
    "# Add any code to install/load your NEST environment here\n"
  }
}
