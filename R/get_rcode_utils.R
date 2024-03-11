#' Generates library calls from current session info
#'
#' Function to create multiple library calls out of current session info to ensure reproducible code works.
#'
#' @return Character vector of `library(<package>)` calls.
#' @keywords internal
get_rcode_libraries <- function() {
  vapply(
    utils::sessionInfo()$otherPkgs,
    function(x) {
      paste0("library(", x$Package, ")")
    },
    character(1)
  ) %>%
    # put it into reverse order to correctly simulate executed code
    rev() %>%
    paste0(sep = "\n") %>%
    paste0(collapse = "")
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

#' Get datasets code
#'
#' Retrieve complete code to create, verify, and filter a dataset.
#'
#' @param datanames (`character`) names of datasets to extract code from
#' @param datasets (`FilteredData`) object
#' @param hashes named (`list`) of hashes per dataset
#'
#' @return Character string concatenated from the following elements:
#'  - data pre-processing code (from `data` argument in `init`)
#'  - hash check of loaded objects
#'  - filter code (if any)
#'
#' @keywords internal
get_datasets_code <- function(datanames, datasets, hashes) {
  # preprocessing code
  str_prepro <- attr(datasets, "preprocessing_code")
  if (length(str_prepro) == 0) {
    str_prepro <- "message('Preprocessing is empty')"
  } else {
    str_prepro <- paste(str_prepro, collapse = "\n")
  }

  # hash checks
  str_hash <- vapply(datanames, function(dataname) {
    sprintf(
      "stopifnot(%s == %s)",
      deparse1(bquote(rlang::hash(.(as.name(dataname))))),
      deparse1(hashes[[dataname]])
    )
  }, character(1))
  str_hash <- paste(str_hash, collapse = "\n")

  # filter expressions
  str_filter <- teal.slice::get_filter_expr(datasets, datanames)
  if (str_filter == "") {
    str_filter <- character(0)
  }

  # concatenate all code
  str_code <- paste(c(str_prepro, str_hash, str_filter), collapse = "\n\n")
  sprintf("%s\n", str_code)
}
