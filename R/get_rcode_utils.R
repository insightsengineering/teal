#' Generates library calls from current session info
#'
#' Function to create multiple library calls out of current session info to make reproducible code works.
#'
#' @return Character object contain code
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
    paste0(collapse = "\n")
}



get_rcode_str_install <- function() {
  code_string <- getOption("teal.load_nest_code")

  if (!is.null(code_string) && is.character(code_string)) {
    return(code_string)
  }

  return("# Add any code to install/load your NEST environment here")
}

#' Get datasets code
#'
#' Get combined code from `FilteredData` and from `CodeClass` object.
#'
#' @param datanames (`character`) names of datasets to extract code from
#' @param datasets (`FilteredData`) object
#' @param hashes named (`list`) of hashes per dataset
#'
#' @return `character(3)` containing following elements:
#'  - code from `CodeClass` (data loading code)
#'  - hash check of loaded objects
#'
#' @keywords internal
get_datasets_code <- function(datanames, datasets, hashes) {
  str_code <- datasets$get_code(datanames)
  if (length(str_code) == 0 || (length(str_code) == 1 && str_code == "")) {
    str_code <- "message('Preprocessing is empty')"
  } else if (length(str_code) > 0) {
    str_code <- paste0(str_code, "\n\n")
  }

  if (!datasets$get_check()) {
    check_note_string <- paste0(
      c(
        "message(paste(\"Reproducibility of data import and preprocessing was not explicitly checked\",",
        "   \" ('check = FALSE' is set). Contact app developer if this is an issue.\n\"))"
      ),
      collapse = "\n"
    )
    str_code <- paste0(str_code, "\n\n", check_note_string)
  }

  str_hash <- paste(
    paste0(
      vapply(
        datanames,
        function(dataname) {
          sprintf(
            "stopifnot(%s == %s)",
            deparse1(bquote(rlang::hash(.(as.name(dataname))))),
            deparse1(hashes[[dataname]])
          )
        },
        character(1)
      ),
      collapse = "\n"
    ),
    "\n\n"
  )

  c(str_code, str_hash)
}
