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
    paste0(sep = "\n") %>%
    paste0(collapse = "")
}



get_rcode_str_install <- function() {
  code_string <- getOption("teal.load_nest_code")

  if (!is.null(code_string) && is.character(code_string)) {
    return(code_string)
  }

  return("# Add any code to install/load your NEST environment here\n")
}

#' Get datasets code
#'
#' Get combined code from `FilteredData` and from `CodeClass` object.
#'
#' @param datanames (`character`) names of datasets to extract code from
#' @param datasets (`FilteredData`) object
#' @param hashes named (`list`) of hashes per dataset
#'
#' @return `character(3)` containing the following elements:
#'  - data pre-processing code (from `data` argument in `init`)
#'  - hash check of loaded objects
#'  - filter code
#'
#' @keywords internal
get_datasets_code <- function(datanames, datasets, hashes) {
  str_prepro <- teal.data:::get_code_dependency(attr(datasets, "preprocessing_code"), names = datanames)
  if (length(str_prepro) == 0) {
    str_prepro <- "message('Preprocessing is empty')"
  } else if (length(str_prepro) > 0) {
    str_prepro <- paste0(str_prepro, "\n\n")
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

  str_filter <- teal.slice::get_filter_expr(datasets, datanames)

  c(str_prepro, str_hash, str_filter)
}
