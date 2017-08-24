#' @title Generates header text for analysis items
#'
#' @param title A character title of the module
#' @param description A character description of the module with additional
#'   information not reflected in the title
#' @param libraries A character vector of the names of libraries required for
#'   the module. Defaults to c("ggplot2", "haven", "dplyr")
#' @param git_repo A character vector of the link to the GitHub repository
#'   containing the script for the module. Optional argument
#' @param data A character vector listing the code for data variable assignments,
#'   one for each dataset and with the file paths to the datasets
#'   (e.g. data = "ASL <- haven::read_sas("/opt/BIOSTAT/qa/cdt7876a/libraries/asl.sas7bdat")").
#'   Note that if any of the dataset paths are not found under /opt/BIOSTAT/...,
#'   then \code{output_header} would not generate the text corresponding to the
#'   code for reading in data, and user would need to manually add it within
#'   the module code. Defaults to c(), or no data lines generated
#'
#' @return A character string for the header text
#'
#' @export
#'
output_header <- function(title, description, libraries = c("ggplot2", "haven", "dplyr"), git_repo = NULL, data = c()) {

  descrip_str <- strsplit(description, "\n")[[1]] # In case of multi-line descriptions

  source_str <- ifelse(is.null(git_repo), "Not given", git_repo)

  if (dir.exists(file.path(getwd(), ".git"))) {
    git_commit <- system("git rev-parse --short HEAD", intern = TRUE)
  }

  date <- date()

  lib_str <- if (is.null(libraries)) { "" } else {
    paste0("library(", libraries, ")", collapse = "\n")
  }

  if (!is.null(data) & all(grepl("/opt/BIOSTAT/", data))) {

    # Need paths for checksums
    paths <- sub("(.*)[/]opt[/]BIOSTAT", "/opt/BIOSTAT", data)
    paths <- sub("[\"][)]$", "", paths)

    checksums <- tools::md5sum(paths)
    checksums_str <- paste0("  # MD5 checksum: ", checksums)

    data_str <- vapply(data, function(x) sub("=", "<-", x), character(1), USE.NAMES = FALSE)
    data_str <- paste0(data_str, checksums_str, collapse = "\n") # Add checksums as comments
  }

  header <- paste(paste0("\n# ", title, "\n"),
                  paste0(paste0("# ", descrip_str), collapse = "\n"),
                  paste0("# Source: ", source_str),
                  sep = "\n")

  if (exists("git_commit")) header <- paste(header, git_commit, sep = "\n")

  header <- paste(header,
                  paste0("# Date: ", date),
                  "#\n# You can run this code interactively in http://r.roche.com \n",
                  lib_str,
                  sep = "\n")

  if (exists("data_str")) header <- paste(header, data_str, sep = "\n")

  header
}

#' @title Generates text for the code to filter datasets
#'
#' @param datasets datasets used for the teal module
#'
#' @return A character string for the code to generate the filtered datasets
#'
#' @export
#'
get_filter_txt <- function(datasets) {

  datasets$get_filter_state("ASL", reactive = TRUE)
  datasets$get_filter_state("ARS", reactive = TRUE)
  str_filter_calls <- lapply(datasets$get_filter_call("ARS", merge = TRUE), deparse)

  paste(c(
    "# Filter Data",
    str_filter_calls[[1]], "",
    str_filter_calls[[2]],
    str_filter_calls[[3]]
  ), collapse = "\n")

}
