#' Generates header text for analysis items
#'
#' @param title A character title of the module
#' @param description A character description of the module with additional
#'   information not reflected in the title
#' @param libraries A character vector of the names of libraries required for
#'   the module.
#' @param git_repo A character vector of the link to the GitHub repository
#'   containing the R-code for the teal module. Optional argument
#' @param data optionally either an FilteredData object, or a named list of
#'   datasets. The data sets need the \code{source} attribute and optionally the
#'   \code{md5sum} attribute.
#'
#' @return A character string for the header text

#' @import methods
#'
#' @export
#'
#' @examples
#'
#' cat(get_rcode_header(
#'  title = "Cross-Table",
#'  description = "A cross-table counts ..."
#' ))
#'
#' cat(get_rcode_header(
#'  title = "Cross-Table",
#'  description = "A cross-table counts ...",
#'  libraries = c('haven', 'ggplot2', 'dplyr')
#' ))
#'
#' cat(get_rcode_header(
#'  title = "Cross-Table",
#'  description = "A cross-table counts ...",
#'  libraries = c('haven', 'ggplot2', 'dplyr'),
#'  git_repo = "http://github.roche.com/Rpackages/teal/",
#'  data = list(
#'    ASL = structure(data.frame(a = 1), source = "haven::read_sas('/opt/BIOSTAT/asl.sas7bdat')"),
#'    ATE = structure(data.frame(a = 1),
#'                    source = "haven::read_sas('/opt/BIOSTAT/ate.sas7bdat')",
#'                    md5sum = "32sdf32fds324")
#'  )
#' ))
#'
get_rcode_header <- function(title, description = NULL, libraries = NULL, git_repo = NULL, data = NULL) {

  descrip_str <- description # unlist(strsplit(description, "\n")) # In case of multi-line descriptions

  source_str <- ifelse(is.null(git_repo), "Not given", git_repo)

  #git_commit <- try(system("git rev-parse --short HEAD", intern = TRUE), silent = TRUE)
  #if (is(git_commit, "try-error")) git_commit <- ""

  git_commit <- "-"

  lib_str <- if (!is.null(libraries)) {
    paste0("library(", libraries, ")", collapse = "\n")
  } else {
    ""
  }

  # get data import strings
  if (is(data, "FilteredData")) {
    data <- lapply(data$datanames(), function(dn) data$get_data(dn, filtered = FALSE, reactive = FALSE))
  }

  if (any(!vapply(data, is.data.frame, logical(1)))) {
    stop("data needs to be either a FilteredData object or a list of data.frames")
  }

  if (any(vapply(data, function(df) is.null(attr(df, "source")), logical(1)))) {
    stop("data needs source attribute")
  }

  data_str <- paste(Map(function(dt, name) {
    md5 <- attr(dt, "md5sum")
    source <- attr(dt, "source")

    md5_str <- if (!is.null(md5)) {
      paste("# md5sum at time of analysis:", md5)
    } else {
      ""
    }
    paste0(name, " <- ", source, "  ",  md5_str)
  }, data, names(data)), collapse = "\n")

  pad <- function(str, pre="", post = "\n\n") {
    if (is.null(str) || length(str) == 0 || str == "") str else paste0(pre, str, post)
  }

  commented <- gsub("\n", "\n# ", paste(c(
    paste("#", title), "\n\n",
    pad(description),
    pad(paste("Module Source:", source_str), post="\n"),
    pad(paste("Git-commit Shiny App:", git_commit), post="\n"),
    pad(paste("Date:", date()), post = "\n"),  "\n",
    "You can run this code interactively in http://r.roche.com"
  ), collapse = ""), fixed=TRUE)

  paste(c(pad(commented), pad(lib_str), data_str), collapse = "")

}

#' Generates text for the code to filter datasets
#'
#' @param datanames vector with data names
#' @param datasets datasets object
#'
#' @return A character string for the code to generate the filtered datasets
#'
#' @export
#'
get_filter_txt <- function(datanames, datasets) {

  if (any(!(datanames %in% datasets$datanames()))) {
    sel <- !(datanames %in% datasets$datanames())
    stop("datanames:", paste(datanames[sel], collapse = ", "), "are not available in datasets")
  }

  dnames <- c("ASL", setdiff(datanames, "ASL"))


  calls <-  lapply(dnames, function(dn) {
    cl <- if (dn == "ASL") {
      datasets$get_filter_call(dn, merge = FALSE, asl=TRUE)
    } else {
      datasets$get_filter_call(dn, merge = TRUE, asl=FALSE)
    }
    x <- if (is.list(cl)) {
      unlist(lapply(cl, deparse, width.cutoff = 80))
    } else {
      deparse(cl, width.cutoff = 80)
    }
    paste(x, collapse = "\n")
  })


  paste(calls, collapse = "\n")
}
