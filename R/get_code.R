#' Get code
#'
#' Reads code from specified files, including code from source. Method reads code without
#' \code{library()} or \code{require()} calls. Function created for teal app, but can be used with any file.
#' @param files_path (\code{character}) (optional) vector of files path, if \code{NULL} then current file is used
#' @param starts_at (\code{character}) regular expression determining start of the block of code
#' @param stops_at (\code{character}) regular expression determining stop  of the block of code
#' @param exclude_comments (\code{logical}) whether exclude commented-out lines of code
#' @param read_sources (\code{logical}) whether to replace \code{source("path")} with code lines from sourced file.
#'
#' @return (\code{character}) code of import and preparation of data for teal application.
#'
#' @export
get_code <- function(files_path,
                     starts_at = "#\ @start_code",
                     stops_at = "#\ @end_code",
                     exclude_comments = TRUE,
                     read_sources = TRUE) {
  stopifnot(is.character(files_path) && length(files_path) >= 1)
  stopifnot(is.character(starts_at), length(starts_at) == 1)
  stopifnot(is.character(stops_at), length(stops_at) == 1)
  stopifnot(is.logical(exclude_comments), length(exclude_comments) == 1)
  stopifnot(is.logical(read_sources), length(read_sources) == 1)

  sapply(
    files_path,
    get_code_single,
    starts_at = starts_at,
    stops_at = stops_at,
    exclude_comments = exclude_comments,
    read_sources = read_sources
  ) %>%
    paste0(., collapse = "\n")

}

#' Get code
#'
#' Get code from specified file.
#' @param file_path (\code{character}) path of the file to be parsed
#' @inheritParams get_code
#'
#' @return code (\code{character}) preprocessing code
get_code_single <- function(file_path,
                            starts_at,
                            stops_at,
                            exclude_comments,
                            read_sources) {
  stopifnot(is.character(file_path), length(file_path) == 1)
  stopifnot(file.exists(file_path))
  stopifnot(is.logical(exclude_comments), length(exclude_comments) == 1)
  stopifnot(is.logical(read_sources), length(read_sources) == 1)

  lines <- readLines(file_path)

  # clean from comments and exclusions
  lines <- lines %>%
    enclosed_with(., starts_at = starts_at, stops_at = stops_at) %>%
    code_exclude(., exclude_comments = exclude_comments) %>%
    code_remove_library(.)

  if (read_sources) {
    lines <- include_source_lines(
      lines = lines,
      starts_at = starts_at,
      stops_at = stops_at,
      exclude_comments = exclude_comments,
      dir = dirname(file_path)
    )
  }


  lines
}

#' Name of executed file
#'
#' Assumes name of executed teal app using Rstudio API or commandArgs.
get_filename <- function() {
  if (rstudioapi::isAvailable()) {
    # if called in RStudio
    context <- rstudioapi::getActiveDocumentContext()
    if (context$path == "") {
      stop("Cannot automatically get preprocessing code when executed from RStudio console. Please either execute from app.R file or provide code as an argument.") #nolint
    } else {
      context$path
    }

  } else if (any(grepl("--file=", commandArgs()))) {
    # if called by Rscript
    commandArgs() %>%
      grep("--file=", x = ., value = TRUE) %>%
      gsub("--file=", "", x = .)

  } else if (Sys.info()["nodename"] == "rkaub00459.kau.roche.com") {
    # if called from BEE
    list.files(pattern = "^app\\.R$")[1]

  } else {
    # application executed outstide RStudio
    list.files(pattern = "^app\\.R$")[1]
  }
}


#' Exclude from code
#'
#' Excludes lines from code. It is possible to exclude one line ended by \code{# nocode}
#' @param lines (\code{character}) of code as seperate element in vector
#' @inheritParams get_code
code_exclude <- function(lines, exclude_comments) {
  stopifnot(is.character(lines), length(lines) >= 1)
  stopifnot(is.logical(exclude_comments), length(exclude_comments) == 1)

  no_preproc_single <- grep("^.+#[[:space:]]*nocode", lines)
  no_preproc_start  <- grep("[[:space:]]*#[[:space:]]*nocode[[:space:]]*>+", lines)
  no_preproc_stop   <- grep("[[:space:]]*#[[:space:]]*<+[[:space:]]*nocode[[:space:]]*", lines)

  if (length(no_preproc_start) != length(no_preproc_stop)) {
    stop(paste("Unequal number of no-code starts and stops in ", file_path))
  }

  no_preproc_multi <- NULL
  if (length(no_preproc_start) > 0) {
    no_preproc_multi <- unlist(Map(seq, from = no_preproc_start, to = no_preproc_stop))
  }

  no_preproc <- c(no_preproc_single, no_preproc_multi)

  if (length(no_preproc) > 0) {
    lines <- lines[-no_preproc]
  }

  if (exclude_comments) {
    lines <- grep("^\\s*#.+$", x = lines, invert = TRUE, value = TRUE)
    lines <- gsub("(^\\s*#.+$)|(#[^\'\"]*$)", "", x = lines, perl = TRUE)
  }

  lines
}

#' Remove library call from code
#'
#' Removes \code{library()} or \code{require()} calls from code
#' @inheritParams code_exclude
code_remove_library <- function(lines) {
  stopifnot(is.character(lines), length(lines) >= 1)
  gsub("(library|require)\\([^\\(\\)]+\\)", "", lines)
}


#' Get lines enclosed in
#'
#' Extracts lines from code which are enclosed within regexp starts_at and stops_at
#' @inheritParams get_code
#' @inheritParams code_exclude
enclosed_with <- function(lines,
                          starts_at,
                          stops_at) {
  stopifnot(is.character(lines), length(lines) >= 1)
  stopifnot(is.character(starts_at), length(starts_at) == 1)
  stopifnot(is.character(stops_at), length(stops_at) == 1)

  # set beginning of preprocessing
  idx_start <- grep(starts_at, lines)
  line_starts <- if (length(idx_start) > 1) {
    warning("More than one preproc start found - using the first one.")
    idx_start[1]
  } else if (length(idx_start) == 1) {
    idx_start
  } else {
    1L
  }

  idx_stop <- grep(stops_at, lines)
  line_stops <- if (length(idx_stop) > 1) {
    warning("More than one preproc stops found - using the last one.")
    tail(idx_stop, 1)
  } else if (length(idx_stop) == 1) {
    idx_stop
  } else {
    length(lines)
  }

  line_numbers <- seq(line_starts, line_stops)

  lines[line_numbers]
}

#' Finds lines with source call
#'
#' Finds lines where \code{source()} call is located
#' @inheritParams code_exclude
find_source_lines <- function(lines) {
  stopifnot(is.character(lines), length(lines) >= 1)
  idx <- grep("^[^#]*source\\([\'\"]([A-Za-z0-9_/.]+\\.R)[\"\']\\)+.*$", lines)

  if (length(idx) == 0) return(idx)

  if (any(grepl("source\\([^)]*chdir\\s*=\\s*T(RUE)*", x = lines[idx]))) {
    stop("Preprocessing doesn't handle source(chdir = TRUE)")
  }

  if (any(grepl("source\\(.+;\\s*source\\(", x = lines[idx]))) {
    stop("Preprocessing doesn't handle multiple sources in one line\n")
  }

  idx
}


#' Includes source in code lines
#'
#' Includes source in code lines
#' @inheritParams get_code
#' @inheritParams code_exclude
#' @return lines of code with source text included
include_source_lines <- function(lines,
                                 starts_at,
                                 stops_at,
                                 exclude_comments,
                                 dir) {
  stopifnot(is.character(lines), length(lines) >= 1)
  stopifnot(is.character(starts_at), length(starts_at) == 1)
  stopifnot(is.character(stops_at), length(stops_at) == 1)
  stopifnot(is.logical(exclude_comments), length(exclude_comments) == 1)

  idx <- find_source_lines(lines)

  if (length(idx) == 0) {
    return(lines)
  }

  sources_path <- gsub("source\\(.*[\"\']([A-Za-z0-9_/.]+)[\"\'].+$", "\\1", lines[idx])

  if (length(sources_path) != length(idx)) {
    stop("Couldn't detect R file name from source() call.")
  }

  sources_path <- ifelse(grepl("^/", sources_path), sources_path, file.path(dir, sources_path))
  if (!all(file.exists(sources_path))) {
    msg <- paste0("File(s) provided in the source() calls don't exist: \n",
                  paste(sources_path[!file.exists(sources_path)], collapse = "\n"))
    stop(msg)
  }

  sources_path <- normalizePath(sources_path)

  sources_code <- lapply(sources_path, function(s) {
    get_code_single(
      file_path = s,
      starts_at = starts_at,
      stops_at  = stops_at,
      exclude_comments = exclude_comments,
      read_sources = TRUE
    ) %>%
      c(sprintf("# Beginning of the source() - %s", s), ., sprintf("# End of the source - %s", s))
  })

  lines[idx] <- sources_code
  lines <- unlist(lines)

  lines
}
