#' Get code
#'
#' Reads code from specified files, including code from source. Method reads code without
#' \code{library()} or \code{require()} calls. Function created for teal app, but can be used with any file.
#' @param files_path (\code{character}) (optional) vector of files path to be read for preprocessing. Code from
#' multiple files is joined together.
#' @param exclude_comments (\code{logical}) whether exclude commented-out lines of code. Lines to be excluded
#' should be ended with \code{# nocode}. For multiple line exclusions one should enclose ignored block of code with
#' \code{# nocode>} and \code{# <nocode}
#' @param read_sources (\code{logical}) whether to replace \code{source("path")} with code lines from sourced file.
#' If \code{read_sources = TRUE} changing working directory inside preprocessing is not allowed.
#'
#' @return (\code{character}) code of import and preparation of data for teal application.
#'
#' @export
#' @importFrom magrittr %>%
get_code <- function(files_path,
                     exclude_comments = TRUE,
                     read_sources = TRUE) {
  stopifnot(is.character(files_path) && length(files_path) >= 1)
  stopifnot(is.logical(exclude_comments), length(exclude_comments) == 1)
  stopifnot(is.logical(read_sources), length(read_sources) == 1)

  lines <- lapply(
    files_path,
    get_code_single,
    exclude_comments = exclude_comments,
    read_sources = read_sources
  ) %>%
    unlist


  paste(lines, collapse = "\n")
}

#' Get code
#'
#' Get code from specified file.
#' @param file_path (\code{character}) path of the file to be parsed
#' @inheritParams get_code
#' @importFrom magrittr %>%
#'
#' @return lines (\code{character}) of preprocessing code
get_code_single <- function(file_path, exclude_comments, read_sources) {
  stopifnot(is.character(file_path), length(file_path) == 1)
  stopifnot(file.exists(file_path))
  stopifnot(is.logical(read_sources), length(read_sources) == 1)

  lines <- readLines(file_path) %>%
            enclosed_with() %>%
            code_exclude(exclude_comments = exclude_comments)

  if (read_sources) {
    lines <- include_source_code(
      lines = lines,
      exclude_comments = exclude_comments,
      dir = dirname(file_path)
    )
  }

  lines
}

#' Get code enclosed within
#'
#' Extracts lines from code which are enclosed within regexp starts_at and stops_at
#' @param lines (\code{character}) of preprocessing code.
#' @inheritParams get_code
enclosed_with <- function(lines) {
  stopifnot(is.character(lines), length(lines) >= 1)
  idx_start <- grep("#\\s*code>", lines)
  idx_stop <- grep("#\\s*<code", lines)

  if(length(idx_stop) == 0) {
    stop("All lines from file included. Please use #<code to stop preprocessing at indicated point.")
  }

  # set beginning of preprocessing
  line_starts <- if (length(idx_start) > 1) {
    warning("More than one preproc start found - using the first one.")
    idx_start[1] + 1
  } else if (length(idx_start) == 1) {
    idx_start + 1
  } else {
    1L
  }


  line_stops <- if (length(idx_stop) > 1) {
    warning("More than one preproc stops found - using the last one.")
    tail(idx_stop, 1) - 1
  } else if (length(idx_stop) == 1) {
    idx_stop - 1
  } else {
    length(lines)
  }

  line_numbers <- seq(line_starts, line_stops)

  lines[line_numbers]
}

#' Exclude from code
#'
#' Excludes lines from code. It is possible to exclude one line ended by \code{# nocode}
#' @inheritParams enclosed_with
#' @inheritParams get_code
#' @inheritParams get_code_single
code_exclude <- function(lines, exclude_comments, file_path) {
  stopifnot(is.character(lines), length(lines) >= 1)
  stopifnot(is.logical(exclude_comments), length(exclude_comments) == 1)

  nocode_single <- grep("^.+#[[:space:]]*nocode", lines)
  nocode_start  <- grep("[[:space:]]*#[[:space:]]*nocode[[:space:]]*>+", lines)
  nocode_stop   <- grep("[[:space:]]*#[[:space:]]*<+[[:space:]]*nocode[[:space:]]*", lines)

  if (length(nocode_start) != length(nocode_stop)) {
    stop(paste("Unequal number of no-code starts and stops in ", file_path)) #nolint
  }

  nocode_multi <- NULL
  if (length(nocode_start) > 0) {
    nocode_multi <- unlist(Map(seq, from = nocode_start, to = nocode_stop))
  }

  nocode <- c(nocode_single, nocode_multi)

  if (length(nocode) > 0) {
    lines <- lines[-nocode]
  }

  if (exclude_comments) {
    lines <- grep("^\\s*#.+$", x = lines, invert = TRUE, value = TRUE)
    lines <- gsub("(^\\s*#.+$)|(#[^\'\"]*$)", "", x = lines, perl = TRUE)
  }

  #

  lines
}

#' Finds lines of code with source call
#'
#' Finds lines in preprocessing code where \code{source()} call is located
#' @inheritParams enclosed_with
find_source_code <- function(lines) {
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

#' Includes source in preprocessing code lines
#'
#' Includes source in preprocessing code lines
#' @inheritParams get_code
#' @inheritParams code_exclude
#' @param dir of the file where source is called from.
#' @return lines of code with source text included
#'
#' @importFrom magrittr %>%
include_source_code <- function(lines, exclude_comments, dir) {
  stopifnot(is.character(lines), length(lines) >= 1)
  stopifnot(dir.exists(dir))


  idx <- find_source_code(lines)

  if (length(idx) == 0) {
    return(lines)
  }

  sources_path <- gsub("source\\(.*[\"\']([A-Za-z0-9_/.]+)[\"\'].+$", "\\1", lines[idx])

  if (length(sources_path) != length(idx)) {
    stop("Couldn't detect R file name from source() call.")
  }

  sources_path <- ifelse(grepl("^(/)|^([\\])|^([A-Za-z]:)", sources_path), sources_path, file.path(dir, sources_path))
  if (!all(file.exists(sources_path))) {
    msg <- paste0("File(s) provided in the source() calls don't exist: \n",
                  paste(sources_path[!file.exists(sources_path)], collapse = "\n"))
    stop(msg)
  }

  sources_path <- normalizePath(sources_path)

  sources_code <- lapply(sources_path, function(s) {
    get_code_single(file_path = s,
                    exclude_comments = exclude_comments,
                    read_sources = TRUE)
  })

  lines[idx] <- sources_code
  lines <- unlist(lines)

  lines
}

#' Libraries names from preprocessing code
#'
#' Reads library names from preprocessing code
#' @inheritParams enclosed_with
#' @return libraries names loaded in preprocessing code
read_lib_names <- function(lines) {
  lib_calls <- unlist(
    regmatches(
      lines,
      gregexpr("(?=(library|require)\\([\"\' ]{0,2}).*?(?<=\\))", lines, perl = TRUE)
    )
  )
  if (length(lib_calls) == 0) {
    return(character())
  }
  lib_names <- gsub("[\"\'\\)\\(]",
                    "",
                    regmatches(lib_calls, gregexpr("\\(.*?\\)", lib_calls)),
                    perl = TRUE)

  lib_names
}
