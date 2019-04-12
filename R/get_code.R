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

  # files_path <- c("tests/testthat/app_get_code/app.R", "inst/app.R");file_path <- files_path[1]
  code <- lapply(
    files_path,
    get_code_single,
    read_sources = read_sources
  ) %>%
    unlist %>%
    code_exclude(exclude_comments = exclude_comments) %>%
    code_libraries()


  structure(paste(code, collapse = "\n"),
            libs_excluded = attr(code, "libs_excluded"),
            libs_loaded   = attr(code, "libs_loaded"))
}

#' Get code
#'
#' Get code from specified file.
#' @param file_path (\code{character}) path of the file to be parsed
#' @inheritParams get_code
#' @importFrom magrittr %>%
#'
#' @return code (\code{character}) preprocessing code
get_code_single <- function(file_path, read_sources) {
  stopifnot(is.character(file_path), length(file_path) == 1)
  stopifnot(file.exists(file_path))
  stopifnot(is.logical(read_sources), length(read_sources) == 1)

  code <- readLines(file_path)
  code <- enclosed_with(code)

  if (read_sources) {
    code <- include_source_code(
      code = code,
      dir = dirname(file_path)
    )
  }

  code
}

#' Get code enclosed within
#'
#' Extracts lines from code which are enclosed within regexp starts_at and stops_at
#' @param code (\code{character}) containing preprocessing code lines.
#' @inheritParams get_code
enclosed_with <- function(code) {
  stopifnot(is.character(code), length(code) >= 1)

  # set beginning of preprocessing
  idx_start <- grep("#\\s*code>", code)
  line_starts <- if (length(idx_start) > 1) {
    warning("More than one preproc start found - using the first one.")
    idx_start[1]
  } else if (length(idx_start) == 1) {
    idx_start
  } else {
    1L
  }

  idx_stop <- grep("#\\s*<code", code)
  line_stops <- if (length(idx_stop) > 1) {
    warning("More than one preproc stops found - using the last one.")
    tail(idx_stop, 1)
  } else if (length(idx_stop) == 1) {
    idx_stop
  } else {
    length(code)
  }

  line_numbers <- seq(line_starts, line_stops)

  code[line_numbers]
}

#' Exclude from code
#'
#' Excludes lines from code. It is possible to exclude one line ended by \code{# nocode}
#' @param code (\code{code}) class object, containing preprocessing code and additional information from parsed files.
#' @inheritParams get_code
#' @inheritParams get_code_single
code_exclude <- function(code, exclude_comments, file_path) {
  stopifnot(is.character(code), length(code) >= 1)
  stopifnot(is.logical(exclude_comments), length(exclude_comments) == 1)

  excluded <- character()

  nocode_single <- grep("^.+#[[:space:]]*nocode", code)
  nocode_start  <- grep("[[:space:]]*#[[:space:]]*nocode[[:space:]]*>+", code)
  nocode_stop   <- grep("[[:space:]]*#[[:space:]]*<+[[:space:]]*nocode[[:space:]]*", code)

  if (length(nocode_start) != length(nocode_stop)) {
    stop(paste("Unequal number of no-code starts and stops in ", file_path)) #nolint
  }

  nocode_multi <- NULL
  if (length(nocode_start) > 0) {
    code_multi <- unlist(Map(seq, from = nocode_start, to = nocode_stop))
  }

  nocode <- c(nocode_single, nocode_multi)

  if (length(nocode) > 0) {
    libs_excluded <- read_lib_names(code[nocode])
    code <- code[-nocode]
  }

  if (exclude_comments) {
    code <- grep("^\\s*#.+$", x = code, invert = TRUE, value = TRUE)
    code <- gsub("(^\\s*#.+$)|(#[^\'\"]*$)", "", x = code, perl = TRUE)
  }

  #

  structure(code, libs_excluded = libs_excluded)
}

#' Remove library call from code
#'
#' Removes \code{library()} or \code{require()} calls from code
#' @inheritParams code_exclude
code_libraries <- function(code) {
  stopifnot(is.character(code), length(code) >= 1)
  libs_loaded <- unique(read_lib_names(code))

  code <- gsub("(library|require|devtools::load_all|load_all)\\([^\\(\\)]*\\)", "", code)
  attr(code, "libs_loaded") <- libs_loaded

  code
}

#' Finds code with source call
#'
#' Finds code where \code{source()} call is located
#' @inheritParams enclosed_with
find_source_code <- function(code) {
  stopifnot(is.character(code), length(code) >= 1)
  idx <- grep("^[^#]*source\\([\'\"]([A-Za-z0-9_/.]+\\.R)[\"\']\\)+.*$", code)

  if (length(idx) == 0) return(idx)

  if (any(grepl("source\\([^)]*chdir\\s*=\\s*T(RUE)*", x = code[idx]))) {
    stop("Preprocessing doesn't handle source(chdir = TRUE)")
  }

  if (any(grepl("source\\(.+;\\s*source\\(", x = code[idx]))) {
    stop("Preprocessing doesn't handle multiple sources in one line\n")
  }

  idx
}

#' Includes source in code lines
#'
#' Includes source in code lines
#' @inheritParams get_code
#' @inheritParams code_exclude
#' @param dir of the file where source is called from.
#' @return lines of code with source text included
#'
#' @importFrom magrittr %>%
include_source_code <- function(code, dir) {
  stopifnot(is.character(code), length(code) >= 1)
  stopifnot(dir.exists(dir))


  idx <- find_source_code(code)

  if (length(idx) == 0) {
    return(code)
  }

  sources_path <- gsub("source\\(.*[\"\']([A-Za-z0-9_/.]+)[\"\'].+$", "\\1", code[idx])

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
    code <- get_code_single(file_path = s, read_sources = TRUE)
    c(sprintf("# Beginning of the source() - %s", s), code, sprintf("# End of the source - %s", s))
  })

  code[idx] <- sources_code
  code <- unlist(code)

  code
}

#' Libraries names from code
#'
#' Reads library nammes from code
#' @inheritParams enclosed_with
#' @return libraries names loaded in preprocessing code
read_lib_names <- function(code) {
  lib_calls <- unlist(regmatches(code, gregexpr("(?=(library|require)\\([\"\']{0,1}).*?(?<=\\))", code, perl = TRUE)))
  if (length(lib_calls) == 0) {
    return(character())
  }
  lib_names <- gsub("[\"\'\\)\\(]",
                    "",
                    regmatches(lib_calls, gregexpr("\\(.*?\\)", lib_calls)),
                    perl = TRUE)

  lib_names
}
