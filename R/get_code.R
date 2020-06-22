#' Get code
#'
#' Reads code from specified files or an R6 object.
#'
#' \itemize{
#'   \item{if reading from R6: }{get the R code stored inside the object.}
#'   \item{if reading from files: }{
#'     Includes code from source if reading from files. Method reads code without
#'   }
#' }
#' \code{library()} or \code{require()} calls. Function created for teal app, but can be used with any file.
#'
#' @param x (\code{object}) of class\link{RawDatasetConnector} or \link{NamedDataset}. If of
#'   class \code{character} will be treated as file to read.
#' @param exclude_comments (\code{logical}) whether exclude commented-out lines of code. Lines to be excluded
#' should be ended with \code{# nocode}. For multiple line exclusions one should enclose ignored block of code with
#' \code{# nocode>} and \code{# <nocode}
#' @param read_sources (\code{logical}) whether to replace \code{source("path")} with code lines from sourced file.
#' If \code{read_sources = TRUE} changing working directory inside preprocessing is not allowed.
#' @param deparse (\code{logical}) whether return deparsed form of a call
#' @param files_path (\code{character}) (optional) vector of files path to be read for preprocessing. Code from
#' multiple files is joined together.
#' @param ... not used, only for support of S3
#' @export
#' @return (\code{character}) code of import and preparation of data for teal application.
get_code <- function(x, ...) {
  UseMethod("get_code")
}


# Getting code from R6 ====

#' @export
#' @rdname get_code
get_code.RawDatasetConnector <- function(x, deparse = TRUE, ...) {
  x$get_code(deparse = deparse)
}

#' @export
#' @rdname get_code
get_code.NamedDataset <- function(x, deparse = TRUE, ...) {
  x$get_code(deparse = deparse)
}


#' @export
#' @rdname get_code
get_code.cdisc_data <- function(x, deparse = FALSE, ...) {
  if (deparse) {
    paste0(deparse(attr(x, "code"), width.cutoff = 80L), collapse = "\n")
  } else {
    attr(x, "code")
  }
}



# Getting code from files ====

#' @rdname get_code
#' @export
#' @importFrom magrittr %>%
get_code.default <- function(x,
                     exclude_comments = TRUE,
                     read_sources = TRUE, deparse = FALSE, files_path = NULL, ...) {
  if (!is.null(files_path)) {
    x <- files_path
  }

  stopifnot(is_character_vector(x))
  stopifnot(is_logical_single(exclude_comments))
  stopifnot(is_logical_single(read_sources))

  lines <- lapply(x, function(file_path)
    get_code_single(file_path, read_sources = read_sources) %>%
      enclosed_with() %>%
      code_exclude(lines, exclude_comments = exclude_comments)
  ) %>%
  unlist

  if (deparse) {
    return(paste(
      vapply(
        lines,
        function(x) {
          paste(
            deparse(x, width.cutoff = 80L),
            collapse = "\n"
          )
        },
        FUN.VALUE = character(1)
      ),
      collapse = "\n"
    ))
  } else {
    return(paste(lines, collapse = "\n"))
  }
}

# * Sub functions for getting code from files ====

#' Get code
#'
#' Get code from specified file.
#' @param file_path (\code{character}) path or URL address of the file to be parsed
#' @param if_url (\code{logical}) (optional) TRUE when URL address is provided
#' @inheritParams get_code
#'
#' @return lines (\code{character}) of preprocessing code
#'
#' @importFrom magrittr %>%
get_code_single <- function(file_path, read_sources, if_url = grepl("^http[s]", file_path)) {
  stopifnot(is_character_single(file_path))
  if (!if_url) {
    stop_if_not(list(
      file.exists(file_path),
      paste0("Reading preprocessing code from ", file_path, " file failed. ",
             "Please double check if you saved your script.")
    ))
  }
  stopifnot(is_logical_single(read_sources))
  stopifnot(is_logical_single(if_url))

  lines <- readLines(file_path)
  if (read_sources) {
    lines <- include_source_code(lines = lines, dir = `if`(if_url, NULL, dirname(file_path)))
  }

  lines
}

#' Get code enclosed within
#'
#' Extracts lines from code which are enclosed within regexp starts_at and stops_at
#' @param lines (\code{character}) of preprocessing code.
enclosed_with <- function(lines) {
  stopifnot(is_character_vector(lines))

  # set beginning of preprocessing
  idx_start <- grep("#\\s*code>", lines)
  line_starts <- if (length(idx_start) > 1) {
    warning("More than one preproc start found - using the first one.")
    idx_start[1] + 1
  } else if (length(idx_start) == 1) {
    idx_start + 1
  } else {
    1L
  }

  # set stop of preprocessing
  idx_stop <- grep("#\\s*<code", lines)
  line_stops <- if (length(idx_stop) > 1) {
    warning("More than one preproc stops found - using the last one.")
    tail(idx_stop, 1) - 1
  } else if (length(idx_stop) == 1) {
    idx_stop - 1
  } else {
    stop("All lines from file included by get_code(). Please use #<code to stop preprocessing at indicated point")
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
  stopifnot(is_character_vector(lines))
  stopifnot(is_logical_single(exclude_comments))

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

  lines
}

#' Finds lines of code with source call
#'
#' Finds lines in preprocessing code where \code{source()} call is located
#' @inheritParams enclosed_with
find_source_code <- function(lines) {
  stopifnot(is_character_vector(lines))
  idx <- grep("^[^#]*source\\([\'\"]([A-Za-z0-9_/.]).*\\.R[\'\"].*\\).*$", lines)

  if (length(idx) == 0) {
    return(idx)
  }

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
include_source_code <- function(lines, dir = NULL) {
  stopifnot(is_character_vector(lines))
  stopifnot(is.null(dir) || dir.exists(dir))


  idx <- find_source_code(lines)

  if (length(idx) == 0) {
    return(lines)
  }

  sources_path <- vapply(
    lines[idx],
    function(x) {
      res <- gsub("source\\(.*[\"\']([A-Za-z0-9_/.])", "\\1", strsplit(x, ",")[[1]][1])
      res <- gsub("[\'\"]", "", res)
      res <- gsub(")", "", res)
      res
    },
    character(1)
  ) %>%
    unname()

  if (length(sources_path) != length(idx)) {
    stop("Couldn't detect R file name from source() call.")
  }

  sources_code <- lapply(sources_path, function(s) {
    if (grepl("^http[s]", s)) {
      # url detected - do nothing
    } else {
      s <- ifelse(grepl("^(/)|^([\\])|^([A-Za-z]:)", s), s, file.path(dir, s))
      if (!all(file.exists(s))) {
        msg <- paste0("File(s) provided in the source() calls don't exist: \n",
                      paste(s[!file.exists(s)], collapse = "\n"))
        stop(msg)
      }

      s <- normalizePath(s)
    }

    get_code_single(file_path = s, read_sources = TRUE)
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

#' Read .R file into character
#'
#' Comments will be excluded
#'
#' @param file (\code{character}) File to be parsed into code
#'
#' @return character vector withe the code
#'
#' @export
#' @examples
#' file_example <- tempfile()
#' writeLines(c("x <- 2", "#second line comment", "x <- x + 2"), file_example)
#'
#' read_script(file_example)
#'
read_script <- function(file) {
  stopifnot(is_character_single(file))
  stopifnot(file.exists(file))
  get_code_single(file, read_sources = TRUE) %>%
    code_exclude(exclude_comments = TRUE) %>%
    paste(sep = "\n", collapse = "\n")
}
