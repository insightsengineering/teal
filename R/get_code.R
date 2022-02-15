#' Get code
#'
#' @description `r lifecycle::badge("stable")`
#' Reads code from specified files or an R6 object.
#'
#' \itemize{
#'   \item{if reading from R6: }{get the R code stored inside the object.}
#'   \item{if reading from files: }{
#'     Includes code from source if reading from files. Method reads code without
#'   }
#' }
#' `library()` or `require()` calls. Function created for teal app, but can be used with any file.
#' Get code from certain files and for specific datasets
#'
#' Reads code from specified files and specific code chunks.
#'
#' Code chunks are described with:
#'
#' \itemize{
#'   \item{to open chunk }{`#code>` or `#code ADSL>` or `#code ADSL ADTTE>`}
#'   \item{to close chunk }{`#<code` or `#<ADSL code` or `#<ADSL ADTTE code`}
#' }
#'
#' @param x ([`TealDatasetConnector`] or [`TealDataset`]). If of class `character` will be treated as file to read.
#' @param exclude_comments (`logical`) whether exclude commented-out lines of code. Lines to be excluded
#' should be ended with `# nocode`. For multiple line exclusions one should enclose ignored block of code with
#' `# nocode>` and `# <nocode`
#' @param read_sources (`logical`) whether to replace `source("path")` with code lines from sourced file.
#' If `read_sources = TRUE` changing working directory inside preprocessing is not allowed.
#' @param deparse (`logical`) whether return deparsed form of a call
#' @param files_path (`character`) (optional) vector of files path to be read for preprocessing. Code from
#' multiple files is joined together.
#' @param dataname (`character`) Name of dataset to return code for.
#' @param ... not used, only for support of S3
#' @export
#' @return (`character`) code of import and preparation of data for teal application.
get_code <- function(x, ...) {
  UseMethod("get_code")
}


# Getting code from R6 ====

#' @export
#' @rdname get_code
get_code.TealDatasetConnector <- function(x, deparse = TRUE, ...) {
  check_ellipsis(...)
  x$get_code(deparse = deparse)
}

#' @export
#' @rdname get_code
get_code.TealDataset <- function(x, deparse = TRUE, ...) {
  check_ellipsis(...)
  x$get_code(deparse = deparse)
}


#' @rdname get_code
#' @export
#' @examples
#' x1 <- dataset(
#'   x = data.frame(x = c(1, 2), y = c("a", "b"), stringsAsFactors = FALSE),
#'   keys = "y",
#'   dataname = "XY",
#'   code = "XY <- data.frame(x = c(1, 2), y = c('aa', 'bb'), stringsAsFactors = FALSE)",
#'   label = character(0)
#' )
#'
#' x2 <- dataset(
#'   x = data.frame(x = c(1, 2), y = c("a", "b"), stringsAsFactors = FALSE),
#'   keys = "y",
#'   dataname = "XYZ",
#'   code = "XYZ <- data.frame(x = c(1, 2), y = c('aa', 'bb'), stringsAsFactors = FALSE)",
#'   label = character(0)
#' )
#'
#' rd <- teal_data(x1, x2)
#'
#' get_code(rd)
#' get_code(rd, "XY")
#' get_code(rd, "XYZ")
get_code.TealDataAbstract <- function(x, dataname = character(0), deparse = TRUE, ...) { # nolint
  check_ellipsis(...)
  if (length(dataname) > 0) {
    if (any(!(dataname %in% x$get_datanames()))) {
      stop("The dataname provided does not exist")
    }
    x$get_code(dataname = dataname, deparse = deparse)
  } else {
    x$get_code(deparse = deparse)
  }
}

# Getting code from files ====

#' @rdname get_code
#' @export
get_code.default <- function(x,
                             exclude_comments = TRUE,
                             read_sources = TRUE,
                             deparse = FALSE,
                             files_path = NULL,
                             dataname = NULL,
                             ...) {
  if (!is.null(files_path)) {
    x <- files_path
  }

  check_ellipsis(...)
  checkmate::assert_character(x, min.len = 1, any.missing = FALSE)
  checkmate::assert_flag(exclude_comments)
  checkmate::assert_flag(read_sources)

  if (!methods::hasArg(dataname)) {
    l_lines <- lapply(x, function(file_path) {
      get_code_single(file_path, read_sources = read_sources) %>%
        enclosed_with() %>%
        code_exclude(lines, exclude_comments = exclude_comments)
    })
  } else {
    l_lines <- lapply(x, function(file_path) {
      get_code_single(file_path, read_sources = read_sources) %>%
        enclosed_with_dataname(dataname = dataname) %>%
        code_exclude(lines, exclude_comments = exclude_comments)
    })
  }

  lines <- l_lines %>% unlist()

  if (deparse) {
    return(paste(
      vapply(lines, FUN = deparse1, collapse = "\n", FUN.VALUE = character(1)),
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
#' @param file_path (`character`) path or URL address of the file to be parsed
#' @param if_url (`logical`) (optional) TRUE when URL address is provided
#' @inheritParams get_code
#'
#' @return lines (`character`) of preprocessing code
#' @keywords internal
get_code_single <- function(file_path, read_sources, if_url = grepl("^http[s]", file_path)) {
  checkmate::assert_string(file_path)
  if (!if_url) {
    if (!file.exists(file_path)) {
      stop(
        "Reading preprocessing code from ", file_path, " file failed. ",
        "Please double check if you saved your script."
      )
    }
  }
  checkmate::assert_flag(read_sources)
  checkmate::assert_flag(if_url)

  lines <- readLines(file_path)
  if (read_sources) {
    lines <- include_source_code(lines = lines, dir = `if`(if_url, NULL, dirname(file_path)))
  }

  lines
}

#' Get code enclosed within
#'
#' Extracts lines from code which are enclosed within regexp starts_at and stops_at
#' @param lines (`character`) of preprocessing code.
#' @return (`character`) subset of lines which start and end with preprocessing
#'   start and stop tags.
#' @keywords internal
enclosed_with <- function(lines) {
  checkmate::assert_character(lines, min.len = 1, any.missing = FALSE)

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
    utils::tail(idx_stop, 1) - 1
  } else if (length(idx_stop) == 1) {
    idx_stop - 1
  } else {
    length(lines)
  }

  line_numbers <- seq(line_starts, line_stops)

  lines[line_numbers]
}

#' Get code enclosed within
#'
#' Extracts lines from code which are enclosed within regexp starts_at and stops_at
#' @inheritParams enclosed_with
#' @param dataname (`character`) metadata for returned lines
#' @return  (`list`) list of lines and their numbers from certain chunks of code at the specific file.
#' @keywords internal
enclosed_with_dataname <- function(lines, dataname = NULL) {
  checkmate::assert_character(lines, min.len = 1, any.missing = FALSE)
  if (!checkmate::test_character(dataname, min.len = 1, any.missing = FALSE)) {
    dataname <- ""
  }
  dataname <- trimws(dataname)
  any_chunk <- any(grepl("#\\s*<?\\s*code", lines))

  if (any_chunk) {
    any_start <- any(grepl(sprintf("#\\s*code[\\sa-zA-Z_]*%s[\\sa-zA-Z_]*>", dataname), lines, perl = TRUE))
    any_stop <- any(grepl(sprintf("#\\s*<[\\sa-zA-Z_]*%s[\\sa-zA-Z_]*(?<![a-zA-Z])code", dataname), lines, perl = TRUE))

    if (!(any_start && any_stop)) {
      stop(sprintf("File doesn't contain code marked for this %1$s.\n
                   Please use # code %1$s> to indicate which lines should be extracted.", dataname))
    }
  }

  # set beginning of preprocessing
  idx_start <- grep(sprintf("#\\s*code(?:[\\sa-zA-Z_]*%s[\\sa-zA-Z_]*|[\\s]*)>", dataname), lines, perl = TRUE)
  line_starts <- if (length(idx_start) >= 1) {
    idx_start + 1
  } else {
    1L
  }

  # set stop of preprocessing
  idx_stop <- grep(
    sprintf("#\\s*<(?:[\\sa-zA-Z_]*%s[\\sa-zA-Z_]*|[\\s]*)(?<![a-zA-Z])code", dataname),
    lines,
    perl = TRUE
  )
  line_stops <- if (length(idx_stop) >= 1) {
    idx_stop - 1
  } else {
    length(lines)
  }

  if (length(line_starts) != length(line_stops) || any(line_starts > line_stops)) {
    stop("Number of #code> has to be the same as #<code")
  }


  ll <- data.frame(line_starts, line_stops)

  line_numbers <- apply(ll, 1, function(x) seq(x[1], x[2]))

  lines_taken <- as.integer(unlist(line_numbers))

  res_lines <- lines[lines_taken]

  return(res_lines)
}

#' Exclude from code
#'
#' Excludes lines from code. It is possible to exclude one line ended by `# nocode`
#' @inheritParams enclosed_with
#' @inheritParams get_code
#' @inheritParams get_code_single
#' @keywords internal
code_exclude <- function(lines, exclude_comments, file_path) {
  checkmate::assert_character(lines, min.len = 1, any.missing = FALSE)
  checkmate::assert_flag(exclude_comments)

  nocode_single <- grep("^.+#[[:space:]]*nocode", lines)
  nocode_start <- grep("[[:space:]]*#[[:space:]]*nocode[[:space:]]*>+", lines)
  nocode_stop <- grep("[[:space:]]*#[[:space:]]*<+[[:space:]]*nocode[[:space:]]*", lines)

  if (length(nocode_start) != length(nocode_stop)) {
    stop(paste("Unequal number of no-code starts and stops in ", file_path)) # nolint
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
#' Finds lines in preprocessing code where `source()` call is located
#' @inheritParams enclosed_with
#' @keywords internal
find_source_code <- function(lines) {
  checkmate::assert_character(lines, min.len = 1, any.missing = FALSE)
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
#' @inheritParams enclosed_with
#' @param dir of the file where source is called from.
#' @return lines of code with source text included
#' @keywords internal
include_source_code <- function(lines, dir = NULL) {
  checkmate::assert_character(lines, min.len = 1, any.missing = FALSE)
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
        msg <- paste0(
          "File(s) provided in the source() calls don't exist: \n",
          paste(s[!file.exists(s)], collapse = "\n")
        )
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
