#' @title Parse Code Chunks
#'
#' @description Save code chunks between specified (a) start and end tags for
#'   multi-line chunks (using '@start_' and '@end_') and (b) one-line tags for
#'   single-line chunks (using '@oneline_') from a code script as a list of
#'   individual code chunks, each named according to the code chunk name
#'   specified in the corresponding tag
#'
#' @param file path to the file containing the code chunks
#' @param reindent boolean value for whether code chunks should be reindented
#'   such that the first line of each code chunk does not start with whitespaces.
#'   Defaults to TRUE
#'
#' @return A list of individual code chunks, with each element consisting
#'   of a character vector of length the number of lines from `file`
#'   that corresponds to the specific code chunk
#'
#' @details Code chunks should be non-overlapping, start with a valid start tag
#'   beginning after '#' (e.g. "# @start_chunk1", "# @oneline_chunk2") and,
#'   in the case of multi-line code chunks, end with a valid end tag
#'   (e.g. "# @end_chunk1"), with non-duplicated code chunk names that
#'   adhere to variable naming rules in R.
#'   Otherwise, the function will throw an error
#'
#' @export
#'
#' @examples
#'
#' file <- tempfile()
#'
#' cat("
#' # @start_aaa
#' x <- 10
#' y <- -10
#' x*y
#' # @end_aaa
#'
#' #  @start_aaa_part2
#' x <- data.frame(y = 1:3, z = 1:3)
#' x$y == x$z
#' #@end_aaa_part2
#'
#' # @oneline_ccc
#' plot(1:100)
#'
#' x <- c(\"abc\", \"abcd\")     # @start_bbb some text
#' y <- pi * 2
#' round(y, 2)
#' # @end_bbb more text
#'
#' #@start_ not part of a chunk
#' plot(1:10) #@end_ also not part of a chunk
#'    ", file = file, append = FALSE)
#'
#' parse_code_chunks(file, reindent = TRUE)
#'
parse_code_chunks <- function(file, reindent = TRUE) {

  if (!file.exists(file)) stop("File does not exist.")

  con <- file(file)
  code <- readLines(con, warn = FALSE)
  close(con)


  re <- "#[[:space:]]*"
  re_start <- paste0(re, "@start_")
  re_end <- paste0(re, "@end_")
  re_oneline <- paste0(re, "@oneline_")

  # Multi-line code chunks with start and end tags
  is_match_multi <- grepl(paste0("(^|[[:space:]]*)", re_start, "\\S"), code,
                          ignore.case = TRUE)

  matched_obj_multi <- if (any(is_match_multi)) {

    Map(function(i) {

      m <- code[i]

      # Extract name
      name <- sub("[[:space:]](.*)", "", sub(paste0("(.*?)", re_start), "", m))

      # Get end
      has_end <- grepl(paste0(re_end, name, "($|[[:space:]])"), code, ignore.case = TRUE)

      nmatches <- sum(has_end)


      if (nmatches == 1) {

        list(
          name = name,
          start = i,
          end = which(has_end)
        )

      } else {

        if (nmatches == 0) {

          stop(paste("Chunk", name, "has no ending"))

        } else {

          stop(paste("Chunk", name, "has multiple ending"))

        }
      }
    }, which(is_match_multi))

  } else {

    NULL

  }

  # Get multi-line code chunk starting and ending indices
  starts <- vapply(matched_obj_multi, function(x) x$start, numeric(1))
  ends <- vapply(matched_obj_multi, function(x) x$end, numeric(1))
  starts_ends <- mapply(c, starts, ends)


  # Single-line code chunk starting indices
  match_single <- grep(paste0("(^|[[:space:]]*)", re_oneline, "\\S"), code,
                           ignore.case = TRUE)

  # Need to ensure the tag does not start on the same line as
  # a valid start or end tag from multi-line code chunks
  match_single <- match_single[!(match_single %in% ends) & !(match_single %in% starts)]

  matched_obj_single <- if (length(match_single) != 0) {

    Map(function(i) {

        m <- code[i]

        # Extract name
        name <- sub("[[:space:]](.*)", "", sub(paste0("(.*?)", re_oneline), "", m))

        list(
          name = name,
          start = i
        )


  }, match_single) } else {

    NULL

  }


  # Checking validity of input
  if (any(vapply(matched_obj_multi, function(x) {name <- x$name; make.names(name) != name}, logical(1))) |
      any(vapply(matched_obj_single, function(x) {name <- x$name; make.names(name) != name}, logical(1)))) {

    stop("Each code chunk must have a valid name in adherence to variable naming
         restrictions in R (e.g. does not begin with a number).")

  }

  # 1. Check if chunk starting tag appears after ending tag
  # 2. Check if chunks are overlapping (file is read line by line, so it should always be start -> end -> start -> end...
  # 3. Check fi single-line code chunks overlap with the multi-line code chunks (i.e. a single-line code chunk within a multi-line code chunk)
  if (any(vapply(matched_obj_multi, function(x) {x$start >= x$end}, logical(1)))
      | any(diff(c(starts_ends)) < 1)
      | any(vapply(match_single, function(x) any(starts_ends[1,] < x & starts_ends[2,] > x), logical(1))) ) {

    stop("Code chunks must be non-overlapping. Each code chunk should start
         with \"#@start_<chunkname>\" to mark the start, and end on a different
         line later on in the script with \"#@end_<chunkname>\"
         (using the same <chunkname>) to mark the end.")

  }

  if (any(duplicated(
    c(vapply(matched_obj_multi, function(x) x$name, character(1)),
      vapply(matched_obj_single, function(x) x$name, character(1)))))) {

    stop("Code chunk names must be unique.")

  }


  x <- Map(function(x) code[seq(x$start+1, x$end)], matched_obj_multi)
  y <- Map(function(x) code[x$start+1], matched_obj_single)

  # Removes any text that appears on and after the end tag on the last line of each code chunk,
  # while preserving any text that appears right before the end tag on the same line
  x <- lapply(x, function(k) {
    vapply(1:length(k), function(i) {
      sub(paste0("(^|[[:space:]]*)", re_end, "\\S(.*)"), "", k[i])}, character(1)) })

  x <- lapply(x, function(k) k[vapply(k, nchar, numeric(1)) > 0])

  names(x) <- vapply(matched_obj_multi, function(x) x$name, character(1))
  names(y) <- vapply(matched_obj_single, function(x) x$name, character(1))

  chunks <- append(x, y)
  chunks <- chunks[order(c(starts, match_single))] # Make sure the code chunks appear in same order as in the script

  # Remove indentation at the beginning of the code
  if (reindent) {
    chunks <- lapply(chunks, function(k) {

      nws <- nchar(k[1]) - nchar(sub("^[[:space:]]*", "", k[1])) # No. of white space at the start of 1st line
      vapply(1:length(k), function(i) { sub(paste0("^[[:space:]]{", nws,"}"), "", k[i])}, character(1)) })

  }

  chunks

}
