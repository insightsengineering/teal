#' @title Parse Code Chunks
#'
#' @description Save code chunks between specified start and end tags
#'   from a code script as a list of individual code chunks, each named
#'   according to the code chunk name included in the tags
#'
#' @param file path to the file containing the code chunks
#'
#' @return A list of individual code chunks, with each element consisting
#'   of a character vector of length the number of lines from `file`
#'   that corresponds to the specific code chunk
#'
#' @details Code chunks should be non-overlapping, start with a valid start tag
#'   beginning after '#' (e.g. "# @start_chunk1") and end with a valid end tag,
#'   with non-duplicated code chunk names that adhere to variable naming rules in R.
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
#' #   @start_aaa_part2
#' x <- data.frame(y = 1:3, z = 1:3)
#' x$y == x$z
#' #@end_aaa_part2
#'
#' x <- c(\"abc\", \"abcd\")     # @start_bbb some text
#' y <- pi * 2
#' round(y, 2)
#'    #@end_bbb more text
#'
#' #@start_ not part of a function
#' plot(1:10) #@end_
#'    ", file = file, append = FALSE)
#'
#' parse_code_chunks(file)
#'
parse_code_chunks <- function(file) {

  if (!file.exists(file)) stop("File does not exist.")

  con <- file(file)
  code <- readLines(con, warn = FALSE)
  close(con)


  re <- "#[[:space:]]*"
  re_start <- paste0(re, "@start_")
  re_end <- paste0(re, "@end_")

  is_match <- grepl(paste0("(^|[[:space:]]*)", re_start, "\\S"), code, ignore.case = TRUE)


  matched_obj <- if (any(is_match)) {

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
    }, which(is_match))

  } else {

    NULL

  }


  # Checking validity of input
  if (any(vapply(matched_obj, function(x) {name <- x$name; make.names(name) != name}, logical(1)))) {

    stop("Each code chunk must have a valid name in adherence to variable naming
         restrictions in R (e.g. does not begin with a number).")

  }

  starts <- vapply(matched_obj, function(x) x$start, numeric(1))

  if (any(vapply(matched_obj, function(x) {x$start >= x$end}, logical(1)))
      | any(diff(starts) < 1)) {

    stop("Code chunks must be non-overlapping. Each code chunk should start
         with \"#@start_<chunkname>\" to mark the start, and end on a different
         line later on in the script with \"#@end_<chunkname>\"
         (using the same <chunkname>) to mark the end.")

  }

  if (any(duplicated(vapply(matched_obj, function(x)x$name, character(1))))) {

    stop("Code chunk names must be unique.")

  }


  x <- Map(function(x) code[seq(x$start+1, x$end)], matched_obj)

  # Removes any text that appears on and after the end tag on the last line of each code chunk,
  # while preserving any text that appears right before the end tag on the same line
  x <- lapply(x, function(k) {
    vapply(1:length(k), function(i) {
      sub(paste0("(^|[[:space:]]*)", re_end, "\\S(.*)"), "", k[i])}, character(1)) })

  x <- lapply(x, function(k) k[vapply(k, nchar, numeric(1)) > 0])

  names(x) <- vapply(matched_obj, function(x) x$name, character(1))

  x

}
