#' Store and restore `teal_slices` object
#'
#' Functions that write a `teal_slices` object to a file in the `JSON` format,
#' and also restore the object from disk.
#'
#' Date and date time objects are stored in the following formats:
#'
#' - `Date` class is converted to the `"ISO8601"` standard (`YYYY-MM-DD`).
#' - `POSIX*t` classes are converted to character by using
#' `format.POSIX*t(usetz = TRUE, tz = "UTC")` (`YYYY-MM-DD HH:MM:SS UTC`, where
#' `UTC` is the `Coordinated Universal Time` timezone short-code).
#'
#' This format is assumed during `slices_restore`. All `POSIX*t` objects in
#' `selected` or `choices` fields of `teal_slice` objects are always printed in
#' `UTC` timezone as well.
#'
#' @param tss (`teal_slices`) object to be stored.
#' @param file (`character(1)`) file path where `teal_slices` object will be
#' saved and restored. The file extension should be `".json"`.
#'
#' @return `slices_store` returns `NULL`, invisibly.
#'
#' @seealso [teal_slices()]
#'
#' @keywords internal
#'
slices_store <- function(tss, file) {
  checkmate::assert_class(tss, "teal_slices")
  checkmate::assert_path_for_output(file, overwrite = TRUE, extension = "json")

  cat(format(tss, trim_lines = FALSE), "\n", file = file)
}

#' @rdname slices_store
#' @return `slices_restore` returns a `teal_slices` object restored from the file.
#' @keywords internal
slices_restore <- function(file) {
  checkmate::assert_file_exists(file, access = "r", extension = "json")

  tss_json <- jsonlite::fromJSON(file, simplifyDataFrame = FALSE)
  tss_json$slices <-
    lapply(tss_json$slices, function(slice) {
      for (field in c("selected", "choices")) {
        if (!is.null(slice[[field]])) {
          if (length(slice[[field]]) > 0) {
            date_partial_regex <- "^[0-9]{4}-[0-9]{2}-[0-9]{2}"
            time_stamp_regex <- paste0(date_partial_regex, "\\s[0-9]{2}:[0-9]{2}:[0-9]{2}\\sUTC$")

            slice[[field]] <-
              if (all(grepl(paste0(date_partial_regex, "$"), slice[[field]]))) {
                as.Date(slice[[field]])
              } else if (all(grepl(time_stamp_regex, slice[[field]]))) {
                as.POSIXct(slice[[field]], tz = "UTC")
              } else {
                slice[[field]]
              }
          } else {
            slice[[field]] <- character(0)
          }
        }
      }
      slice
    })

  tss_elements <- lapply(tss_json$slices, as.teal_slice)

  do.call(teal_slices, c(tss_elements, tss_json$attributes))
}
