#' Store teal_slices object to a file
#'
#' This function takes a `teal_slices` object and saves it to a file in `JSON` format.
#' The `teal_slices` object contains information about filter states and can be used to
#' create, modify, and delete filter states. The saved file can be later loaded using
#' the `slices_restore` function.
#'
#' @param tss (`teal_slices`) object to be stored.
#' @param file (`character(1)`) The file path where `teal_slices` object will be saved.
#'  The file extension should be `".json"`.
#'
#' @details `Date` class is stored in `"ISO8601"` format (`YYYY-MM-DD`). `POSIX*t` classes are converted to a
#' character by using `format.POSIX*t(usetz = TRUE, tz = "UTC")` (`YYYY-MM-DD {N}{N}:{N}{N}:{N}{N} UTC`, where
#' `{N} = [0-9]` is a number and `UTC` is `Coordinated Universal Time` timezone short-code).
#' This format is assumed during `slices_restore`. All `POSIX*t` objects in `selected` or `choices` fields of
#' `teal_slice` objects are always printed in `UTC` timezone as well.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#'
#' @examples
#' # Create a teal_slices object
#' tss <- teal_slices(
#'   teal_slice(dataname = "data", varname = "var"),
#'   teal_slice(dataname = "data", expr = "x > 0", id = "positive_x", title = "Positive x")
#' )
#'
#' if (interactive()) {
#'   # Store the teal_slices object to a file
#'   slices_store(tss, "path/to/file.json")
#' }
#'
slices_store <- function(tss, file) {
  checkmate::assert_class(tss, "teal_slices")
  checkmate::assert_path_for_output(file, overwrite = TRUE, extension = "json")

  cat(format(tss, trim_lines = FALSE), "\n", file = file)
}

#' Restore teal_slices object from a file
#'
#' This function takes a file path to a `JSON` file containing a `teal_slices` object
#' and restores it to its original form. The restored `teal_slices` object can be used
#' to access filter states and their corresponding attributes.
#'
#' @param file Path to file where `teal_slices` is stored. Must have a `.json` extension and read access.
#'
#' @return A `teal_slices` object restored from the file.
#'
#' @keywords internal
#'
#' @examples
#' if (interactive()) {
#'   # Restore a teal_slices object from a file
#'   tss_restored <- slices_restore("path/to/file.json")
#' }
#'
slices_restore <- function(file) {
  checkmate::assert_file_exists(file, access = "r", extension = "json")

  tss_json <- jsonlite::fromJSON(file, simplifyDataFrame = FALSE)
  tss_json$slices <-
    lapply(tss_json$slices, function(slice) {
      for (field in c("selected", "choices")) {
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
          slice[[field]] <-
            if (field == "selected") {
              character(0)
            } else {
              NULL
            }
        }
      }
      slice
    })

  tss_elements <- lapply(tss_json$slices, as.teal_slice)

  do.call(teal_slices, c(tss_elements, tss_json$attributes))
}
