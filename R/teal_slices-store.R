#' Store and restore `teal_slices` object
#'
#' Functions that serialize a `teal_slices` object to and from a file in
#' `JSON` format.
#' The `teal_slices` object contains information about filter states and can be used to
#' create, modify, and delete filter states.
#'
#' Date and date time objects are stored in the following formats:
#'
#' - `Date` class is converted to the `"ISO8601"` standard (`YYYY-MM-DD`).
#' - `POSIX*t` classes are converted to character by using
#' `format.POSIX*t(usetz = TRUE, tz = "UTC")` (`YYYY-MM-DD {N}{N}:{N}{N}:{N}{N} UTC`,
#' where `{N} = [0-9]` is a number and `UTC` is `Coordinated Universal Time`
#' timezone short-code).
#'
#' This format is assumed during `slices_restore`. All `POSIX*t` objects in
#' `selected` or `choices` fields of `teal_slice` objects are always printed in
#' `UTC` timezone as well.
#'
#' @param tss (`teal_slices`) object to be stored.
#' @param file (`character(1)`) The file path where `teal_slices` object will be
#' saved and restored. The file extension should be `".json"`.
#'
#' @return `slices_store` returns `NULL`, invisibly.
#' @examplesIf requireNamespace("withr")
#' # use non-exported function from teal
#' slices_store <- getFromNamespace("slices_store", "teal")
#'
#' # Create a teal_slices object
#' tss <- teal_slices(
#'   teal.slice::teal_slice(dataname = "data", varname = "var"),
#'   teal_slice(dataname = "data", expr = "x > 0", id = "positive_x", title = "Positive x")
#' )
#'
#' # Store the teal_slices object to a file
#' slices_store(tss, withr::local_file("file.json"))
#'
#' # use non-exported function from teal
#' slices_restore <- getFromNamespace("slices_restore", "teal")
#'
#' # Restore a teal_slices object from a file
#' tss_restored <- slices_restore(withr::local_file("file.json"))
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
#'
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
