#' CDISC data check
#'
#' @param ... datasets constructed with \link{data_for_teal}
#'
#' @return a list of the input data sets
#'
#' @details This function checks if there were keys added to all data
#' 	sets that shall be analyzed inside a teal app.
#'
#' @export
#'
cdisc_data <- function(...) {
  all_keys <- lapply(list(...), function(dataset) attr(dataset, "keys"))

  stopifnot(
    all(
      vapply(
        all_keys, function(keys) length(intersect(keys, all_keys[[1]])) > 0, TRUE
      )
    )
  )
  list(...)
}

#' Constructor for teal data set
#'
#' @param df (\code{data.frame}) data to be handled inside
#' @param keys (\code{character}) vector telling which columns of the \code{ds} are
#'   key variables.
#' @param source (\code{character}) string defining how the data was constructed. Important
#'   for reproducibility.
#'
#' @return (\code{data.frame}) with additional attributes keys and source.
#'
#' @export
#'
data_for_teal <- function(df, keys, source = NULL) {
  stopifnot(methods::is(df, "data.frame"))
  stopifnot(is.atomic(keys) && all_true(keys, is.character))
  stopifnot(is.character(source))

  attr(df, "keys") <- keys
  attr(df, "source") <- source
  df
}
