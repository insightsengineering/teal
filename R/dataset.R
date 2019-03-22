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
  datasets <- list(...)
  stopifnot(is_named_list(datasets))

  # set dataname attribute
  # todo: how to simplify the following
  dataset_names <- names(datasets)
  datasets <- lapply(seq_along(datasets), function(i) structure(datasets[[i]], dataname = dataset_names[[i]]))
  names(datasets) <- dataset_names

  # check for key intersection (at least one key), otherwise merge won't be possible
  all_keys <- lapply(list(...), function(dataset) attr(dataset, "keys"))
  stopifnot(
    all_true(
      all_keys, function(keys) length(intersect(keys, all_keys[[1]])) > 0
    )
  )
  datasets
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
  stopifnot(is.data.frame(df))
  stopifnot(is.atomic(keys) && all_true(keys, is.character))
  stopifnot(is.character(source))

  attr(df, "keys") <- keys
  attr(df, "source") <- source
  df
}
