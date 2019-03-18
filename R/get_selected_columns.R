#' Derive non-key columns from data
#'
#' @export
#'
#' @param data (\code{data.frame}) Data with attribute \code{keys} and \code{dataname}
#'
#' @return A character vector with all non-key variable names as
#'   \code{<VARIABLE>.<DATANAME>}
#'
get_selected_columns <- function(data) {
  dataname <- attr(data, "dataname")

  keys <- attr(data, "keys")

  leftover <- setdiff(names(data), keys)

  new_names <- paste(dataname, leftover, sep = ".")
  names(new_names) <- leftover

  return(new_names)
}

set_selected_column_names <- function(data) {
  overwrite_names <- get_selected_columns(data)

  names(data)[which(names(data) %in% names(overwrite_names))] <- overwrite_names

  return(data)
}
