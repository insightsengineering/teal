#' Derive non-key column names from data
#'
#' @export
#'
#' @param data (\code{data.frame}) Data with attribute \code{keys} and \code{dataname}
#'
#' @return A character vector with all non-key variable names as
#'   \code{<VARIABLE>.<DATANAME>}
#'
get_selected_column_names <- function(data) {
  dataname <- attr(data, "dataname")

  keys <- attr(data, "keys")

  leftover <- setdiff(names(data), keys)

  new_names <- paste(dataname, leftover, sep = ".")
  names(new_names) <- leftover

  return(new_names)
}

#' Derive non-key column values from data
#'
#' @export
#'
#' @param data (\code{data.frame}) Data with attribute \code{keys} and \code{dataname}
#'
#' @return A character data.frame with just the columns selected that are not
#'  keys
#'
get_selected_column_values <- function(data) {
  keys <- attr(data, "keys")

  leftover <- setdiff(names(data), keys)

  data[, leftover]
}

set_selected_column_names <- function(data) {
  overwrite_names <- get_selected_column_names(data)

  names(data)[which(names(data) %in% names(overwrite_names))] <- overwrite_names

  return(data)
}
