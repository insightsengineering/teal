full_dataset_column_identifier <- function(data, columns) {
  dataname <- attr(data, "dataname")
  stopifnot(is.atomic(dataname) && (length(dataname) == 1))
  paste(dataname, columns, sep = ".")
}

get_non_key_columns <- function(data) {
  keys <- attr(data, "keys")
  setdiff(names(data), keys)
}

#' Derive non-key column names from data
#' 
#' Called by the user writing functions like tm_plot_xy
#' 
#' @export
#'
#' @param data (\code{data.frame}) Data with attribute \code{keys} and \code{dataname}
#' @param remove_rowid whether to remove rowid that was added during filtering (should only be set to FALSE internally)
#'
#' @return A named character vector with <variable> ---> <dataname>.<variable>
#'
get_dataset_prefixed_col_names <- function(data, remove_rowid=TRUE) {
  keys <- attr(data, "keys")
  non_key_columns <- setdiff(names(data), keys)
  stopifnot("rowid" %in% non_key_columns)
  if (remove_rowid) {
    non_key_columns <- setdiff(non_key_columns, "rowid")
  }

  new_column_names <- full_dataset_column_identifier(data, non_key_columns)
  names(new_column_names) <- non_key_columns
  return(new_column_names)
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
#' @importFrom magrittr %>%
get_nonkey_col_values <- function(data) {
  keys <- attr(data, "keys")
  non_key_columns <- setdiff(names(data), keys)
  data %>% dplyr::select(non_key_columns)
}


