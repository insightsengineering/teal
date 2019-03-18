get_non_key_columns <- function(data) {
  dataname <- attr(data, "dataname")
  keys <- attr(data, "keys")
  setdiff(names(data), keys)
}

#' Derive non-key column names from data
#'
#' @export
#'
#' @param data (\code{data.frame}) Data with attribute \code{keys} and \code{dataname}
#'
#' @return A named character vector with <variable> ---> <dataname>.<variable>
#'
get_dataset_prefixed_column_names <- function(data) {
  dataname <- attr(data, "dataname")
  keys <- attr(data, "keys")
  non_key_columns <- setdiff(names(data), keys)

  new_column_names <- paste(dataname, non_key_columns, sep = ".")
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
get_nonkey_column_values <- function(data) {
  keys <- attr(data, "keys")
  non_key_columns <- setdiff(names(data), keys)
  data %>% dplyr::select(non_key_columns)
}

# prefixes non-key columns with dataset name
prefix_column_names_with_dataset <- function(data) {
  overwrite_names <- get_dataset_prefixed_column_names(data)
  names(data)[which(names(data) %in% names(overwrite_names))] <- overwrite_names
  return(data)
}
