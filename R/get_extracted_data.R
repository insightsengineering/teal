#' Extract a single data set from multiple outcomes of data_extract_module
#'
#' @param ... \code{data.frame} Each input to this function shall be a data.frame that
#'   has the attributes: \code{keys}, \code{dataname}
#'
#' @description This function builds a joined data set of all the data sets coming
#'  being pasted into it. All columns that are non key columns get renamed into
#'  \code{<COLUMNNAME>.<DATANAME>} due to the data set they were coming from.
#'
#' @export
#' @importFrom stringr str_replace_all
#' @importFrom purrr reduce
#' @importFrom magrittr %>%
#' @importFrom dplyr left_join
get_extracted_data <- function(...) {
  datasets <- list(...)
  lapply(datasets, function(x) {
    if (is.null(dim(x))) {
      stop(paste0("The dataset ", attr(x, "dataname"), "does not contain any data in your setup."))
    }
  })
  all_keys <- lapply(datasets, function(dataset) attr(dataset, "keys"))

  if (!all_true(all_keys, function(keys) identical(keys, all_keys[[1]]))) {
    stop("The datasets chosen cannot be merged as the keys are not equal.")
  }
  keys <- all_keys[[1]]

  datasets <- lapply(datasets, prefix_column_names_with_dataset)

  # Take merge from Max merge_datasets
  # Create merged data set
  merged_data <- purrr::reduce(
    .x = lapply(datasets, function(dataset) dataset),
    .f = left_join_without_duplicated,
    by = keys
  )
  attr(merged_data, "keys") <- keys
  merged_data
}

#' @importFrom dplyr select
left_join_without_duplicated <- function(x, y, by, ...) {
  # removes duplicated columns before left joining so they don't appear with suffix
  {
    common_cols <- union(names(x), names(y))
    stopifnot(identical(
      x %>% select(common_cols), 
      y %>% select(common_cols))
    )  
  } 
  new_y_cols <- union(by, setdiff(names(y), names(x)))
  y <- y %>% select(new_y_cols)
  left_join(x, y, by = by, ...)
}
