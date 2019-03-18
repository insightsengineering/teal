#' Data Extract input for teal modules
#'
#' The Data Extract input can be used to filter and select columns from a data
#' set to be used in teal. From this function's output a \code{data_extract_input} can
#' be constructed. This input can be read by a \link{data_extract_module} module.
#'
#' @export
#' @name DataExtractSpec
#' @aliases data_extract_class
#' @param dataname (\code{character}) Name of a teal data set
#' @param keys_filtering (\code{list}) Define how to filter the
#'  key columns of the data set. This is the outcome of \link{keys_filtering_spec}
#' @param columns (\code{choices_selected}) Define which columns of the data set shall
#'  be selected next to the key variables. This shall be the outcome of \link{choices_selected}
#' 
#' @field dataname (\code{character}) Data set to be extracted and selected
#' @field keys_filtering (\code{KeysFilteringSpec}) Setup of the dataset filtering
#' @field columns (\code{choices_selected}) Columns to be selected from the input dataset
#'
data_extract_class <- R6::R6Class("DataExtractSpec",
    public = list(
        dataname = character(0),
        keys_filtering = NULL,
        columns = NULL,
        initialize = function(dataname = NULL, keys_filtering = NULL, columns = NULL) {
          self$dataname = dataname
          self$set_keys_filtering(keys_filtering)
          self$set_columns(columns)
        },
        set_keys_filtering = function(keys_filtering){
          stopifnot(methods::is(keys_filtering, "KeysFilteringSpec") || is.null(keys_filtering))
          self$keys_filtering <- keys_filtering
        },
        set_columns = function(columns){
          stopifnot(methods::is(columns, "choices_selected"))
          self$columns <- columns
        }
        
))

#' Constructor for \link{DataExtractSpec}
#' 
#' @export 
#' @param ... params of \link{DataExtractSpec}
data_extract_spec <- function(...){
  data_extract_class$new(...)
}

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
extracted_data <- function(...) {
  datasets <- list(...)
  lapply(datasets, function(x) {
    if (is.null(dim(x))) {
      stop(paste0("The dataset ", attr(x, "dataname"), "does not contain any data in your setup."))
    }
  })
  all_keys <- lapply(datasets, function(dataset) attr(dataset, "keys"))

  if (!all(vapply(all_keys, function(keys) identical(keys, all_keys[[1]]), TRUE))) {
    stop("The datasets chosen cannot be merged as the keys are not equal.")
  }
  keys <- all_keys[[1]]

  datanames <- lapply(datasets, function(dataset) attr(dataset, "dataname")) %>% unlist()

  datasets <- lapply(datasets, set_selected_column_names)
 
  # Take merge from Max merge_datasets
  # Create merged data set
  merged_data <- purrr::reduce(
    .x = lapply(datasets, function(dataset) dataset),
    .f = left_join_without_duplicated,
    by = keys
  )
  attr(merged_data, "keys") <- keys
  return(merged_data)
}

#' @importFrom dplyr select
left_join_without_duplicated <- function(x, y, by, ...){
  new_y_cols <- union(by, setdiff(names(y), names(x)))
  y <- y %>% select(new_y_cols)
  left_join(x, y, by = by, ...)
}
