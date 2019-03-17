#' Data Extract input for teal modules
#'
#' The Data Extract input can be used to filter and select columns from a data
#' set to be used in teal. From this function's output a \code{data_extract_input} can
#' be constructed. This input can be read by a \link{data_extractor} module.
#'
#' @export
#'
#' @param dataname (\code{character}) Name of a teal data set
#' @param keys_filtering (\code{list}) Define how to filter the
#'  key columns of the data set. This is the outcome of \link{keys_filter_from_sep}
#' @param columns (\code{choices_selected}) Define which columns of the data set shall
#'  be selected next to the key variables. This shall be the outcome of \link{choices_selected}
#'
#'
data_extract <- function(dataname = NULL, keys_filtering = NULL, columns = NULL) {
  output <- list(dataname = dataname, keys_filtering = keys_filtering, columns = columns)
  class(output) <- c("list", "data_extract")
  return(output)
}

#' Extract a single data set from multiple outcomes of data_extractor
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

  # Create merged data set
  merged_data <- purrr::reduce(
    .x = lapply(datasets, function(dataset) dataset),
    .f = left_join,
    by = keys,
    suffix = paste0(".", datanames)
  )

  # Deal with duplicated entries inside the data sets
  if (any(duplicated(datanames))) {
    merged_data <- merged_data[, !duplicated(colnames(merged_data))]

    # Remove duplicated datanames by regular expression
    names(merged_data) <- stringr::str_replace_all(
      string = names(merged_data),
      pattern = "\\b([^\\.]+)\\.(\\1)\\b",
      replacement = c("\\1" = "\\1")
    )
  }
  attr(merged_data, "keys") <- keys
  return(merged_data)
}
