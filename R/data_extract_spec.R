#' Data Extract input for teal modules
#'
#' The Data Extract input can be used to filter and select columns from a data
#' set to be used in teal. From this function's output a \code{data_extract_input} can
#' be constructed. This input can be read by a \link{data_extract_module} module.
#'
#' @export
#' @name DataExtractSpec
#' @aliases data_extract_class data_extract_spec
#' 
#' @param dataname (\code{character}) Name of a teal data set
#' @param keys_filtering (\code{list}) Define how to filter the
#'  key columns of the data set. This is the outcome of \link{keys_filtering_spec}
#' @param columns (\code{choices_selected}) Define which columns of the data set shall
#'  be selected next to the key variables. This shall be the outcome of \link{choices_selected}
#'
#' @field dataname (\code{character}) Data set to be extracted and selected
#' @field keys_filtering (\code{KeysFilteringSpec}) Setup of the dataset filtering
#' @field columns (\code{choices_selected}) Columns to be selected from the input dataset
data_extract_class <- R6::R6Class("DataExtractSpec",
  public = list(
    dataname = character(0),
    keys_filtering = NULL,
    columns = NULL,
    initialize = function(dataname = NULL, keys_filtering = NULL, columns = NULL) {
      self$dataname <- dataname
      self$set_keys_filtering(keys_filtering)
      self$set_columns(columns)
    },
    set_keys_filtering = function(keys_filtering) {
      stopifnot(methods::is(keys_filtering, "KeysFilteringSpec") || is.null(keys_filtering))
      self$keys_filtering <- keys_filtering
    },
    set_columns = function(columns) {
      stopifnot(methods::is(columns, "choices_selected"))
      self$columns <- columns
    }
  )
)

#' Constructor for \link{DataExtractSpec}
#'
#' @inheritParams DataExtractSpec
#' @rdname DataExtractSpec
#' @export
data_extract_spec <- function(dataname = NULL, keys_filtering = NULL, columns = NULL) {
  data_extract_class$new(dataname = dataname, keys_filtering = keys_filtering, columns = columns)
}
