#' Data Extract input for teal modules
#'
#' The Data Extract input can be used to filter and select columns from a data
#' set to be used in teal. Please use the constructor function \link{data_extract_spec}
#' to set it up.
#'
#' From this function's output a \code{\link{data_extract_input}} can
#' be constructed. This input can be read by a \link{data_extract_module} module.
#'
#' @export
#' @name DataExtractSpec
#' @keywords data
#' @docType class
#' @section Initialize:
#' \describe{
#'   \item{via new}{ \code{DataExtractSpec$new(dataname, keys_filtering = NULL, columns = NULL)} }
#'   \item{via constructor}{
#'    \code{\link{data_extract_spec}(dataname, keys_filtering = NULL, columns = NULL)}
#'   }
#' }
#'
#' @field dataname (\code{character}) Data set to be extracted and selected
#' @field keys_filtering (\code{\link{KeysFilteringSpec}}) Setup of the dataset filtering
#' @field columns (\code{\link{choices_selected}}) Columns to be selected from the input dataset
DataExtractSpec <- R6::R6Class("DataExtractSpec", # nolint
  public = list(
    dataname = character(0),
    filter = NULL,
    columns = NULL,
    initialize = function(dataname, columns, filter = NULL) {
      stopifnot(!is.null(dataname))
      stopifnot(!is.null(columns))
      self$dataname <- dataname
      self$set_filter(filter)
      self$set_columns(columns)
    },
    set_filter = function(filter) {
      stopifnot(methods::is(filter, "filter_choices_spec") || is.null(filter))
      self$filter <- filter
    },
    set_columns = function(columns) {
      stopifnot(methods::is(columns, "column_choices_spec"))
      self$columns <- columns
    }
  )
)

#' Constructor for \link{DataExtractSpec}
#'
#' @param dataname (\code{character}) Name of a teal data set
#' @param keys_filtering (\code{KeysFilteringSpec}) Define how to filter the
#'  key columns of the data set. This is the outcome of \link{keys_filtering_spec}
#' @param columns (\code{choices_selected}) Define which columns of the data set shall
#'  be selected next to the key variables. This shall be the outcome of \link{choices_selected}
#'
#' @return \link{DataExtractSpec} class object
#' @export
data_extract_spec <- function(dataname, columns, filter = NULL) {
  DataExtractSpec$new(dataname = dataname, columns = columns, filter = filter)
}
