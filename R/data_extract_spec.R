#' Constructor for \code{data_extract_spec}
#'
#' @param dataname (\code{character}) Name of a teal data set
#' @param columns (\code{choices_selected}) Define which columns of the data set shall
#'  be selected next to the key variables. This shall be the outcome of \link{choices_selected}
#' @param filter (\code{filter_spec}-S3-class) Define how to filter the
#'  key columns of the data set. This is the outcome of \link{filter_spec}
#'
#' @return \code{data_extract_spec}-S3-class object
#' @export
data_extract_spec <- function(dataname, columns, filter = NULL) {
  stopifnot(is.character(dataname), length(dataname) == 1)
  stopifnot(is(columns, "column_spec"), length(columns) >= 1)
  stopifnot(is.null(filter) || (is(filter, "filter_spec") & length(filter) >= 1))

  res <- list(dataname = dataname, columns = columns, filter = filter)
  class(res) <- "data_extract_spec"

  res
}
