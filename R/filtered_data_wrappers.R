#' Create a new `FilteredData` object
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param x an object that inherits from `RelationalData`
#'
#' @return a (`CDISCDataset`, `CDISCDatasetConnector`) object
#'
#' @export
filtered_data_new <- function(x) {
  UseMethod("filtered_data_new")
}

#' @rdname filtered_data_new
#' @export
filtered_data_new.RelationalData <- function(x) { # nolint
  FilteredData$new()
}

#' @rdname filtered_data_new
#' @export
filtered_data_new.CDISCData <- function(x) {
  CDISCFilteredData$new()
}

#' Set `FilteredData` with data from `RelationalData`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param data an object that inherits from `RelationalData`
#' @param datasets an object that inherits from `FilteredData`
#'
#' @return modified `FilteredData` object
#'
#' @export
filtered_data_set <- function(data, datasets) {
  UseMethod("filtered_data_set")
}

#' @rdname filtered_data_set
#' @export
filtered_data_set.RelationalData <- function(data, datasets) { # nolint

  datasets$set_code(data$get_code_class())

  for (dataset in data$get_datasets()) {
    dataname <- get_dataname(dataset)
    raw_dataset <- get_raw_data(dataset)
    datasets$set_data(dataname, raw_dataset)
    datasets$set_data_attrs(dataname, get_attrs(dataset))
    datasets$set_data_attr(dataname, "check", data$get_check_result())
  }

  datasets$set_join_keys(data$get_join_keys())

  return(invisible(NULL))
}


#' Set up `FilteredData` filters
#'
#' @param datasets an object that inherits from `FilteredData`
#' @param filter (named `list`) of filter entries
#'
#' @importFrom methods is
#' @export
filtered_data_set_filters <- function(datasets, filter) {
  stopifnot(
    is(datasets, "FilteredData"),
    is_fully_named_list(filter),
    all(names(filter) %in% datasets$datanames())
  )

  Map(function(dataname, filters_for_dataname) {
    datasets$set_filter_state(dataname, state = filters_for_dataname)
  }, names(filter), filter)

  return(invisible(NULL))
}
