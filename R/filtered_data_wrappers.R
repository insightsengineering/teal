#' Create a new `FilteredData` object
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param x an object that inherits from `RelationalData`
#'
#' @return a (`CDISCDataset`, `CDISCDatasetConnector`) object
#'
#' @noRd
filtered_data_new <- function(x) {
  UseMethod("filtered_data_new")
}

filtered_data_new.RelationalData <- function(x) { # nolintr # nousage
  FilteredData$new()
}

filtered_data_new.CDISCData <- function(x) { # nolintr # nousage
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
#' @noRd
filtered_data_set <- function(data, datasets) { # nolintr # nousage
  UseMethod("filtered_data_set", data)
}

filtered_data_set.RelationalData <- function(data, datasets) { # nolintr # nousage

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
#' @noRd
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
