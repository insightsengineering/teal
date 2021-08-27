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

#' @export
filtered_data_new.RelationalData <- function(x) { # nolintr # nousage
  FilteredData$new()
}

#' @export
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

#' @export
filtered_data_set.RelationalData <- function(data, datasets) { # nolintr # nousage
  datasets$set_code(data$get_code_class())
  for (dataset in data$get_datasets()) {
    datasets$set_dataset(dataset)
  }

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

  if (length(filter) > 0) {
    datasets$set_bookmark_state(filter)
  }


  return(invisible(NULL))
}
