set_datasets_data <- function(datasets, data) {
  stopifnot(is(datasets, "FilteredData"))
  stopifnot(is(data, "RelationalDataList"))

  datasets$set_code(data$get_code_class())

  for (dataname in data$get_datanames()) {
    raw_dataset <- get_raw_data(data, dataname)
    datasets$set_data(dataname, raw_dataset)

    dataset <- get_dataset(data, dataname)
    datasets$set_data_attr(dataname, "keys", dataset$get_keys())
    datasets$set_data_attr(dataname, "column_labels", dataset$get_column_labels())
    datasets$set_data_attr(dataname, "data_label", dataset$get_dataset_label())
    datasets$set_data_attr(dataname, "check", data$get_check_result())
  }

  return(invisible(NULL))
}

set_datasets_filters <- function(datasets, filter) {
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
