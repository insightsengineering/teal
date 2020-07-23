set_datasets_data <- function(datasets, data) {
  stopifnot(is(datasets, "FilteredData"))
  stopifnot(is(data, "RelationalDataList"))

  for (dataname in data$get_datanames()) {
    datasets$set_data(dataname, get_raw_data(data, dataname))
    datasets$set_code(dataname, get_code(data, dataname, deparse = TRUE))

    dataset <- get_dataset(data, dataname)
    datasets$set_data_attr(dataname, "keys", dataset$get_keys())
    datasets$set_data_attr(dataname, "column_labels", dataset$get_column_labels())
    datasets$set_data_attr(dataname, "data_label", dataset$get_dataset_label())
  }

  code_all <- data$.__enclos_env__$private$get_mutate_code(deparse = TRUE)
  if_not_empty(code_all, datasets$set_code_all(code_all))

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
