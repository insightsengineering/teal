#' @importFrom methods is
set_datasets_data <- function(datasets, data) {
  stopifnot(is(datasets, "FilteredData"))
  stopifnot(is(data, "RelationalData"))

  # execute reproducibility check on just loaded data
  # check will be executed according to data class configuration (it's possible to disable this step)
  data$check()
  if (isFALSE(data$get_check_result())) {
    stop("Reproducibility error. Couldn't reproduce object(s) with a given code")
  }

  datasets$set_code(data$get_code_class())

  for (dataset in data$get_datasets()) {
    dataname <- get_dataname(dataset)
    raw_dataset <- get_raw_data(dataset)
    datasets$set_data(dataname, raw_dataset)
    datasets$set_data_attr(dataname, "keys", dataset$get_keys())
    datasets$set_data_attr(dataname, "column_labels", dataset$get_column_labels())
    datasets$set_data_attr(dataname, "data_label", dataset$get_dataset_label())
    datasets$set_data_attr(dataname, "check", data$get_check_result())
  }

  return(invisible(NULL))
}

#' @importFrom methods is
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
