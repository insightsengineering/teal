convert_to_cdisc_data <- function(data) {
  if (is_class_list("data.frame")(data)) {

    data_names <- names(data)
    stopifnot(length(data_names) == length(data))

    data_substitute <- substitute(data)
    data <- eval(as.call(append(
      quote(cdisc_data), lapply(
        seq_along(data),
        function(idx) {
          call(
            "cdisc_dataset",
            dataname = data_names[[idx]],
            data = data_substitute[[idx + 1]]
          )
        }
      )
    )))
  } else {
    return(do.call(RelationalData$new, data)$get_cdisc_data())
  }

  return(data)
}

set_datasets_data <- function(datasets, data) {
  stopifnot(is(datasets, "FilteredData"))

  if (!is(data, "cdisc_data")) {
    warning("Please use teal_data() as a wrapper for 'data' argument. 'list' will be depreciated soon.")
    data <- convert_to_cdisc_data(data)
  }

  for (idx in seq_along(data)) {
    data_i <- data[[idx]]$get_raw_data()
    dataname <- data[[idx]]$get_dataname()
    keys <- data[[idx]]$get_keys()
    keys <- if_null(keys, get_cdisc_keys(dataname))

    attr(data_i, "keys") <- keys
    attr(data_i, "column_labels") <- data[[idx]]$get_column_labels()
    attr(data_i, "data_label") <- data[[idx]]$get_dataset_label()
    datasets$set_data(dataname, data_i)
  }

  # set code to generate the unfiltered datasets
  datasets$set_preproc_code(get_code(data))

  return(invisible(NULL))
}

set_datasets_filters <- function(datasets, filter_states) {
  stopifnot(
    is(datasets, "FilteredData"),
    is_fully_named_list(filter_states),
    all(names(filter_states) %in% datasets$datanames())
  )

  Map(function(dataname, filters_for_dataname) {
    datasets$set_filter_state(dataname, varname = NULL, state = filters_for_dataname)
  }, names(filter_states), filter_states)
  return(invisible(NULL))
}
