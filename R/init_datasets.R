convert_to_cdisc_data <- function(data) {
  stopifnot(is_class_list("data.frame")(data))
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

  return(data)
}

set_datasets_data <- function(datasets, data) {
  stopifnot(is(datasets, "FilteredData"))

  if (!is(data, "cdisc_data")) {
    warning("Please use cdisc_data() instead of list() for 'data' argument. It will be depreciated soon.")
    data <- convert_to_cdisc_data(data)
  }

  for (idx in seq_along(data)) {
    # for initializing, we don't need a reactive context as dependencies will be established later only
    data_i <- data[[idx]][["data"]]
    attr(data_i, "keys") <- data[[idx]][["keys"]]
    attr(data_i, "column_labels") <- data[[idx]][["column_labels"]]
    attr(data_i, "data_label") <- data[[idx]][["data_label"]]
    datasets$set_data(data[[idx]][["dataname"]], data_i)
  }

  # set code to generate the unfiltered datasets
  datasets$set_preproc_code(attr(data, "code"))

  return(invisible(NULL))
}

set_datasets_filters <- function(datasets, filters) {
  stopifnot(is(datasets, "FilteredData"))
  stopifnot(is.list(filters))

  Map(function(dataname, filters_for_dataname) {
    # replace "default" string by actual default filter state for that variable
    filters_for_dataname <- Map(function(varname, var_filter_state) {
      if (identical(var_filter_state, "default")) {
        datasets$get_default_filter_state(dataname, varname)
      } else {
        var_filter_state
      }
    }, names(filters_for_dataname), filters_for_dataname)
    datasets$set_filter_state(dataname, varname = NULL, state = filters_for_dataname)
  }, names(filters), filters)
}
