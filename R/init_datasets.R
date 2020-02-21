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


create_empty_datasets <- function() {
  return(FilteredData$new())
}


set_datasets_data <- function(datasets, data) {
  stopifnot(is(datasets, "FilteredData"))

  if (!is(data, "cdisc_data")) {
    warning("Please use cdisc_data() instead of list() for 'data' argument. It will be depreciated soon.")
    data <- convert_to_cdisc_data(data)
  }

  for (idx in seq_along(data)) {
    datasets$set_data(data[[idx]][["dataname"]], data[[idx]][["data"]])
    datasets$set_data_attr(data[[idx]][["dataname"]], "keys", data[[idx]][["keys"]])
    datasets$set_data_attr(data[[idx]][["dataname"]], "labels", data[[idx]][["labels"]])
  }

  return(invisible(NULL))
}


set_datasets_filter <- function(datasets, filter) {
  stopifnot(is(datasets, "FilteredData"))

  if (!is.null(filter) && !is.null(filter$init)) {
    Map(function(vars, dataset) {
      lapply(vars, function(var) datasets$set_default_filter_state(dataset, var))
    }, filter$init, names(filter$init))
  }

  return(invisible(NULL))
}
