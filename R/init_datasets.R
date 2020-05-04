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
  datasets$set_code(attr(data, "code"))

  return(invisible(NULL))
}

# todo1: rename
# set initial filter to default state, i.e. such that the encoding panel is there when the panel is there
# but no very restrictive filtering is applied (default filter may filter NA by default)
set_datasets_default_filter <- function(datasets, vars_per_dataset) {
  stopifnot(is(datasets, "FilteredData"))
  stopifnot(is.list(vars_per_dataset))

  Map(function(vars, dataset) {
    lapply(vars, function(varname) datasets$set_filter_state(
      dataset, varname, state = datasets$get_default_filter_state(dataset, varname)
    ))
  }, vars_per_dataset, names(vars_per_dataset))

  return(invisible(NULL))
}
