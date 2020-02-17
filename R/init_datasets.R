#' Initialize datasets for a teal app
#'
#' @inheritParams init
#' @return \link{FilteredData} object with the data
#'   to be used in the teal app
#'
init_datasets <- function(data, filter) {
  if (!is(data, "cdisc_data")) {
    warning("Please use cdisc_data() instead of list() for 'data' argument. It will be depreciated soon.")

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
  }

  # initialize FilteredData object
  datasets <- FilteredData$new(vapply(data, `[[`, character(1), "dataname", USE.NAMES = FALSE))
  for (idx in seq_along(data)) {
    datasets$set_data(data[[idx]][["dataname"]], data[[idx]][["data"]])
    datasets$set_data_attr(data[[idx]][["dataname"]], "keys", data[[idx]][["keys"]])
    datasets$set_data_attr(data[[idx]][["dataname"]], "labels", data[[idx]][["labels"]])
  }

  # including attributes of data object
  datasets$set_attrs(data)

  # set default init filters
  if (!is.null(filter) && !is.null(filter$init)) {
    Map(function(vars, dataset) {
          lapply(vars, function(var) datasets$set_default_filter_state(dataset, var))
        }, filter$init, names(filter$init))
  }


  return(datasets)
}
