#' S3 generic for `to_relational_data` function.
#'
#' Takes the input of data argument and translates them to relational data objects.
#'
#' @param data `TealDataset`, `TealDatasetConnector`, `data.frame`, `list` or `function` returning a named list.
#'
#' @return list of `TealData` objects
#'
#' @note `to_relational_data` should only be used inside `init` call to guarantee correct behavior.
#'
to_relational_data <- function(data) {
  UseMethod("to_relational_data")
}

#' @export
to_relational_data.data.frame <- function(data) { # nolint
  dataname <- deparse(substitute(data, parent.frame()), width.cutoff = 500L)

  if (grepl("\\)$", dataname) && inherits(data, "data.frame")) {
    stop("Single data.frame shouldn't be provided as a result of a function call. Please name
         the object first or use a named list.")
  }

  if (dataname %in% names(default_cdisc_keys)) {
    cdisc_data(cdisc_dataset(dataname, data))
  } else {
    teal_data(dataset(dataname, data))
  }
}

#' @export
to_relational_data.TealDataset <- function(data) {
  dataname <- get_dataname(data)

  if (dataname %in% names(default_cdisc_keys)) {
    cdisc_data(data)
  } else {
    teal_data(data)
  }
}

#' @export
to_relational_data.TealDatasetConnector <- function(data) { # nolint
  to_relational_data.TealDataset(data)
}

#' @export
to_relational_data.list <- function(data) {
  call <- substitute(data, parent.frame())
  list_names <- names(data)
  parsed_names <- as.character(call)[-1]

  if (
    (
      is_empty(list_names) &&
        is_empty(parsed_names) &&
        (
          any(sapply(data, function(x) inherits(x, "dataset"))) ||
            any(sapply(data, function(x) inherits(x, "data.frame")))
        )
    ) ||
      (any(list_names == "") && is_empty(parsed_names)) ||
      (any(is.na(list_names)))
  ) {
    stop("Unnamed lists shouldn't be provided as input for data. Please use a named list.")
  }

  datasets_list <- lapply(
    seq_along(data),
    function(idx) {
      if (is.data.frame(data[[idx]])) {
        dataname <- if (is_empty(list_names) || list_names[[idx]] == "") {
          parsed_names[[idx]]
        } else {
          list_names[[idx]]
        }

        if (dataname %in% names(default_cdisc_keys)) {
          cdisc_dataset(dataname, data[[idx]])
        } else {
          dataset(dataname, data[[idx]])
        }
      } else if (inherits(data[[idx]], "TealDataset") || inherits(data[[idx]], "TealDatasetConnector")) {
        data[[idx]]
      } else {
        stop("Unknown class to create TealDataset from.")
      }
    }
  )

  if (any(sapply(datasets_list, function(x) inherits(x, "CDISCTealDataset")))) {
    do.call("cdisc_data", args = datasets_list)
  } else {
    do.call("teal_data", args = datasets_list)
  }
}

#' @export
to_relational_data.MultiAssayExperiment <- function(data) { # nolint
  teal_data(mae_dataset("MAE", data))
}
