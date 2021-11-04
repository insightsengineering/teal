#' S3 generic for \code{to_relational_data} function.
#'
#' Takes the input of data argument and translates them to relational data objects.
#'
#' @param data `Dataset`, `DatasetConnector`, `data.frame`, `list` or `function` returning a named list.
#'
#' @return list of `RelationalData` objects
#'
#' @note `to_relational_data` should only be used inside `init` call to guarantee correct behavior.
#'
to_relational_data <- function(data) {
  UseMethod("to_relational_data")
}

#' @export
to_relational_data.data.frame <- function(data) { # nolint #nousage
  dataname <- deparse(substitute(data, parent.frame()), width.cutoff = 500L)

  if (grepl("\\)$", dataname) && is(data, "data.frame")) {
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
to_relational_data.Dataset <- function(data) { #nousage
  dataname <- get_dataname(data)

  if (dataname %in% names(default_cdisc_keys)) {
    cdisc_data(data)
  } else {
    teal_data(data)
  }
}

#' @export
to_relational_data.DatasetConnector <- function(data) { # nolint #nousage
  to_relational_data.Dataset(data)
}

#' @export
to_relational_data.list <- function(data) { #nousage
  call <- substitute(data, parent.frame())
  list_names <- names(data)
  parsed_names <- as.character(call)[-1]

  if (
    (is_empty(list_names) && is_empty(parsed_names) &&
     (any(sapply(data, function(x) is(x, "dataset"))) || any(sapply(data, function(x) is(x, "data.frame"))))) ||
     (any(list_names == "") && is_empty(parsed_names)) || (any(is.na(list_names)))
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
      } else if (is(data[[idx]], "Dataset") || is(data[[idx]], "DatasetConnector")) {
        data[[idx]]
      } else {
        stop("Unknown class to create Dataset from.")
      }
    }
  )

  if (any(sapply(datasets_list, function(x) is(x, "CDISCDataset")))) {
    do.call("cdisc_data", args = datasets_list)
  } else {
    do.call("teal_data", args = datasets_list)
  }
}
