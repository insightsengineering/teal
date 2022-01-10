#' Create a new `FilteredData` object
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param x an object that inherits from `TealData`
#'
#' @return a (`CDISCTealDataset`, `CDISCTealDatasetConnector`) object
#'
#' @noRd
filtered_data_new <- function(x) {
  UseMethod("filtered_data_new")
}

#' @export
filtered_data_new.TealData <- function(x) { # nolintr # nousage
  FilteredData$new()
}

#' @export
filtered_data_new.CDISCTealData <- function(x) { # nolintr # nousage
  CDISCFilteredData$new()
}

#' Set `FilteredData` with data from `TealData`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param data an object that inherits from `TealData`
#' @param datasets an object that inherits from `FilteredData`
#'
#' @return modified `FilteredData` object
#'
#' @noRd
filtered_data_set <- function(data, datasets) { # nolintr # nousage
  UseMethod("filtered_data_set", data)
}

#' @export
filtered_data_set.TealData <- function(data, datasets) { # nolintr # nousage
  datasets$set_code(data$get_code_class())
  for (dataset in data$get_datasets()) {
    datasets$set_dataset(dataset)
  }

  return(invisible(NULL))
}



#' Managing `FilteredData` states
#'
#' Set, get and remove filter states of `FilteredData` object
#'
#' @name filter_state_api
#' @inheritParams srv_tabs_with_filters
#' @inheritParams init
#'
#' @return
#' - set, remove and clear returns `NULL`
#' - get returns named `list` of the same structure as described in `filter` argument.
#'
#' @examples
#' datasets <- teal:::FilteredData$new()
#' datasets$set_dataset(dataset("iris", iris))
#' datasets$set_dataset(dataset("mae", MultiAssayExperiment::miniACC))
#' fs <- list(
#'   iris = list(
#'     Sepal.Length = list(selected = c(5.1, 6.4), keep_na = TRUE, keep_inf = FALSE),
#'     Species = list(selected = c("setosa", "versicolor"), keep_na = FALSE)
#'   ),
#'   mae = list(
#'     subjects = list(
#'       years_to_birth = list(selected = c(30, 50), keep_na = TRUE, keep_inf = FALSE),
#'       vital_status = list(selected = "1", keep_na = FALSE),
#'       gender = list(selected = "female", keep_na = TRUE)
#'     ),
#'     RPPAArray = list(
#'       subset = list(ARRAY_TYPE = list(selected = "", keep_na = TRUE))
#'     )
#'   )
#' )
#'
#' # set initial filter state
#' set_filter_state(datasets, filter = fs)
#'
#' # get filter state
#' get_filter_state(datasets)
#'
#' # modify filter state (works only in shiny app)
#' set_filter_state(
#'   datasets,
#'   filter = list(iris = list(Species = list(selected = "setosa", keep_na = TRUE)))
#' )
#'
#' # remove specific filters
#' remove_filter_state(datasets,
#'   filter = list(
#'     iris = "Species",
#'     mae = list(
#'       subjects = c("years_to_birth", "vital_status")
#'     )
#'   )
#' )
#'
#' # remove all states
#' clear_filter_states(datasets)
#'
#' @export
set_filter_state <- function(datasets, filter) {
  checkmate::assert_class(datasets, "FilteredData")
  checkmate::assert_list(filter, min.len = 0, null.ok = TRUE)
  if (length(filter) > 0) {
    datasets$set_filter_state(filter)
  }
  invisible(NULL)
}

#' @rdname filter_state_api
#' @export
get_filter_state <- function(datasets) {
  checkmate::assert_class(datasets, "FilteredData")
  if (shiny::isRunning()) {
    datasets$get_filter_state()
  } else {
    isolate(datasets$get_filter_state())
  }
}

#' @rdname filter_state_api
#' @export
remove_filter_state <- function(datasets, filter) {
  checkmate::assert_class(datasets, "FilteredData")
  checkmate::assert_list(filter, min.len = 0, null.ok = TRUE)
  if (length(filter) > 0) {
    datasets$remove_filter_state(filter)
  }
  invisible(NULL)
}

#' @rdname filter_state_api
#' @export
clear_filter_states <- function(datasets) {
  checkmate::assert_class(datasets, "FilteredData")
  datasets$remove_all_filter_states()
  invisible(NULL)
}
