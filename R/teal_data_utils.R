#' `teal_data` utils
#'
#' In `teal` we need to recreate the `teal_data` object due to two operations:
#' - we need to append filter-data code and objects which have been evaluated in `FilteredData` and
#' we want to avoid double-evaluation.
#' - we need to subset `teal_data` to `datanames` used by the module, to shorten obtainable R-code
#'
#' Due to above recreation of `teal_data` object can't be done simply by using public
#' `teal.code` and `teal.data` methods.
#'
#' @param data (`teal_data`)
#' @param code (`character`) code to append to the object's code slot.
#' @param objects (`list`) objects to append to object's environment.
#' @return modified `teal_data`
#' @keywords internal
#' @name teal_data_utilities
NULL

#' @rdname teal_data_utilities
.append_evaluated_code <- function(data, code) {
  checkmate::assert_class(data, "teal_data")
  data@code <- c(data@code, code2list(code))
  methods::validObject(data)
  data
}

#' @rdname teal_data_utilities
.append_modified_data <- function(data, objects) {
  checkmate::assert_class(data, "teal_data")
  checkmate::assert_class(objects, "list")
  new_env <- list2env(objects, parent = .GlobalEnv)
  rlang::env_coalesce(new_env, as.environment(data))
  data@.xData <- new_env
  data
}
