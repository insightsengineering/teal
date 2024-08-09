#' `teal_data` utils
#'
#' In `teal` we need to recreate the `teal_data` object due to two operations:integer.max
#' - we need to append filter-data code and objects which have been evaluated in `FilteredData` and
#' we want to avoid double-evaluation.
#' - we need to subset `teal_data` to datanames used by the module, to shorten obtainable R-code
#'
#' Due to above recreation of `teal_data` object can't be done simply by using public
#' `teal.code` and `teal.data` methods.
#'
#' @param data (`teal_data`)
#' @param code (`character`) code to append to `data@code`
#' @param objects (`list`) objects to append to `data@env`
#' @param datanames (`character`) names of the datasets
#' @return modified `teal_data`
#' @name teal_data_utilities
NULL

#' @rdname teal_data_utilities
.append_evaluated_code <- function(data, code) {
  checkmate::assert_class(data, "teal_data")
  data@code <- c(data@code, code)
  data@id <- c(data@id, sample.int(.Machine$integer.max, size = length(code)))
  data@messages <- c(data@messages, rep("", length(code)))
  data@warnings <- c(data@warnings, rep("", length(code)))
  methods::validObject(data)
  data
}

#' @rdname teal_data_utilities
.append_modified_data <- function(data, objects) {
  checkmate::assert_class(data, "teal_data")
  checkmate::assert_class(objects, "list")
  new_env <- list2env(objects, parent = .GlobalEnv)
  rlang::env_coalesce(new_env, data@env)
  data@env <- new_env
  data
}

#' @rdname teal_data_utilities
.subset_teal_data <- function(data, datanames) {
  checkmate::assert_class(data, "teal_data")
  checkmate::assert_class(datanames, "character")
  datanames_corrected <- intersect(datanames, ls(data@env))
  dataname_corrected_with_raw <- intersect(c(datanames, sprintf("%s_raw", datanames)), ls(data@env))

  if (!length(datanames)) {
    return(teal_data())
  }

  new_data <- do.call(
    teal.data::teal_data,
    args = c(
      mget(x = dataname_corrected_with_raw, envir = data@env),
      list(
        code = gsub(
          "warning('Code was not verified for reproducibility.')\n",
          "",
          teal.data::get_code(data, datanames = dataname_corrected_with_raw),
          fixed = TRUE
        ),
        join_keys = teal.data::join_keys(data)[datanames_corrected]
      )
    )
  )
  new_data@verified <- data@verified
  teal.data::datanames(new_data) <- datanames_corrected
  new_data
}

#' @rdname teal_data_utilities
.teal_data_datanames <- function(data) {
  checkmate::assert_class(data, "teal_data")
  if (length(teal.data::datanames(data))) {
    teal.data::datanames(data)
  } else {
    .teal_data_ls(data)
  }
}

#' @rdname teal_data_utilities
.teal_data_ls <- function(data) {
  ls(teal.code::get_env(data), all.names = TRUE)
}
