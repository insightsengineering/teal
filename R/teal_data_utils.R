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
  if (length(code) && !identical(code, "")) {
    data@code <- c(data@code, code2list(code))
    teal.reporter::teal_card(data) <- c(
      teal.reporter::teal_card(data),
      "# Data filtering",
      teal.reporter::code_chunk(code)
    )
    methods::validObject(data)
  }
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

#' @rdname teal_data_utilities
.collapse_subsequent_chunks <- function(report) {
  Reduce(
    function(x, this) {
      l <- length(x)
      if (
        l &&
          inherits(x[[l]], "code_chunk") &&
          inherits(this, "code_chunk") &&
          identical(attr(x[[l]], "params"), attr(this, "params"))
      ) {
        x[[length(x)]] <- do.call(
          code_chunk,
          args = c(
            list(code = paste(x[[l]], this, sep = "\n")),
            attr(x[[l]], "params")
          )
        )
        x
      } else {
        c(x, this)
      }
    },
    init = teal.reporter::teal_card(),
    x = report
  )
}
