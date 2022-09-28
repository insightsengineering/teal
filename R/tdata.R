#' Create a new `tdata` object which contains
#'
#' - `data` a `reactive` list of data.frames (or `MultiAssayExperiment`) with attributes
#' i) `code` (`reactive`) containing code used to generate the data
#' and ii) join_keys (`JoinKeys`) containing the relationships between
#' the data iii) metadata (`named list`) containing any metadata associated with
#' the data frames
#'
#' @param data A `named list` of `data.frames` (or `MultiAssayExperiment`)
#'  which optionally can be `reactive`.
#'   Inside this object all of these items will be made `reactive`.
#' @param code A `character` (or `reactive` which evaluates to a `character`) containing
#'   the code used to generate the data. This should be `reactive` if the code is changing
#'   during a reactive context (e.g. if filtering changes the code). Inside this
#'   object `code` will be made reactive
#' @param join_keys A `teal.data::JoinKeys` object containing relationships between the
#'   datasets.
#' @param metadata A `named list` each element contains a list of metadata about the named data.frame
#' Each element of these list should be atomic and length one.
#' @return A `tdata` object
#' @examples
#'
#' data <- new_tdata(
#'   data = list(iris = iris, mtcars = reactive(mtcars), dd = data.frame(x = 1:10)),
#'   code = "iris <- iris
#'     mtcars <- mtcars
#'     dd <- data.frame(x = 1:10)",
#'   metadata = list(dd = list(author = "NEST"), iris = list(version = 1))
#' )
#'
#' # Extract a data.frame
#' isolate(data[["iris"]]())
#'
#' @export
new_tdata <- function(data, code = "", join_keys = NULL, metadata = NULL) {
  checkmate::assert_list(
    data,
    any.missing = FALSE, names = "unique",
    types = c("data.frame", "reactive", "MultiAssayExperiment")
  )
  checkmate::assert_class(join_keys, "JoinKeys", null.ok = TRUE)
  checkmate::assert_multi_class(code, c("character", "reactive"))

  checkmate::assert_list(metadata, names = "unique", null.ok = TRUE)
  checkmate::assert_subset(names(metadata), names(data))
  for (m in metadata) teal.data::validate_metadata(m)

  if (is.reactive(code)) {
    isolate(checkmate::assert_class(code(), "character"))
  }

  # create reactive data.frames
  for (x in names(data)) {
    if (!is.reactive(data[[x]])) {
      data[[x]] <- do.call(reactive, list(as.name(x)), envir = list2env(data[x]))
    } else {
      isolate(checkmate::assert_multi_class(data[[x]](), c("data.frame", "MultiAssayExperiment")))
    }
  }

  # set attributes
  attr(data, "code") <- if (is.reactive(code)) code else reactive(code)
  attr(data, "join_keys") <- join_keys
  attr(data, "metadata") <- metadata

  # set class
  class(data) <- c("tdata", class(data))
  data
}

#' Function to convert a `tdata` object to an `environment`
#' Any `reactives` inside `tdata` are first evaluated
#' @param `data` a `tdata` object
#' @return an `environment`
#' @examples
#'
#' data <- new_tdata(
#'   data = list(iris = iris, mtcars = reactive(mtcars)),
#'   code = "iris <- iris
#'     mtcars = mtcars"
#' )
#'
#' my_env <- isolate(tdata2env(data))
#'
#' @export
tdata2env <- function(data) { # nolint
  checkmate::assert_class(data, "tdata")
  list2env(lapply(data, function(x) if (is.reactive(x)) x() else x))
}

#' @export
get_code.tdata <- function(data) {
  # note teal.data which teal depends on defines the get_code method
  attr(data, "code")()
}

#' Wrapper for `get_code.tdata`
#' This wrapper is to be used by downstream packages to extract the code of a `tdata` object
#'
#' @param data (`tdata`) object
#'
#' @return (`character`) code used in the `tdata` object.
#' @export
get_code_tdata <- function(data) {
  checkmate::assert_class(data, "tdata")
  get_code(data)
}

#' @export
get_join_keys <- function(data) {
  UseMethod("get_join_keys", data)
}

#' @export
get_join_keys.tdata <- function(data) {
  attr(data, "join_keys")
}

#' @export
get_join_keys.default <- function(data) {
  stop("get_join_keys function not implemented for this object")
}

#' @export
get_metadata <- function(data, dataname) {
  checkmate::assert_string(dataname)
  UseMethod("get_metadata", data)
}

#' @export
get_metadata.tdata <- function(data, dataname) {
  metadata <- attr(data, "metadata")
  if (is.null(metadata)) {
    return(NULL)
  }
  metadata[[dataname]]
}

#' @export
get_metadata.default <- function(data, dataname) {
  stop("get_metadata function not implemented for this object")
}
