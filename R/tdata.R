#' Create a `tdata` Object
#'
#' Create a new object called `tdata` which contains `data`, a `reactive` list of data.frames
#' (or `MultiAssayExperiment`), with attributes:
#' \itemize{
#'   \item{`code` (`reactive`) containing code used to generate the data}
#'   \item{join_keys (`join_keys`) containing the relationships between the data}
#'   \item{metadata (`named list`) containing any metadata associated with the data frames}
#' }
#' @name tdata
#' @param data A `named list` of `data.frames` (or `MultiAssayExperiment`)
#'  which optionally can be `reactive`.
#'   Inside this object all of these items will be made `reactive`.
#' @param code A `character` (or `reactive` which evaluates to a `character`) containing
#'   the code used to generate the data. This should be `reactive` if the code is changing
#'   during a reactive context (e.g. if filtering changes the code). Inside this
#'   object `code` will be made reactive
#' @param join_keys A `teal.data::join_keys` object containing relationships between the
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
#' # Get code
#' isolate(get_code(data))
#'
#' # Get metadata
#' get_metadata(data, "iris")
#'
#' @export
new_tdata <- function(data, code = "", join_keys = NULL, metadata = NULL) {
  checkmate::assert_list(
    data,
    any.missing = FALSE, names = "unique",
    types = c("data.frame", "reactive", "MultiAssayExperiment")
  )
  checkmate::assert_class(join_keys, "join_keys", null.ok = TRUE)
  checkmate::assert_multi_class(code, c("character", "reactive"))

  checkmate::assert_list(metadata, names = "unique", null.ok = TRUE)
  checkmate::assert_subset(names(metadata), names(data))
  for (m in metadata) teal.data::validate_metadata(m)

  if (is.reactive(code)) {
    isolate(checkmate::assert_class(code(), "character", .var.name = "code"))
  }

  # create reactive data.frames
  for (x in names(data)) {
    if (!is.reactive(data[[x]])) {
      data[[x]] <- do.call(reactive, list(as.name(x)), envir = list2env(data[x]))
    } else {
      isolate(
        checkmate::assert_multi_class(
          data[[x]](), c("data.frame", "MultiAssayExperiment"),
          .var.name = "data"
        )
      )
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
#' @param data a `tdata` object
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

#' @rdname tdata
#' @param x a `tdata` object
#' @param ... additional arguments for the generic
#' @export
get_code.tdata <- function(x, ...) { # nolint
  # note teal.data which teal depends on defines the get_code method
  attr(x, "code")()
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

#' Extract `join_keys` from `tdata`
#' @param data A `tdata` object
#' @param ... Additional arguments (not used)
#' @export
join_keys.tdata <- function(data, ...) {
  attr(data, "join_keys")
}


#' Function to get metadata from a `tdata` object
#' @param data `tdata` - object to extract the data from
#' @param dataname `character(1)` the dataset name whose metadata is requested
#' @return Either list of metadata or NULL if no metadata
#' @export
get_metadata <- function(data, dataname) {
  checkmate::assert_string(dataname)
  UseMethod("get_metadata", data)
}

#' @rdname get_metadata
#' @export
get_metadata.tdata <- function(data, dataname) {
  metadata <- attr(data, "metadata")
  if (is.null(metadata)) {
    return(NULL)
  }
  metadata[[dataname]]
}

#' @rdname get_metadata
#' @export
get_metadata.default <- function(data, dataname) {
  stop("get_metadata function not implemented for this object")
}
