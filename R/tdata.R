#' Create a `tdata` object
#'
#' @description `r lifecycle::badge("deprecated")`
#'
#' Create a new object called `tdata` which contains `data`, a `reactive` list of `data.frames`
#' (or `MultiAssayExperiment`), with attributes:
#' - `code` (`reactive`) containing code used to generate the data
#' - join_keys (`join_keys`) containing the relationships between the data
#' - metadata (named `list`) containing any metadata associated with the data frames
#'
#' @name tdata
#' @param data (named `list`) A list of `data.frame` or `MultiAssayExperiment` objects,
#'  which optionally can be `reactive`.
#'   Inside this object all of these items will be made `reactive`.
#' @param code (`character` or `reactive` which evaluates to a `character`) containing
#'   the code used to generate the data. This should be `reactive` if the code is changing
#'   during a reactive context (e.g. if filtering changes the code). Inside this
#'   object `code` will be made reactive
#' @param join_keys (`teal.data::join_keys`) object containing relationships between the
#'   datasets.
#' @param metadata (named `list`) each element contains a list of metadata about the named `data.frame`
#' Each element of these list should be atomic and length one.
#' @return A `tdata` object.
#'
#' @seealso `as_tdata`
#'
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
#' isolate(get_code_tdata(data))
#'
#' # Get metadata
#' get_metadata(data, "iris")
#'
#' @export
new_tdata <- function(data, code = "", join_keys = NULL, metadata = NULL) {
  lifecycle::deprecate_soft(
    when = "0.15.0",
    what = "tdata()",
    details = paste(
      "tdata is deprecated and will be removed in the next release. Use `teal_data` instead.\n",
      "Please follow migration instructions https://github.com/insightsengineering/teal/discussions/987."
    )
  )
  checkmate::assert_list(
    data,
    any.missing = FALSE, names = "unique",
    types = c("data.frame", "reactive", "MultiAssayExperiment")
  )
  checkmate::assert_class(join_keys, "join_keys", null.ok = TRUE)
  checkmate::assert_multi_class(code, c("character", "reactive"))

  checkmate::assert_list(metadata, names = "unique", null.ok = TRUE)
  checkmate::assert_subset(names(metadata), names(data))

  if (is.reactive(code)) {
    isolate(checkmate::assert_class(code(), "character", .var.name = "code"))
  }

  # create reactive data.frames
  for (x in names(data)) {
    if (!is.reactive(data[[x]])) {
      data[[x]] <- do.call(reactive, list(as.name(x)), envir = list2env(data[x]))
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
#'
#' Any `reactive` expressions inside `tdata` are evaluated first.
#' @param data (`tdata`) object
#' @return An `environment`.
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


#' Wrapper for `get_code.tdata`
#'
#' This wrapper is to be used by downstream packages to extract the code of a `tdata` object.
#'
#' @param data (`tdata`) object
#'
#' @return (`character`) code used in the `tdata` object.
#' @export
get_code_tdata <- function(data) {
  checkmate::assert_class(data, "tdata")
  attr(data, "code")()
}

#' Extract `join_keys` from `tdata`
#' @param data (`tdata`) object
#' @param ... Additional arguments (not used)
#' @export
join_keys.tdata <- function(data, ...) {
  attr(data, "join_keys")
}

#' Function to get metadata from a `tdata` object
#' @param data (`tdata` - object) to extract the data from
#' @param dataname (`character(1)`) the dataset name whose metadata is requested
#' @return Either list of metadata or NULL if no metadata.
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


#' Downgrade `teal_data` objects in modules for compatibility
#'
#' Convert `teal_data` to `tdata` in `teal` modules.
#'
#' Recent changes in `teal` cause modules to fail because modules expect a `tdata` object
#' to be passed to the `data` argument but instead they receive a `teal_data` object,
#' which is additionally wrapped in a reactive expression in the server functions.
#' In order to easily adapt such modules without a proper refactor,
#' use this function to downgrade the `data` argument.
#'
#' @param x data object, either `tdata` or `teal_data`, the latter possibly in a reactive expression
#'
#' @return Object of class `tdata`.
#'
#' @examples
#' td <- teal_data()
#' td <- within(td, iris <- iris) %>% within(mtcars <- mtcars)
#' td
#' as_tdata(td)
#' as_tdata(reactive(td))
#'
#' @export
#' @rdname tdata_deprecation
#'
as_tdata <- function(x) {
  if (inherits(x, "tdata")) {
    return(x)
  }
  if (is.reactive(x)) {
    checkmate::assert_class(isolate(x()), "teal_data")
    datanames <- isolate(teal_data_datanames(x()))
    datasets <- sapply(datanames, function(dataname) reactive(x()[[dataname]]), simplify = FALSE)
    code <- reactive(teal.code::get_code(x()))
    join_keys <- isolate(teal.data::join_keys(x()))
  } else if (inherits(x, "teal_data")) {
    datanames <- teal_data_datanames(x)
    datasets <- sapply(datanames, function(dataname) reactive(x[[dataname]]), simplify = FALSE)
    code <- reactive(teal.code::get_code(x))
    join_keys <- isolate(teal.data::join_keys(x))
  }

  new_tdata(data = datasets, code = code, join_keys = join_keys)
}
