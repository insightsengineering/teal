#' Create a new `tdata` object which contains
#'
#' - `data` a `reactive` list of data.frames with attributes
#' i) `code` (`reactive`) containing code used to generate the data
#' and ii) join_keys (`JoinKeys`) containing the relationships between
#' the data
#'
#' @param data A `named list` of `data.frames` which optionally can be `reactive`.
#'   Inside this object all `data.frames` will be made `reactive`.
#' @param code A `character` (or `reactive` which evaluates to a `character`) containing
#'   the code used to generate the data. This should be `reactive` if the code is changing
#'   during a reactive context (e.g. if filtering changes the code). Inside this
#'   object `code` will be made reactive
#' @param join_keys A `teal.data::JoinKeys` object containing relationships between the
#'   datasets.
#'
#' @return A `tdata` object
#' @examples
#'
#' data <- new_tdata(
#'   data = list(iris = iris, mtcars = reactive(mtcars), dd = data.frame(x = 1:10)),
#'   code = "iris <- iris
#'     mtcars <- mtcars
#'     dd <- data.frame(x = 1:10)"
#' )
#'
#' # Extract a data.frame
#' isolate(data[["iris"]]())
#'
#' @export
new_tdata <- function(data, code = "", join_keys = NULL) {
  checkmate::assert_list(
    data,
    types = c("data.frame", "reactive"), any.missing = FALSE, names = "unique"
  )
  checkmate::assert_class(join_keys, "JoinKeys", null.ok = TRUE)
  checkmate::assert_multi_class(code, c("character", "reactive"))

  #create reactive data.frames
  for (x in names(data)) {
    if (!is.reactive(data[[x]])) {
      data[[x]] <- do.call(reactive, list(as.name(x)), envir = list2env(data[x]))
    } else{
      isolate(checkmate::assert_class(data[[x]](), "data.frame"))
    }
  }

  # set attributes
  attr(data, "code") <- if (is.reactive(code)) code else reactive(code)
  attr(data, "join_keys") <- join_keys

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
