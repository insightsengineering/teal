#' @export
new_tdata <- function(data, code, join_keys) {

  #validation here

  # code from previous stages
  attr(data, "code") <- code

  # join_keys
  attr(data, "join_keys") <- join_keys

  # set class
  class(data) <- c("tdata", class(data))
  data
}

#' @export
tdata2env <- function(data) { # nolint
  #validation

  list2env(lapply(data, function(x) if (is.reactive(x)) x() else x))
}

#' @export
get_code <- function(data) {
  UseMethod("get_code", data)
}

#' @export
get_join_keys <- function(data) {
  UseMethod("get_join_keys", data)
}

#' @export
get_code.tdata <- function(data) {
  attr(data, "code")()
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
get_code.default <- function(data) {
  stop("get_code function not implemented for this object")
}
