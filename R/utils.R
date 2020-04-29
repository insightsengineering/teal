# are elements unique
is.unique <- function(x) { # nolint
  length(x) == length(unique(x))
}

# also returns a list if only a single element
#' Split by separator
#'
#' @param x (\code{character}) Character (single)
#' @param sep (\code{character}) Separator
#' @export
split_by_sep <- function(x, sep) {
  stopifnot(is.atomic(x))
  strsplit(x, sep, fixed = TRUE)
}

#' List element in other list
#'
#' Checks if \code{x} element matches any of \code{y} element. If one of the arguments is a list then list elements
#' are treated as whole - in this case list elements can be a vector, so it looks
#' for equal element in second vector to be matched.
#'
#' @param x \code{list} to be matched.
#' @param y \code{list} to be matched against.
#' @return \code{logical} vector length of \code{x} denoting if element was found in second list.
#' @export
`%is_in%`  <- function(x, y) {
  if (!is.list(x) & is.list(y)) {
    x <- list(x)
  } else if (!is.list(y) & is.list(x)) {
    y <- list(y)
  }
  vapply(x, function(x1) {
    any(vapply(y, function(y1) isTRUE(all.equal(y1, x1)), logical(1), USE.NAMES = FALSE))
  }, logical(1), USE.NAMES = FALSE)
}

#' Extract labels from choices basing on attributes and names
#'
#' @param choices (\code{list} or \code{vector}) select choices
#' @param values optional, choices subset for which labels should be extracted, \code{NULL} for all choices
#'
#' @return (\code{character}) vector with labels
extract_choices_labels <- function(choices, values = NULL) {
  res <- if (is(choices, "choices_labeled")) {
    attr(choices, "raw_labels")
  } else if (!is.null(names(choices)) && !setequal(names(choices), unlist(unname(choices)))) {
    names(choices)
  } else {
    NULL
  }

  if (!is.null(values) && !is.null(res)) {
    stopifnot(all(values %in% choices))
    res <- res[vapply(values, function(val) which(val == choices), numeric(1))]
  }

  return(res)
}


#' Check if package can be loaded
#'
#' @param pckg \code{character} package name.
#' @param msg \code{character} error message to display if package is not available.
#'
#' @return Error or invisible NULL.
#'
check_pckg_quietly <- function(pckg, msg) {
  stopifnot(is_character_single(pckg), is_character_single(msg))

  if (!requireNamespace(pckg, quietly = TRUE)) {
    stop(msg)
  }

  return(invisible(NULL))
}

#' @importFrom shiny isRunning showModal modalDialog tags req
error_dialog <- function(x) {
  if (shiny::isRunning()) {
    showModal(
        modalDialog(
            tags$span("Error while evaluating following call:"),
            tags$br(),
            tags$code(ifelse(
              "condition" %in% class(x),
              deparse(x$call),
              deparse(attr(x, "condition")$call)
            ), "\n"),
            tags$br(),
            tags$span("Error message:"),
            tags$br(),
            tags$code(
              ifelse("condition" %in% class(x),
                deparse(x$message),
                deparse(attr(x, "condition")$message)
              )
            )
        )
    )
    req(FALSE)
  } else {
    stop(x)
  }
}


#' Restore labels from a list of data.frames
#'
#' This is useful to restore labels after dplyr transformations where labels
#' are typically lost.
#' This also works well with merged datasets.
#' In case of columns of the same name in `from`, the label from the
#' first data.frame in the `from` list is used.
#'
#' @md
#' @param x `data.frame `
#' @param from list of data.frames
#' @param x_label_precedence `logical` whether labels from x take precedence
#'   when there are also labels for a given column in the `from` list
#'
#' @examples
#' df0 <- set_labels_df(
#'   data.frame(a = 1, b = 2, c = 3, d = 4, i = 8, x = 24),
#'   c(c = "l3_orig", i = "l9_orig")
#' )
#' get_labels_df(df0)
#' df1 <- set_labels_df(
#'   data.frame(a = 1, b = 2, c = 3, d = 4),
#'   c(a = "l1", b = "l2", c = "l3")
#' )
#' get_labels_df(df1)
#' df2 <- set_labels_df(
#'   data.frame(a = 11, d = 44, e = 55),
#'   c(a = "l11")
#' )
#' get_labels_df(df2)
#' df3 <- set_labels_df(
#'   data.frame(a = 111, c = 333, d = 444, e = 555),
#'   c(a = "l111", c = "l333", d = "l444")
#' )
#' get_labels_df(df3)
#' all(get_labels_df(restore_labels(df0, list(df1, df2, df3))) == c(a = "l1", b = "l2", c = "l3_orig", d = "l444", i = "l9_orig"))
#' all(get_labels_df(restore_labels(df0, list(df1, df2, df3), x_label_precedence = FALSE)) == c(a = "l1", b = "l2", c = "l3", d = "l444", i = "l9_orig"))
restore_labels <- function(x, from, x_label_precedence = TRUE) {
  stopifnot(
    is.data.frame(x),
    is.list(from),
    all_true(from, is.data.frame),
    is_logical_single(x_label_precedence)
  )
  # named list of labels with labels from first items in from list taking precedence
  labels_from <- Reduce(
    function(cur_labels, new_labels) {
      # add only new labels
      c(cur_labels, new_labels[!names(new_labels) %in% names(cur_labels)])
    },
    lapply(from, get_labels_df)
  )

  labels_from <- labels_from[names(labels_from) %in% colnames(x)]
  if (x_label_precedence) {
    labels_from <- labels_from[!names(labels_from) %in% names(get_labels_df(x))]
  }
  return(set_labels_df(x, labels_from))
}

# get non-NULL labels for single df
get_labels_df <- function(df) {
  unlist(Filter(Negate(is.null), lapply(df, function(col) attr(col, "label"))))
}
# df1 <- set_labels_df(
#   data.frame(a = 1, b = 2, c = 3, d = 4),
#   c(a = "l1", b = "l2", c = "l3")
# )
# get_labels_df(df1)
set_labels_df <- function(df, labels) {
  stopifnot(is.data.frame(df))
  stopifnot(is_character_vector(labels, min_length = 0))
  stopifnot(is_fully_named_list(as.list(labels)), all(names(labels) %in% colnames(df)))
  for (i in seq_along(labels)) {
    col <- names(labels)[[i]]
    attr(df[[col]], "label") <- labels[[i]]
  }
  return(df)
}

