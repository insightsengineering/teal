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

# sodo3: test this function or put into another package?
#' Restore labels from a list of `data.frames`
#'
#' This is useful to restore labels after `dplyr` transformations where labels
#' are typically lost.
#' This also works well with merged datasets to restore labels from its
#' constituent datasets.
#' In case of columns with the same name in `from`, the label from the
#' first `data.frame` in the `from` list is used.
#'
#' @md
#' @param x `data.frame` that needs labels
#' @param from `list of data.frames` to take labels from
#' @param x_label_precedence `logical` whether labels from `x` take precedence
#'   when there are also labels for a given column in the `from` list
#'
#' @examples
#' set_labels_df <- teal:::set_labels_df
#' get_labels_df <- teal:::get_labels_df
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
#' stopifnot(all(
#'   get_labels_df(teal:::restore_labels(df0, list(df1, df2, df3))) ==
#'     c(a = "l1", b = "l2", c = "l3_orig", d = "l444", i = "l9_orig")
#' ))
#' stopifnot(all(
#'   get_labels_df(teal:::restore_labels(df0, list(df1, df2, df3), x_label_precedence = FALSE)) ==
#'     c(a = "l1", b = "l2", c = "l3", d = "l444", i = "l9_orig")
#' ))
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
# see `set_labels_df`
get_labels_df <- function(df) {
  unlist(Filter(Negate(is.null), lapply(df, function(col) attr(col, "label"))))
}

#' Set labels of a data.frame
#'
#' @param df `data.frame`
#' @param labels named `character vector` of labels,
#'   must all be column names present in `df`
#' @md
#' @return `data.frame` with labels set
#' @examples
#' df1 <- set_labels_df(
#'   data.frame(a = 1, b = 2, c = 3, d = 4),
#'   c(a = "l1", b = "l2", c = "l3")
#' )
#' get_labels_df(df1)
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

# sodo3: put into utils.nest
#' Create a call using a namespaced function
#'
#' The arguments in ... need to be quoted because they will be evaluated otherwise
#'
#' @examples
#' `%>%` <- magrittr::`%>%`
#' print_call_and_eval <- function(x) { eval(print(x)) }
#'
#' # mtcars$cyl evaluated
#' teal:::call_with_colon("dplyr::filter", as.name("mtcars"), mtcars$cyl == 6) %>% print_call_and_eval()
#' # mtcars$cyl argument not evaluated immediately (in call expression)
#' teal:::call_with_colon("dplyr::filter", as.name("mtcars"), rlang::expr(cyl == 6)) %>% print_call_and_eval()
#'
#' # does not work because argument is evaluated and the
#' # non-dplyr filter does not look inside mtcars
#' call("filter", as.name("mtcars"), rlang::expr(.data$cyl == 6)) %>% print_call_and_eval()
#' # works, but non-dplyr filter is taken
#' call("filter", as.name("mtcars"), mtcars$cyl == 6) %>% print_call_and_eval()
call_with_colon <- function(name, ...) {
  as.call(c(
    parse(text = name)[[1]],
    list(...)
  ))
}

# Get a random joke to display while the app is starting up
get_random_joke <- function() {
  # downloaded from https://raw.githubusercontent.com/Daronspence/One-Liners/master/jokes.txt
  file_contents <- readLines(system.file("jokes.txt", package = "teal", mustWork = TRUE))

  # jokes are separated by empty lines (possibly several) each, a joke can be over several lines
  file_contents <- c("", file_contents, "") # add sentinel values
  # a joke must start after an empty line and end before an empty line
  start_positions <- head(which(file_contents == "") + 1, -1) # all except for last element
  end_positions <- (which(file_contents == "") - 1)[-1] # all except for first
  valid_indices <- which(start_positions <= end_positions) # filter when there are several empty lines in a row

  # sample joke
  joke_idx <- sample(valid_indices, 1)
  return(paste(
    file_contents[
      start_positions[[joke_idx]]:end_positions[[joke_idx]]
    ],
    collapse = "\n"
  ))
}

# sodo3: move into utils.nest
#' See a function's code in a temporary file
#' This is more handy when you want to search for variables in the code a function
#' rather than doing so in the console.
#'
#' Debugging function
#'
#' The file is created in the temporary directory of the R session and
#' will be deleted once the R session exits.
#'
#' @param f function or object that is printed and whose output is shown in the file
#' @examples
#' \dontrun{
#' teal:::see_in_file(factor)
#' }
see_in_file <- function(f) {
  # will be deleted at end of session
  filename <- tempfile(pattern = paste0(deparse(substitute(f)), "_"), fileext = ".R")
  cat(paste(capture.output(f), collapse = "\n"), file = filename)
  file.edit(filename)
  return(invisible(NULL))
}
