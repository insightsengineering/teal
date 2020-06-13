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
check_pkg_quietly <- function(pckg, msg) {
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
#' df1 <- teal:::set_labels_df(
#'   data.frame(a = 1, b = 2, c = 3, d = 4),
#'   c(a = "l1", b = "l2", c = "l3")
#' )
#' teal:::get_labels_df(df1)
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

# todo1: put into utils.nest
#' Create a call using a namespaced function
#'
#' The arguments in ... need to be quoted because they will be evaluated otherwise
#'
#' @md
#' @param name `character` function name, possibly using namespace colon `::`
#' @param ... arguments to pass to function with name `name`
#' @param unlist_args `list` extra arguments passed in a single list,
#'   avoids the use of `do.call` with this function
#' @examples
#' `%>%` <- magrittr::`%>%`
#' print_call_and_eval <- function(x) { eval(print(x)) }
#'
#' # mtcars$cyl evaluated
#' teal:::call_with_colon("dplyr::filter", as.name("mtcars"), mtcars$cyl == 6) %>%
#'   print_call_and_eval()
#' # mtcars$cyl argument not evaluated immediately (in call expression)
#' teal:::call_with_colon("dplyr::filter", as.name("mtcars"), rlang::expr(cyl == 6)) %>%
#'   print_call_and_eval()
#'
#' # does not work because argument is evaluated and the
#' # non-dplyr filter does not look inside mtcars
#' # cannot eval becausee it does not pass checks because of non-standard evaluation
#' call("filter", as.name("mtcars"), rlang::expr(cyl == 6))
#' # works, but non-dplyr filter is taken
#' call("filter", as.name("mtcars"), mtcars$cyl == 6)
#'
#' nb_args <- function(...) nargs()
#' teal:::call_with_colon("nb_args", arg1 = 1, unlist_args = list(arg2 = 2, args3 = 3)) %>%
#'   print_call_and_eval()
#' # duplicate arguments
#' teal:::call_with_colon("nb_args", arg1 = 1, unlist_args = list(arg2 = 2, args2 = 2)) %>%
#'   print_call_and_eval()
call_with_colon <- function(name, ..., unlist_args = list()) {
  stopifnot(
    is_character_single(name),
    is.list(unlist_args)
  )
  as.call(c(
    parse(text = name)[[1]],
    c(list(...), unlist_args)
  ))
}

# todo1: move into utils.nest
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
#' @importFrom utils file.edit
see_in_file <- function(f) {
  # will be deleted at end of session
  filename <- tempfile(pattern = paste0(deparse(substitute(f)), "_"), fileext = ".R")
  cat(paste(capture.output(f), collapse = "\n"), file = filename)
  file.edit(filename)
  return(invisible(NULL))
}

#' Move ADSL to the first index in a vector
#'
#' This is useful in the UI as ADSL should always appear first in
#' any vector of the dataset names.
#' If it does not contain ADSL, the vector is returned unmodified.
#'
#' @md
#' @param datanames (`character` vector) datanames
#'
#' @examples
#' list_adsl_first(c("A", "B", "ADSL", "C"))
#' list_adsl_first(c("A", "B", "C"))
list_adsl_first <- function(datanames) {
  if ("ADSL" %in% datanames) {
    # make ADSL first
    datanames <- c("ADSL", setdiff(datanames, "ADSL"))
  }
  return(datanames)
}

#' When active_datanames is "all", sets them to all datanames
#' otherwise, it makes sure that it is a subset of the available datanames
#'
#' @md
#' @param datasets `FilteredData` object
#' @param datanames `character vector` datanames to pick
#'
#' @return intersection of `datasets$datanames()` and `datanames`
handle_active_datanames <- function(datasets, datanames) {
  if (identical(datanames, "all")) {
    datanames <- datasets$datanames()
  }
  return(intersect(datasets$datanames(), datanames))
}

#' Turn a label into a valid html id, prefix a string
#'
#' From both `label` and `prefix`, remove one trailing and
#' one leading "_", then convert all non-alphanumeric characters
#' to "_".
#' This can be used to create a hierarchy within a Shiny module namespace
#' itself, e.g. create nested tabs whose ids all live in one namespace,
#' but where a child tab's name is prefixed with the parent tab's name.
#' See `\link{ui_nested_tabs}`.
#'
#' @md
#' @param label label of module
#' @param prefix `character or NULL` to prepend to label;
#'   `NULL` for no prefix
#'
#' @return valid HTML label with invalid characters removed
#' @examples
#' label_to_id("var", prefix = "prefix")
#' label_to_id("var")
#' label_to_id("__var___", prefix = "prefix")
#' label_to_id("__var___", prefix = "_prefix__")
label_to_id <- function(label, prefix = NULL) {
  stopifnot(is_character_single(label))
  stopifnot(is_character_single(prefix) || is.null(prefix))

  replace_remove_invalid <- function(x) {
    # remove one leading or trailing "_"
    # then replace all non alpha-numeric characters by "_"
    # todo1: explain why?
    gsub("^_|_$", "", gsub("[^[:alnum:]]", "_", x))
  }
  label <- replace_remove_invalid(label)
  if (!is.null(prefix)) {
    prefix <- replace_remove_invalid(prefix)
    paste(prefix, label, sep = "_")
  } else {
    label
  }
}

#' Function to inherit Shiny module arguments that must always be present
#'
#' @md
#' @param input `Shiny input object`
#' @param output `Shiny output object`
#' @param session `Shiny session object`
#' @param modules `teal_module` or `teal_modules` object, the latter
#'   can be used for nested tabs, see `\link{ui_nested_tabs}`
#' @param datasets `FilteredData` object to store filter state and filtered
#'   datasets, shared across modules
srv_shiny_module_arguments <- function(input, output, session, datasets, modules) {
}



# Check functions that error with informative error messages ----

#' Whether the variable name is good to use within Show R Code
#'
#' Spaces are problematic because the variables must be escaped
#' with backticks.
#' Also, they should not start with a number as R may silently make
#' it valid by changing it.
#' Therefore, we only allow alphanumeric characters.
#' The first character of the `name` must be an alphabetic character
#' and can be followed by alphanumeric characters.
#'
#' @md
#' @param name `character, single or vector` name to check
#'
#' @examples
#' check_variable_name_okay("aas2df")
#' check_variable_name_okay("ADSL")
#' check_variable_name_okay("ADSLmodified")
#' check_variable_name_okay("a1")
#' # the following fail
#' \dontrun{
#' check_variable_name_okay("1a")
#' check_variable_name_okay("ADSL.modified")
#' check_variable_name_okay("ADSL_modified")
#' check_variable_name_okay("a1...")
#' }
check_variable_name_okay <- function(name) {
  stopifnot(is_character_single(name) || is_character_vector(name))
  if (!grepl("^[[:alpha:]][[:alnum:]]*$", name)) {
    stop(paste0(
      "name '", name,
      "' must only contain alphanumeric characters and first character must be an alphabetic character"
    ))
  }
  return(invisible(NULL))
}

#' Check that one set is a subset of another
#'
#' Raises an error message if not and says which elements are not in
#' the allowed `choices`.
#'
#' @md
#' @param subset `collection-like` should be a subset of `choices`
#' @param choices `collection-like` superset
#' @param pre_msg `character` message to print before
#'
#' @examples
#' check_in_subset(c("a", "b"), c("a", "b", "c"))
#' \dontrun{
#' check_in_subset(c("a", "b"), c("b", "c"), pre_msg = "Error: ")
#' # truncated because too long
#' check_in_subset("a", LETTERS, pre_msg = "Error: ")
#' }
check_in_subset <- function(subset, choices, pre_msg = "") {
  stopifnot(is_character_single(pre_msg))

  subset <- unique(subset)
  choices <- unique(choices)

  if (any(!(subset %in% choices))) {
    stop(paste0(
      pre_msg,
      "(", toString(subset[!(subset %in% choices)], width = 30), ")",
      " not in valid choices ",
      "(", toString(choices, width = 30), ")"
    ))
  }
  return(invisible(NULL))
}

#' Check that two sets are equal and informative error message otherwise
#'
#' @md
#' @param x object to be compared to other
#' @param y object to be compared to other
#' @param pre_msg `character` to be displayed before error message
#' @examples
#' check_setequal(1:3, 1:3)
#' check_setequal(c(1, 1, 3), c(1, 3, 3))
#' try(check_setequal(c(1, 2, 3), c(1, 3, 3), pre_msg = "Not equal: "))
#' try(check_setequal(c(1, 2, 3), c("a", "b"), pre_msg = "Not equal: "))
check_setequal <- function(x, y, pre_msg = "") {
  stopifnot(is_character_single(pre_msg))
  x <- unique(x)
  y <- unique(y)
  if (!setequal(x, y)) {
    stop(paste0(
      pre_msg,
      "(", toString(x, width = 100), ")",
      " is not equal to ",
      "(", toString(y, width = 100), ")"
    ))
  }
  return(invisible(NULL))
}

#' Whether the element is a Shiny HTML element
#'
#' The classes returned by `shiny::tag`, `shiny::tagList`, `shiny::HTML`
#' are one of `"shiny.tag", "shiny.tag.list", "html"`. This checks if the element
#' has any of these classes.
#'
#' @md
#' @param x any object
#' @return `logical` whether the element is Shiny HTML like
is_html_like <- function(x) {
  inherits(x, c("shiny.tag", "shiny.tag.list", "html")) # logical or
}
