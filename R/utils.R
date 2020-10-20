# also returns a list if only a single element
#' Split by separator
#'
#' @md
#' @description `r lifecycle::badge("maturing")`
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
#' @md
#' @description `r lifecycle::badge("maturing")`
#' Checks if \code{x} element matches any of \code{y} element. If one of the arguments is a list then list elements
#' are treated as whole - in this case list elements can be a vector, so it looks
#' for equal element in second vector to be matched.
#'
#' @param x \code{list} to be matched.
#' @param y \code{list} to be matched against.
#' @return \code{logical} vector length of \code{x} denoting if element was found in second list.
#' @export
`%is_in%` <- function(x, y) {
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
          pdeparse(x$call),
          pdeparse(attr(x, "condition")$call)
        ), "\n"),
        tags$br(),
        tags$span("Error message:"),
        tags$br(),
        tags$code(
          ifelse("condition" %in% class(x),
            pdeparse(x$message),
            pdeparse(attr(x, "condition")$message)
          )
        )
      )
    )
    req(FALSE)
  } else {
    stop(x)
  }
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
#' list_adsl_first <- teal:::list_adsl_first
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
  # convert error to warning
  tryCatch(
    check_in_subset(datanames, datasets$datanames(), "Some datasets are not available: "),
    error = function(e) {
      message(e$message)
    }
  )
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
#' label_to_id <- teal:::label_to_id
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
    # explain why?
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
srv_shiny_module_arguments <- function(input, output, session, datasets, modules) { # nousage # nolint
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
#' check_in_subset <- teal:::check_in_subset
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
    ), call. = FALSE)
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
#' check_setequal <- teal:::check_setequal
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
    ), call. = FALSE)
  }
  return(invisible(NULL))
}

pdeparse <- function(x, width.cutoff = 500L) { # nolint
  paste0(deparse(x, width.cutoff = width.cutoff), collapse = "\n") # nolint
}

teal_with_pkg <- function(pkg, code) {
  pkg_name <- paste0("package:", pkg)
  if (! pkg_name %in% search()) {
    require(pkg, character.only = TRUE)
    on.exit(detach(pkg_name, character.only = TRUE))
  }
  eval.parent(code)
  return(invisible(NULL))
}
