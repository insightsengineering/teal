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

#' Get Client Timezone
#'
#' Local timezone in the browser may differ from the system timezone from the server.
#'   This script can be run to register a shiny input which contains information about
#'   the timezone in the browser.
#' @md
#'
#' @param ns (`function`) namespace function passed from the `session` object in the
#'   Shiny server. For Shiny modules this will allow for proper name spacing of the
#'   registered input.
#'
#' @return (`Shiny`) input variable accessible with `input$tz` which is a (`character`)
#'  string containing the timezone of the browser/client.
#'
#' @importFrom shinyjs runjs
get_client_timezone <- function(ns) {
  script <- paste0(
  "var tz = Intl.DateTimeFormat().resolvedOptions().timeZone;
  Shiny.onInputChange('", ns("tz"), "', Intl.DateTimeFormat().resolvedOptions().timeZone);"
  )
  shinyjs::runjs(script)
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

#' Check that a given range is valid
#' @md
#' @param subinterval (`numeric` or `date`) vector of length 2 to be
#'   compared against the full range.
#' @param range (`numeric` or `date`) vector of length 2 containing
#'   the full range to validate against.
#' @param pre_msg `character` message to print before error for
#'   additional context.
#'
#' @return `NULL` if `subinterval` is a valid range or error with message
#'   otherwise.
#'
#' @examples
#' \dontrun{
#' check_in_range(c(3,1), c(1,3))
#' check_in_range(
#'   c(as.Date("2020-01-01"), as.Date("2020-01-20")),
#'   c(as.Date("2020-01-01"), as.Date("2020-01-02"))
#'   )
#' }
check_in_range <- function(subinterval, range, pre_msg = "") {
  if ((length(subinterval) != 2) ||
      (subinterval[[1]] > subinterval[[2]]) ||
      ((subinterval[[1]] < range[[1]]) || (subinterval[[2]] > range[[2]]))
  ) {
    stop(paste0(
      pre_msg, " range (", toString(subinterval),
      ") not valid for full range (", toString(range), ")"
    ))
  }
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

#' Get code from script
#'
#' Get code from script. Switches between `code` and `script` arguments
#' to return non-empty one to pass it further to constructors.
#' @md
#'
#' @param code (`character`)\cr
#'   an R code to be evaluated or a `PythonCodeClass` created using [python_code].
#' @inheritParams relational_dataset_connector
#' @return code (`character`)
code_from_script <- function(code, script, dataname = NULL) {
  stopifnot(
    is_character_vector(code, min_length = 0, max_length = 1) || inherits(code, "PythonCodeClass"),
    is_character_vector(script, min_length = 0, max_length = 1)
    )
  if (length(code) == 0 && length(script) == 0) {
    return(character(0))
  }

  if (is_character_single(code) && is_character_single(script)) {
    stop("Function doesn't accept 'code' and 'script' at the same time.
         Please specify either 'code' or 'script'", call. = FALSE)
  }

  if (is_character_single(script)) {
    code <- read_script(file = script, dataname = dataname)
  }

  return(code)
}

#' Read .R file into character
#'
#' @md
#' @description `r lifecycle::badge("maturing")`
#' Comments will be excluded
#'
#' @param file (\code{character}) File to be parsed into code
#' @param dataname (\code{character}) dataset name to subset code from chunks
#' @return (\code{character}) vector with the code
#'
#' @export
#' @examples
#' file_example <- tempfile()
#' writeLines(c("x <- 2", "#second line comment", "x <- x + 2"), file_example)
#'
#' read_script(file_example)
#'
#' @importFrom magrittr %>%
read_script <- function(file, dataname = NULL) {
  stopifnot(is_character_single(file))
  stopifnot(file.exists(file))
  get_code_single(file, read_sources = TRUE) %>%
    enclosed_with_dataname(dataname = dataname) %>%
    code_exclude(exclude_comments = TRUE) %>%
    paste(sep = "\n", collapse = "\n")
}

#' S3 generic for creating an information summary about the duplicate key values in a dataset
#'
#' @md
#' @description `r lifecycle::badge("experimental")`
#'
#' @details The information summary provides row numbers and number of duplicates
#' for each duplicated key value.
#'
#' @param dataset \code{RelationalDataset} or \code{dataframe} a dataset, which will be tested
#' @param keys \code{character} vector of variable names in `dataset` consisting the key
#' or \code{keys} object, which does have a `primary` element with a vector of variable
#' names in `dataset` consisting the key. Optional, default: NULL
#'
#' @return a \code{tibble} with variables consisting the key and \code{row_no} and \code{duplicates_count} columns
#'
#' @examples
#' library(random.cdisc.data)
#'
#' adsl <- radsl(cached = TRUE)
#' # Creates a RelationalDataset, keys are automatically assigned,
#' # because the name is in the recognized ADAM nomenclature ("ADSL")
#' rel_adsl <- cdisc_dataset("ADSL", adsl)
#' get_key_duplicates(rel_adsl)
#'
#' df <- as.data.frame(
#'   list(a = c('a', 'a', 'b', 'b', 'c'), b = c(1, 2, 3, 3, 4), c = c(1, 2, 3, 4, 5))
#' )
#' res <- get_key_duplicates(df, keys = c('a', 'b')) # duplicated keys are in rows 3 and 4
#' print(res) # prints a tibble
#'
#' \dontrun{
#' get_key_duplicates(df) # raises an exception, because keys are missing with no default
#' }
#'
#' @seealso \itemize{
#' \item{\link{get_key_duplicates_util}}
#' \item{\link{get_key_duplicates.RelationalDataset}}
#' \item{\link{get_key_duplicates.data.frame}}
#' }
#'
#' @export
get_key_duplicates <- function(dataset, keys = NULL) {
 UseMethod("get_key_duplicates", dataset)
}


#' Creates a short information summary about the duplicate primary key values in a dataset
#'
#' @md
#' @description `r lifecycle::badge("experimental")`
#' @details S3 method for get_key_duplicates. Uses the public API of
#' `teal.devel::RelationalDataset` to read the primary key and the raw data.
#'
#' If `keys` argument is provided, then checks against that, if it's `NULL`, then checks
#' against the \code{get_keys()$primary} method of the `dataset` argument.
#'
#' @inheritParams get_key_duplicates
#' @param dataset a \code{RelationalDataset} object, which will be used to detect duplicated
#' primary keys
#'
#' @examples
#' library(random.cdisc.data)
#'
#' adsl <- radsl(cached = TRUE)
#' # Keys are automatically assigned, because the name
#' # is in the recognized ADAM nomenclature ("ADSL")
#' rel_adsl <- cdisc_dataset("ADSL", adsl)
#' get_key_duplicates(rel_adsl)
#'
#' @seealso \link{get_key_duplicates} \link{get_key_duplicates_util}
#'
#' @export
get_key_duplicates.RelationalDataset <- function(dataset, keys = NULL) { #nolint
  stopifnot("RelationalDataset" %in% class(dataset))

  df <- dataset$get_raw_data()
  keys <- if ("keys" %in% class(keys)) {
    keys$primary
  } else if (!is.null(keys)) {
    keys
  } else if (!is.null(dataset$get_keys()$primary)) {
    dataset$get_keys()$primary
  }

  get_key_duplicates_util(df, keys)
}

#' Creates a short information summary about the duplicate key values in a dataset.
#'
#' @md
#' @description `r lifecycle::badge("experimental")`
#'
#' @details
#' When the key argument is provided the function uses it to generate the summary, otherwise
#' looks for `primary_key` attribute of the `dataset`. If neither are provided raises an exception.
#'
#' @inheritParams get_key_duplicates
#' @param dataset \code{data.frame} object
#'
#' @return a \code{tibble} with a short information summary
#'
#' @examples
#' df <- as.data.frame(
#'   list(a = c('a', 'a', 'b', 'b', 'c'), b = c(1, 2, 3, 3, 4), c = c(1, 2, 3, 4, 5))
#' )
#' res <- get_key_duplicates(df, keys = c('a', 'b')) # duplicated keys are in rows 3 and 4
#' print(res) # outputs a tibble
#'
#' @seealso \link{get_key_duplicates} \link{get_key_duplicates_util}
#'
#' @export
get_key_duplicates.data.frame <- function(dataset, keys = NULL) { #nolint
  keys <- if ("keys" %in% class(keys)) {
    keys$primary
  } else if (!is.null(keys)) {
    keys
  } else if (!is.null(attr(dataset, "primary_key"))) {
    attr(dataset, "primary_key")
  }

  get_key_duplicates_util(dataset, keys)
}

#' Creates a duplicate keys information summary.
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @md
#' @details
#' Accepts a list of variable names - \code{keys}, which are treated as the
#' key to the \code{dataframe} argument. An instance of duplicated key is
#' defined as two rows, which have the same values in columns defined by \code{keys}.
#' Per each key value with duplicates returns a row in a \code{tibble}. The return table
#' has columns corresponding to the variable names passed in \code{keys} and
#' two additional columns: \code{row_no} and \code{duplicates_count}, which provide
#' information about row numbers of the original dataframe, which contain duplicated keys
#' and total duplicates counts.
#'
#' @param dataframe dataframe
#' @param keys \code{character} list of variable names consisting the key to the \code{dataframe}
#'
#' @return \code{tibble} with a duplicate keys information summary
#'
#' @importFrom dplyr mutate row_number select all_of summarise group_by across n
#' @importFrom rlang .data
#'
#' @examples
#' df <- as.data.frame(
#'   list(a = c("a", "a", "b", "b", "c"), b = c(1, 2, 3, 3, 4), c = c(1, 2, 3, 4, 5))
#' )
#' res <- teal:::get_key_duplicates_util(df, keys = c("a", "b")) # duplicated keys are in rows 3 and 4
#' print(res) # outputs a tibble
#'
#' @seealso \link{get_key_duplicates}
get_key_duplicates_util <- function(dataframe, keys) {
  stopifnot(!is.null(keys))
  stopifnot(is.data.frame(dataframe))
  stopifnot(is.character(keys))
  stopifnot(
    all(
      vapply(keys, function(key) key %in% colnames(dataframe), FUN.VALUE = logical(1))
    )
  )

  # The goal is to print values of duplicated primary keys with number of duplicates and row numbers
  # Seemed like adding a column with id numbers and pasting it once duplicates are subset was
  # the simplest course of action.
  summary <- mutate(dataframe, rows = row_number())
  summary <- summary[duplicated(
    select(summary, all_of(keys))) | duplicated(select(summary, all_of(keys)), fromLast = TRUE), ]
  summary <- summarise(
    group_by(summary, across(all_of(keys))),
    rows = paste0(.data[["rows"]], collapse = ","),
    n = n(),
    .groups = "drop")
  summary
}
