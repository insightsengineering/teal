#' Helper function to deep copy `R6` object
#'
#' When cloning an R6 object the private function
#' `deep_clone` is automatically used. To ensure a complete
#' clone the private function should call this function
#'
#' @param name (`character`) argument passed by `deep_clone` function.
#' @param value (any `R` object) argument passed by `deep_clone` function.
deep_clone_r6 <- function(name, value) {
  if (checkmate::test_list(value, types = "R6")) {
    lapply(value, function(x) x$clone(deep = TRUE))
  } else if (R6::is.R6(value)) {
    value$clone(deep = TRUE)
  } else if (is.environment(value)) {
    new_env <- as.environment(as.list(value, all.names = TRUE))
    parent.env(new_env) <- parent.env(value)
    new_env
  } else {
    value
  }
}

# also returns a list if only a single element
#' Split by separator
#'
#' @description `r lifecycle::badge("maturing")`
#'
#' @param x (`character`) Character (single)
#' @param sep (`character`) Separator
#' @export
split_by_sep <- function(x, sep) {
  stopifnot(is.atomic(x))
  if (is.character(x)) {
    strsplit(x, sep, fixed = TRUE)
  } else {
    x
  }
}

#' List element in other list
#'
#' @description `r lifecycle::badge("maturing")`
#' Checks if `x` element matches any of `y` element. If one of the arguments is a list then list elements
#' are treated as whole - in this case list elements can be a vector, so it looks
#' for equal element in second vector to be matched.
#'
#' @param x `list` to be matched.
#' @param y `list` to be matched against.
#' @return `logical` vector length of `x` denoting if element was found in second list.
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
#' @param choices (`list` or `vector`) select choices
#' @param values optional, choices subset for which labels should be extracted, `NULL` for all choices
#'
#' @return (`character`) vector with labels
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
#'
#' @param ns (`function`) namespace function passed from the `session` object in the
#'   Shiny server. For Shiny modules this will allow for proper name spacing of the
#'   registered input.
#'
#' @return (`Shiny`) input variable accessible with `input$tz` which is a (`character`)
#'  string containing the timezone of the browser/client.
#'
get_client_timezone <- function(ns) {
  script <- sprintf(
    "Shiny.setInputValue(`%s`, Intl.DateTimeFormat().resolvedOptions().timeZone)",
    ns("timezone")
  )
  shinyjs::runjs(script) # function does not return anything
  return(invisible(NULL))
}

#' Check if package can be loaded
#'
#' @param pckg `character` package name.
#' @param msg `character` error message to display if package is not available.
#'
#' @return Error or invisible NULL.
#'
check_pkg_quietly <- function(pckg, msg) {
  checkmate::assert_string(pckg)
  checkmate::assert_string(msg)
  if (!pckg %in% utils::installed.packages()) {
    stop(msg)
  }

  return(invisible(NULL))
}

#' Check that a given range is valid
#' @param subinterval (`numeric` or `date`)\cr
#'  vector of length 2 to be  compared against the full range.
#' @param range (`numeric` or `date`)\cr
#'  vector of length 2 containing the full range to validate against.
#' @param pre_msg `character` message to print before error for
#'   additional context.
#'
#' @return `NULL` if `subinterval` is a valid range or error with message
#'   otherwise.
#'
#' @examples
#' \dontrun{
#' check_in_range(c(3, 1), c(1, 3))
#' check_in_range(c(0, 3), c(1, 3))
#' check_in_range(
#'   c(as.Date("2020-01-01"), as.Date("2020-01-20")),
#'   c(as.Date("2020-01-01"), as.Date("2020-01-02"))
#' )
#' }
check_in_range <- function(subinterval, range, pre_msg = "") {
  epsilon <- .Machine$double.eps^0.5 # needed for floating point arithmetic; same value as in base::all.equal()
  if ((length(subinterval) != 2)) {
    stop(
      sprintf(
        "%s range length should be 2 while it is %s",
        pre_msg,
        length(subinterval)
      )
    )
  }
  if (subinterval[[2]] + epsilon < subinterval[[1]]) {
    stop(sprintf(
      "%s unexpected: the upper bound of the range lower than the lower bound \n %s < %s",
      pre_msg,
      subinterval[[2]],
      subinterval[[1]]
    ))
  }

  if ((subinterval[[1]] + epsilon < range[[1]]) || (subinterval[[2]] - epsilon > range[[2]])) {
    stop(
      sprintf(
        "%s range (%s) not valid for full range (%s)",
        pre_msg, toString(subinterval), toString(range)
      )
    )
  }
}


#' Check that one set is a subset of another
#'
#' Raises an error message if not and says which elements are not in
#' the allowed `choices`.
#'
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
  checkmate::assert_string(pre_msg)

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

teal_with_pkg <- function(pkg, code) {
  pkg_name <- paste0("package:", pkg)
  if (!pkg_name %in% search()) {
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
#'
#' @param code (`character`)\cr
#'   an R code to be evaluated or a `PythonCodeClass` created using [python_code].
#' @inheritParams dataset_connector
#' @return code (`character`)
code_from_script <- function(code, script, dataname = NULL) {
  checkmate::assert(
    checkmate::check_character(code, max.len = 1, any.missing = FALSE),
    checkmate::check_class(code, "PythonCodeClass")
  )
  checkmate::assert_character(script, max.len = 1, any.missing = FALSE)
  if (length(code) == 0 && length(script) == 0) {
    return(character(0))
  }

  if (checkmate::test_string(code) && checkmate::test_string(script)) {
    stop("Function doesn't accept 'code' and 'script' at the same time.
         Please specify either 'code' or 'script'", call. = FALSE)
  }

  if (checkmate::test_string(script)) {
    code <- read_script(file = script, dataname = dataname)
  }

  code
}

#' Read .R file into character
#'
#' @description `r lifecycle::badge("maturing")`
#' Comments will be excluded
#'
#' @param file (`character`) File to be parsed into code
#' @param dataname (`character`) dataset name to subset code from chunks
#' @return (`character`) vector with the code
#'
#' @export
#' @examples
#' file_example <- tempfile()
#' writeLines(c("x <- 2", "#second line comment", "x <- x + 2"), file_example)
#'
#' read_script(file_example)
read_script <- function(file, dataname = NULL) {
  checkmate::assert_string(file)
  stopifnot(file.exists(file))
  get_code_single(file, read_sources = TRUE) %>%
    enclosed_with_dataname(dataname = dataname) %>%
    code_exclude(exclude_comments = TRUE) %>%
    paste(sep = "\n", collapse = "\n")
}

#' S3 generic for creating an information summary about the duplicate key values in a dataset
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @details The information summary provides row numbers and number of duplicates
#' for each duplicated key value.
#'
#' @param dataset `TealDataset` or `data.frame` a dataset, which will be tested
#' @param keys `character` vector of variable names in `dataset` consisting the key
#' or `keys` object, which does have a `primary` element with a vector of variable
#' names in `dataset` consisting the key. Optional, default: NULL
#'
#' @return a `tibble` with variables consisting the key and `row_no` and `duplicates_count` columns
#'
#' @examples
#' library(scda)
#'
#' adsl <- synthetic_cdisc_data("latest")$adsl
#' # create a TealDataset with default keys
#' rel_adsl <- cdisc_dataset("ADSL", adsl)
#' get_key_duplicates(rel_adsl)
#'
#' df <- as.data.frame(
#'   list(a = c("a", "a", "b", "b", "c"), b = c(1, 2, 3, 3, 4), c = c(1, 2, 3, 4, 5))
#' )
#' res <- get_key_duplicates(df, keys = c("a", "b")) # duplicated keys are in rows 3 and 4
#' print(res) # prints a tibble
#' \dontrun{
#' get_key_duplicates(df) # raises an exception, because keys are missing with no default
#' }
#'
#' @seealso \itemize{
#' \item{[get_key_duplicates_util]}
#' \item{[get_key_duplicates.TealDataset]}
#' \item{[get_key_duplicates.data.frame]}
#' }
#'
#' @export
get_key_duplicates <- function(dataset, keys = NULL) {
  UseMethod("get_key_duplicates", dataset)
}

#' Creates a short information summary about the duplicate primary key values in a dataset
#'
#' @description `r lifecycle::badge("experimental")`
#' @details S3 method for get_key_duplicates. Uses the public API of
#' `TealDataset` to read the primary key and the raw data.
#'
#' If `keys` argument is provided, then checks against that, if it's `NULL`, then checks
#' against the `get_keys()$primary` method of the `dataset` argument.
#'
#' @inheritParams get_key_duplicates
#' @param dataset a `TealDataset` object, which will be used to detect duplicated
#' primary keys
#'
#' @examples
#' library(scda)
#'
#' adsl <- synthetic_cdisc_data("latest")$adsl
#' # Keys are automatically assigned, because the name
#' # is in the recognized ADAM nomenclature ("ADSL")
#' rel_adsl <- cdisc_dataset("ADSL", adsl)
#' get_key_duplicates(rel_adsl)
#' @seealso [get_key_duplicates] [get_key_duplicates_util]
#'
#' @export
get_key_duplicates.TealDataset <- function(dataset, keys = NULL) { # nolint
  df <- get_raw_data(dataset)
  if (is.null(keys)) {
    keys_ds <- get_keys(dataset)
    keys <- if (is.null(keys_ds)) character(0) else keys_ds
  }

  get_key_duplicates_util(df, keys)
}

#' Creates a short information summary about the duplicate key values in a dataset.
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @details
#' When the key argument is provided the function uses it to generate the summary, otherwise
#' looks for `primary_key` attribute of the `dataset`. If neither are provided raises an exception.
#'
#' @inheritParams get_key_duplicates
#' @param dataset `data.frame` object
#'
#' @return a `tibble` with a short information summary
#'
#' @examples
#' df <- as.data.frame(
#'   list(a = c("a", "a", "b", "b", "c"), b = c(1, 2, 3, 3, 4), c = c(1, 2, 3, 4, 5))
#' )
#' res <- get_key_duplicates(df, keys = c("a", "b")) # duplicated keys are in rows 3 and 4
#' print(res) # outputs a tibble
#' @seealso [get_key_duplicates] [get_key_duplicates_util]
#'
#' @export
get_key_duplicates.data.frame <- function(dataset, keys = NULL) { # nolint
  if (is.null(keys)) {
    attr_key <- attr(dataset, "primary_key")
    keys <- if (is.null(attr_key)) character(0) else attr
  }
  get_key_duplicates_util(dataset, keys)
}

#' Creates a duplicate keys information summary.
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @details
#' Accepts a list of variable names - `keys`, which are treated as the
#' key to the `data.frame` argument. An instance of duplicated key is
#' defined as two rows, which have the same values in columns defined by `keys`.
#' Per each key value with duplicates returns a row in a `tibble`. The return table
#' has columns corresponding to the variable names passed in `keys` and
#' two additional columns: `row_no` and `duplicates_count`, which provide
#' information about row numbers of the original dataframe, which contain duplicated keys
#' and total duplicates counts.
#'
#' @param dataframe dataframe
#' @param keys `character` list of variable names consisting the key to the `data.frame`
#'
#' @return `tibble` with a duplicate keys information summary
#'
#' @importFrom rlang .data
#'
#' @examples
#' df <- as.data.frame(
#'   list(a = c("a", "a", "b", "b", "c"), b = c(1, 2, 3, 3, 4), c = c(1, 2, 3, 4, 5))
#' )
#' res <- teal:::get_key_duplicates_util(df, keys = c("a", "b")) # duplicated keys are in rows 3 and 4
#' print(res) # outputs a tibble
#' @seealso [get_key_duplicates]
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
  summary <- dplyr::mutate(dataframe, rows = dplyr::row_number())
  summary <- summary[
    duplicated(dplyr::select(summary, dplyr::all_of(keys))) |
      duplicated(dplyr::select(summary, dplyr::all_of(keys)), fromLast = TRUE),
  ]
  summary <- dplyr::summarise(
    dplyr::group_by(summary, dplyr::across(dplyr::all_of(keys))),
    rows = paste0(.data$rows, collapse = ","),
    n = dplyr::n(),
    .groups = "drop"
  )
  summary
}

# Function to be used while trying to load the object of specific class from the script.
object_file <- function(path, class) {
  checkmate::assert_string(path)
  checkmate::assert_file_exists(path)
  checkmate::assert_string(class)

  lines <- paste0(readLines(path), collapse = "\n")
  object <- eval(parse(text = lines, keep.source = FALSE))

  stop_if_not(list(is(object, class), paste("The object returned from the file is not of", class, "class.")))

  return(object)
}

eval_expr_with_msg <- function(expr, env) {
  lapply(
    expr,
    function(x) {
      tryCatch(
        eval(x, envir = env),
        error = function(e) {
          stop(
            sprintf(
              "Call execution failed:\n - call:\n   %s\n - message:\n   %s ",
              deparse1(x, collapse = "\n"), e
            )
          )
        }
      )
    }
  )
}

#' Set state of `FilterState`
#'
#' @description
#' Set state of `FilterState`. Function can change states in [FilterState] in two ways, by:
#' - changing `reactive` state fields which triggers observers in the `FilterState`.
#' - change state directly.
#'
#' For more, please see section "Modifying state" in [FilterState]
#'
#' @inheritParams init_filter_state
#' @param value (named `list`)\cr
#'  see `set_state` method in [FilterState] and `filter` argument in the [teal::init()]
#' @param is_reactive (`logical(1)`)\cr
#'  - `TRUE` to change `reactive` fields which triggers observers in the `FilterState`
#'  - `FALSE` to change the state directly.
#' @return invisible `NULL`
#' @keywords internal
set_state <- function(x, value, is_reactive = shiny::isRunning()) {
  checkmate::assert_class(x, "FilterState")
  checkmate::assert_list(value)
  if (is_reactive) {
    x$set_state_reactive(value)
  } else {
    x$set_state(value)
  }
  invisible(NULL)
}
