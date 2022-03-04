#' Resolve delayed inputs by evaluating the code within the provided datasets
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @param x Object of class `delayed_data` to resolve.
#' @param datasets Object of class `FilteredData` to use for evaluation.
#'
#' @return Resolved object.
#'
#' @export
#'
#' @examples
#' library(scda)
#'
#' ds <- teal.slice:::FilteredData$new()
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#' attr(ADSL, "keys") <- teal.data::get_cdisc_keys("ADSL")
#' isolate({
#'   ds$set_dataset(teal.data::dataset("ADSL", ADSL))
#'
#'   # value_choices example
#'   v1 <- value_choices("ADSL", "SEX", "SEX")
#'   v1
#'   resolve_delayed(v1, ds)
#'
#'   # variable_choices example
#'   v2 <- variable_choices("ADSL", c("BMRKR1", "BMRKR2"))
#'   v2
#'   resolve_delayed(v2, ds)
#'
#'   # data_extract_spec example
#'   adsl_filter <- filter_spec(
#'     vars = variable_choices("ADSL", "SEX"),
#'     sep = "-",
#'     choices = value_choices("ADSL", "SEX", "SEX"),
#'     selected = "F",
#'     multiple = FALSE,
#'     label = "Choose endpoint and Censor"
#'   )
#'
#'   adsl_select <- select_spec(
#'     label = "Select variable:",
#'     choices = variable_choices("ADSL", c("BMRKR1", "BMRKR2")),
#'     selected = "BMRKR1",
#'     multiple = FALSE,
#'     fixed = FALSE
#'   )
#'
#'   adsl_de <- data_extract_spec(
#'     dataname = "ADSL",
#'     select = adsl_select,
#'     filter = adsl_filter
#'   )
#'
#'   resolve_delayed(adsl_filter, ds)
#'   resolve_delayed(adsl_select, ds)
#'   resolve_delayed(adsl_de, ds)
#'
#'   # nested list (arm_ref_comp)
#'   arm_ref_comp <- list(
#'     ARMCD = list(
#'       ref = variable_choices("ADSL"),
#'       comp = variable_choices("ADSL")
#'     )
#'   )
#'
#'   resolve_delayed(arm_ref_comp, ds)
#' })
resolve_delayed <- function(x, datasets) {
  stopifnot(is(datasets, "FilteredData"))
  UseMethod("resolve_delayed")
}

#' @export
resolve_delayed.delayed_variable_choices <- function(x, datasets) { # nolint
  if (is.null(x$key)) {
    x$key <- datasets$get_keys(x$data)
  }
  x$data <- datasets$get_data(x$data)
  if (is(x$subset, "function")) {
    x$subset <- resolve_delayed_expr(x$subset, ds = x$data, is_value_choices = FALSE)
  }
  return(do.call("variable_choices", x))
}

#' @export
resolve_delayed.delayed_value_choices <- function(x, datasets) { # nolint
  x$data <- datasets$get_data(x$data)
  if (is.function(x$subset)) {
    x$subset <- resolve_delayed_expr(x$subset, ds = x$data, is_value_choices = TRUE)
  }
  return(do.call("value_choices", x))
}

#' @export
resolve_delayed.delayed_choices_selected <- function(x, datasets) { # nolint
  if (is(x$selected, "delayed_data")) {
    x$selected <- resolve_delayed(x$selected, datasets = datasets)
  }
  x$choices <- resolve_delayed(x$choices, datasets = datasets)
  return(do.call("choices_selected", x))
}

#' @export
resolve_delayed.delayed_select_spec <- function(x, datasets) { # nolint
  x$choices <- resolve_delayed(x$choices, datasets = datasets)
  if (is(x$selected, "delayed_data")) {
    x$selected <- resolve_delayed(x$selected, datasets = datasets)
  }
  return(do.call("select_spec", x))
}

#' @export
resolve_delayed.delayed_filter_spec <- function(x, datasets) { # nolint
  if (is(x$vars_choices, "delayed_data")) {
    x$vars_choices <- resolve_delayed(x$vars_choices, datasets = datasets)
  }
  if (is(x$vars_selected, "delayed_data")) {
    x$vars_selected <- resolve_delayed(x$vars_selected, datasets = datasets)
  }
  if (is(x$choices, "delayed_data")) {
    x$choices <- resolve_delayed(x$choices, datasets = datasets)
  }
  if (is(x$selected, "delayed_data")) {
    x$selected <- resolve_delayed(x$selected, datasets = datasets)
  }

  return(do.call("filter_spec_internal", x[intersect(names(x), methods::formalArgs(filter_spec_internal))]))
}

#' @export
resolve_delayed.delayed_data_extract_spec <- function(x, datasets) { # nolint
  x$select <- `if`(is(x$select, "delayed_data"), resolve_delayed(x$select, datasets = datasets), x$select)

  if (any(vapply(x$filter, is, logical(1L), "delayed_data"))) {
    idx <- vapply(x$filter, is, logical(1), "delayed_data")
    x$filter[idx] <- lapply(x$filter[idx], resolve_delayed, datasets = datasets)
  }

  return(do.call("data_extract_spec", x))
}

#' @export
resolve_delayed.list <- function(x, datasets) { # nolint

  # If specified explicitly, return it unchanged. Otherwise if delayed, resolve.
  res <- lapply(x, resolve_delayed, datasets)
  return(res)
}

#' @export
resolve_delayed.default <- function(x, datasets) {
  return(x)
}

#' Resolve expression after delayed data are loaded
#'
#'
#' @param x (`function`) Function that is applied on dataset.
#' It must take only a single argument "data" and return character vector with columns / values.
#' @param ds (`data.frame`) `TealDataset` on which the function is applied to.
#' @param is_value_choices (`logical`) Determines which check of the returned value will be applied.
#'
#' @return Character vector - result of calling function `x` on dataset `ds`.
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # get only possible factor variables from mtcars dataset
#' resolve_delayed_expr(
#'   function(data) {
#'     idx <- vapply(data, function(x) is.numeric(x) && length(unique(x)) <= 6, logical(1))
#'     colnames(data)[idx]
#'   },
#'   ds = mtcars,
#'   is_value_choices = FALSE
#' )
#' }
resolve_delayed_expr <- function(x, ds, is_value_choices) {
  checkmate::assert_function(x, args = "data", nargs = 1)

  # evaluate function
  res <- do.call("x", list(data = ds))

  # check returned value
  if (is_value_choices) {
    if (!is.atomic(res) || anyDuplicated(res)) {
      stop(paste(
        "The following function must return a vector with unique values",
        "from the respective columns of the dataset.\n\n",
        deparse1(bquote(.(x)), collapse = "\n")
      ))
    }
  } else {
    if (!checkmate::test_character(res, any.missing = FALSE) || length(res) > ncol(ds) || anyDuplicated(res)) {
      stop(paste(
        "The following function must return a character vector with unique",
        "names from the available columns of the dataset:\n\n",
        deparse1(bquote(.(x)), collapse = "\n")
      ))
    }
  }

  return(res)
}

#' @keywords internal
#' @export
print.delayed_variable_choices <- function(x, indent = 0L, ...) {
  cat(indent_msg(indent, paste("variable_choices with delayed data:", x$data)))
  cat("\n")
  print_delayed_list(x, indent)
  return(invisible(NULL))
}

#' @keywords internal
#' @export
print.delayed_value_choices <- function(x, indent = 0L, ...) {
  cat(indent_msg(indent, paste("value_choices with delayed data: ", x$data)))
  cat("\n")
  print_delayed_list(x, indent)
  return(invisible(NULL))
}

#' @keywords internal
#' @export
print.delayed_choices_selected <- function(x, indent = 0L, ...) {
  cat(indent_msg(indent, paste("choices_selected with delayed data: ", x$choices$data)))
  cat("\n")
  print_delayed_list(x, indent)
  return(invisible(NULL))
}

#' @keywords internal
#' @export
print.delayed_select_spec <- function(x, indent = 0L, ...) {
  cat(indent_msg(indent, paste("select_spec with delayed data:", x$choices$data)))
  cat("\n")
  print_delayed_list(x, indent)
  return(invisible(NULL))
}

#' @keywords internal
#' @export
print.filter_spec <- function(x, indent = 0L, ...) {
  cat(indent_msg(indent, "filter_spec with delayed data:"))
  cat("\n")
  print_delayed_list(x, indent)
  return(invisible(NULL))
}

#' @keywords internal
#' @export
print.delayed_filter_spec <- function(x, indent = 0L, ...) {
  cat(indent_msg(indent, "filter_spec with delayed data:"))
  cat("\n")
  print_delayed_list(x, indent)
  return(invisible(NULL))
}

#' @keywords internal
#' @export
print.delayed_data_extract_spec <- function(x, indent = 0L, ...) {
  cat(paste("data_extract_spec with delayed data:", x$dataname))
  cat("\n\n")
  print_delayed_list(x)
  return(invisible(NULL))
}

indent_msg <- function(n, msg) {
  checkmate::assert_integer(n, len = 1, lower = 0, any.missing = FALSE)
  checkmate::assert_character(msg, min.len = 1, any.missing = FALSE)
  indent <- paste(rep("  ", n), collapse = "")
  return(paste0(indent, msg))
}

print_delayed_list <- function(obj, n = 0L) {
  checkmate::assert_integer(n, len = 1, lower = 0, any.missing = FALSE)
  stopifnot(is.list(obj))

  for (idx in seq_along(obj)) {
    cat(indent_msg(n, ifelse(is.null(names(obj)[[idx]]), paste0("[[", idx, "]]"), paste("$", names(obj)[[idx]]))))
    cat("\n")
    if (is(obj[[idx]], "delayed_data")) {
      print(obj[[idx]], n + 1L)
    } else if (is.list(obj[[idx]])) {
      print_delayed_list(obj[[idx]], n + 1L)
    } else {
      cat(indent_msg(n, paste(utils::capture.output(print(obj[[idx]])), collapse = "\n")))
      cat("\n")
    }
  }
  return(invisible(NULL))
}
