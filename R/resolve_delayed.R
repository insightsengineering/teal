#' Resolve delayed inputs by evaluating the code within the provided datasets
#'
#' @param x Object of class \code{delayed_data} to resolve.
#' @param datasets Object of class \code{FilteredData} to use for evaluation.
#'
#' @return Resolved object.
#'
#' @importFrom methods is
#' @export
#'
#' @examples
#' library(random.cdisc.data)
#'
#' x <- teal:::FilteredData$new()
#' x$set_data("ADSL", radsl(cached = TRUE))
#'
#' # value_choices example
#' v1 <- value_choices("ADSL", "SEX", "SEX")
#' v1
#' resolve_delayed(v1, x)
#'
#' # variable_choices example
#' v2 <- variable_choices("ADSL", c("BMRKR1", "BMRKR2"))
#' v2
#' resolve_delayed(v2, x)
#'
#' # data_extract_spec example
#' adsl_filter <- filter_spec(
#'   vars = variable_choices("ADSL", "SEX"),
#'   sep = "-",
#'   choices = value_choices("ADSL", "SEX", "SEX"),
#'   selected = "F",
#'   multiple = FALSE,
#'   label = "Choose endpoint and Censor"
#' )
#'
#' adsl_select <- select_spec(
#'   label = "Select variable:",
#'   choices = variable_choices("ADSL", c("BMRKR1", "BMRKR2")),
#'   selected = "BMRKR1",
#'   multiple = FALSE,
#'   fixed = FALSE
#' )
#'
#' adsl_de <- data_extract_spec(
#'   dataname = "ADSL",
#'   select = adsl_select,
#'   filter = adsl_filter
#' )
#'
#' resolve_delayed(adsl_filter, x)
#' resolve_delayed(adsl_select, x)
#' resolve_delayed(adsl_de, x)
resolve_delayed <- function(x, datasets) {
  stopifnot(is(datasets, "FilteredData"))
  UseMethod("resolve_delayed")
}

#' @export
resolve_delayed.delayed_variable_choices <- function(x, datasets) { # nolint
  x$data <- datasets$get_data(x$data)
  return(do.call("variable_choices", x))
}

#' @export
resolve_delayed.delayed_value_choices <- function(x, datasets) { # nolint
  x$data <- datasets$get_data(x$data)
  return(do.call("value_choices", x))
}

#' @export
resolve_delayed.delayed_select_spec <- function(x, datasets) { # nolint
  x$choices <- resolve_delayed(x$choices, datasets = datasets)
  return(do.call("select_spec", x))
}

#' @export
#' @importFrom methods is
resolve_delayed.delayed_filter_spec <- function(x, datasets) { # nolint
  x$choices <- `if`(is(x$choices, "delayed_data"), resolve_delayed(x$choices, datasets = datasets), x$choices)
  x$vars <- `if`(is(x$vars, "delayed_data"), resolve_delayed(x$vars, datasets = datasets), x$vars)
  return(do.call("filter_spec", x))
}

#' @export
#' @importFrom methods is
resolve_delayed.delayed_data_extract_spec <- function(x, datasets) { # nolint
  x$select <- `if`(is(x$select, "delayed_data"), resolve_delayed(x$select, datasets = datasets), x$select)

  if (is(x$filter, "delayed_data")) {
    x$filter <- resolve_delayed(x$filter, datasets)
  } else if (any(unlist(lapply(x$filter, class)) %in% "delayed_data")) {
    idx <- vapply(x$filter, inherits, logical(1), what = "delayed_data")
    x$filter[idx] <- lapply(x$filter[idx], resolve_delayed, datasets = datasets)
  }

  return(do.call("data_extract_spec", x))
}

#' @export
#' @importFrom methods is
resolve_delayed.default <- function(x, datasets) {
  stop(paste('No "resolve_delayed" method implemented for object of class',
             paste(class(x), collapse = ", ")))
}

#' @importFrom methods is
resolve_teal_module <- function(x, datasets) {
  stopifnot(is(x, "teal_module"))
  stopifnot(is(datasets, "FilteredData"))

  x$server_args <- resolve_teal_args(x$server_args, datasets)
  return(x)
}

#' @importFrom methods is
resolve_teal_args <- function(args, datasets) {
  Map(function(arg) {
    if (identical(arg, "teal_datasets")) {
      datasets
    } else if (is(arg, "delayed_data")) {
      resolve_delayed(arg, datasets)
    } else if (is.list(arg) && any(vapply(arg, is, logical(1), "delayed_data"))) {
      idx <- vapply(arg, is, logical(1), "delayed_data")
      arg[idx] <- lapply(arg[idx], resolve_delayed, datasets = datasets)
      arg
    } else {
      arg
    }
  }, args)
}


# print methods

#' @export
print.delayed_variable_choices <- function(x, indent = 0L, ...) {
  cat(indent_msg(indent, paste("variable_choices with delayed data:", x$data)))
  cat("\n")
  print_delayed_list(x, indent)
  return(invisible(NULL))
}

#' @export
print.delayed_value_choices <- function(x, indent = 0L, ...) {
  cat(indent_msg(indent, paste("value_choices with delayed data: ", x$data)))
  cat("\n")
  print_delayed_list(x, indent)
  return(invisible(NULL))
}

#' @export
print.delayed_select_spec <- function(x, indent = 0L, ...) {
  cat(indent_msg(indent, paste("select_spec with delayed data:", x$choices$data)))
  cat("\n")
  print_delayed_list(x, indent)
  return(invisible(NULL))
}

#' @export
#' @importFrom methods is
print.delayed_filter_spec <- function(x, indent = 0L, ...) {
  cat(indent_msg(indent, paste("filter_spec with delayed data:", x$choices$data)))
  cat("\n")
  print_delayed_list(x, indent)
  return(invisible(NULL))
}

#' @export
#' @importFrom methods is
print.delayed_data_extract_spec <- function(x, indent = 0L, ...) {
  cat(paste("data_extract_spec with delayed data:", x$dataname))
  cat("\n\n")
  print_delayed_list(x)
  return(invisible(NULL))
}

indent_msg <- function(n, msg) {
  stopifnot(is_integer_single(n) || n == 0)
  stopifnot(is_character_vector(msg))
  indent <- paste(rep("  ", n), collapse = "")
  return(paste0(indent, msg))
}

print_delayed_list <- function(obj, n = 0L) {
  stopifnot(is_integer_single(n))
  stopifnot(is.list(obj))

  for (idx in seq_along(obj)) {
    cat(indent_msg(n, ifelse(is.null(names(obj)[[idx]]), paste0("[[", idx, "]]"), paste("$", names(obj)[[idx]]))))
    cat("\n")
    if (is(obj[[idx]], "delayed_data")) {
      print(obj[[idx]], n + 1L)
    } else if (is.list(obj[[idx]])) {
      print_delayed_list(obj[[idx]], n + 1L)
    } else {
      cat(indent_msg(n, paste(capture.output(print(obj[[idx]])), collapse = "\n")))
      cat("\n")
    }
  }
  return(invisible(NULL))
}
