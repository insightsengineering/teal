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
resolve_delayed.delayed_filter_spec <- function(x, datasets) { # nolint
  x$choices <- `if`(is(x$choices, "delayed_data"), resolve_delayed(x$choices, datasets = datasets), x$choices)
  x$vars <- `if`(is(x$vars, "delayed_data"), resolve_delayed(x$vars, datasets = datasets), x$vars)
  return(do.call("filter_spec", x))
}

#' @export
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
resolve_delayed.default <- function(x, datasets) {
  stop(paste('No "resolve_delayed" method implemented for object of class',
             paste(class(x), collapse = ", ")))
}


print.delayed_variable_choices <- function(x, ...) {
  cat(paste("variable_choices with delayed data:", x$data))
  return(invisible(NULL))
}

print.delayed_value_choices <- function(x, ...) {
  cat(paste("value_choices with delayed data:", x$data))
  return(invisible(NULL))
}

print.delayed_select_spec <- function(x, ...) {
  cat(paste("select_spec with delayed data:", x$choices$data))
  return(invisible(NULL))
}

print.delayed_filter_spec <- function(x, ...) {
  dd <- c(
    `if`(is(x$choices, "delayed_data"), x$choices$data, NULL),
    `if`(is(x$vars, "delayed_data"), x$vars$data, NULL)
  )
  cat(paste("filter_spec with delayed data:", unique(dd)))
  return(invisible(NULL))
}

print.delayed_data_extract_spec <- function(x, ...) {
  cat(paste("data_extract_spec with delayed data:", x$dataname))
  return(invisible(NULL))
}
