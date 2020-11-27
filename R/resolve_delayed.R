# resolve function and methods ------------

#' Resolve delayed inputs by evaluating the code within the provided datasets
#'
#' @description `r lifecycle::badge("maturing")`
#'
#' @md
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
#' ds <- teal:::FilteredData$new()
#' ADSL <- radsl(cached = TRUE)
#' attr(ADSL, "keys") <- get_cdisc_keys("ADSL")
#' isolate({
#'   ds$set_data("ADSL", ADSL)
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
#'     ARMCD = list(ref = variable_choices("ADSL"),
#'     comp = variable_choices("ADSL")
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
  if_null(x$key, x$key <- datasets$get_keys(x$data)$primary)
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
#' @importFrom methods is
resolve_delayed.delayed_choices_selected <- function(x, datasets) { # nolint
  if (!is.null(x$selected)) {
    x$selected <- if (is(x$selected, "delayed_data")) {
      resolve_delayed(x$selected, datasets = datasets)
    } else {
      x$selected
    }
  }
  x$choices <- resolve_delayed(x$choices, datasets = datasets)
  return(do.call("choices_selected", x))
}

#' @export
resolve_delayed.delayed_select_spec <- function(x, datasets) { # nolint
  x$choices <- resolve_delayed(x$choices, datasets = datasets)
  x$selected <- `if`(is(x$selected, "delayed_data"), resolve_delayed(x$selected, datasets = datasets), x$selected)
  return(do.call("select_spec", x))
}

#' @export
#' @importFrom methods is
resolve_delayed.delayed_filter_spec <- function(x, datasets) { # nolint
  x$selected <- `if`(is(x$selected, "delayed_data"), resolve_delayed(x$selected, datasets = datasets), x$selected)
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
  } else if (any(ulapply(x$filter, class) %in% "delayed_data")) {
    idx <- vapply(x$filter, inherits, logical(1), what = "delayed_data")
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
#' @importFrom methods is
resolve_delayed.default <- function(x, datasets) {
  return(x)
}

#' @importFrom methods is
resolve_teal_module <- function(x, datasets) {
  stopifnot(is(x, "teal_module"))
  stopifnot(is(datasets, "FilteredData"))

  x$server_args <- resolve_teal_args(x$server_args, datasets)
  return(x)
}

#' Handles teal arguments that are only available through delayed loading
#'
#' @md
#' @param args `list` arguments to evaluate by passing them the datasets
#' @param datasets `datasets` datasets used to set `args`
#' @importFrom methods is
resolve_teal_args <- function(args, datasets) {
  Map(function(arg) {
    if (is(arg, "delayed_data")) {
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


#' Resolve expression after delayed data are loaded
#'
#' @param x \code{function} Function that is applied on dataset.
#' It must take only a single argument "data" and return character vector with columns / values.
#' @param ds \code{data.frame} Dataset on which the function is applied to.
#' @param is_value_choices \code{logical} Determines which check of the returned value will be applied.
#'
#' @return Character vector - result of calling function \code{x} on dataset \code{ds}.
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
  stopifnot(
    is.function(x),
    is_fully_named_list(formals(x)),
    length(formals(x)) == 1,
    names(formals(x))[1] == "data"
  )

  # evaluate function
  res <- do.call("x", list(data = ds))

  # check returned value
  if (is_value_choices) {
    if (!is.atomic(res) || anyDuplicated(res)) {
      stop(paste("The following function must return a vector with unique values",
                 "from the respective columns of the dataset.\n\n",
                 pdeparse(bquote(.(x)))))
    }
  } else {
    if (!is_character_vector(res, min_length = 0L) || length(res) > ncol(ds) || anyDuplicated(res)) {
      stop(paste("The following function must return a character vector with unique",
                 "names from the available columns of the dataset:\n\n",
                 pdeparse(bquote(.(x)))))
    }
  }

  return(res)
}



# print methods --------

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
print.delayed_choices_selected <- function(x, indent = 0L, ...) {
  cat(indent_msg(indent, paste("choices_selected with delayed data: ", x$choices$data)))
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
