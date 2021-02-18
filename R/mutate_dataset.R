#' Mutate dataset by code
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param x (`Dataset`)\cr
#'    object.
#' @param dataname (`character`)\cr
#'   Dataname to be mutated.
#' @param code (`character`)\cr
#'   Code to mutate the dataset. Must contain the `dataset$dataname`. Or can also be an object
#'   of class `PythonCodeClass` returned by [`python_code`].
#' @param script (`character`)\cr
#'   file that contains R Code that can be read using [`read_script`].
#'   Preferred before `code` argument.
#' @param vars (list)\cr
#'   In case when this object code depends on the `raw_data` from the other
#'   `Dataset`, `DatasetConnector` object(s) or other constant value,
#'   this/these object(s) should be included.
#' @param keys optional, (`character`)\cr
#'   vector with primary keys
#' @param ... not used, only for support of S3
#'
#' @return modified `x` object
#'
#' @export
mutate_dataset <- function(x, ...) {
  UseMethod("mutate_dataset")
}

#' @rdname mutate_dataset
#' @examples
#' library(random.cdisc.data)
#' library(magrittr)
#'
#' ADSL <- radsl(cached = TRUE)
#'
#' ADSL_dataset <- dataset(
#'   dataname = "ADSL",
#'   x = ADSL,
#'   label = "AdAM subject-level dataset",
#'   code = "ADSL <- radsl(cached = TRUE)"
#' )
#' ADSL_mutated <- ADSL_dataset %>%
#'   mutate_dataset(code = "ADSL$new_variable <- 1")
#'
#' ADSL_mutated$get_raw_data()$new_variable[1]
#'
#' # Use an R script to mutate the data
#' file_example <- tempfile(fileext = ".R")
#' writeLines(
#'   text = c(
#'     "ADSL <- ADSL %>%
#'       dplyr::mutate(new_variable = new_variable * 2)"
#'   ),
#'   con = file_example
#' )
#'
#' ADSL_mutated <- ADSL_mutated %>%
#'   mutate_dataset(script = file_example)
#'
#' ADSL_mutated$get_raw_data()$new_variable[1]
#'
#' ADSL_mutated <- ADSL_mutated %>%
#'   mutate_dataset(code = read_script(file_example))
#'
#' ADSL_mutated$get_raw_data()$new_variable[1]
#'
#' @export
mutate_dataset.Dataset <- function(x,
                                   code = character(0),
                                   script = character(0),
                                   vars = list(),
                                   keys = get_keys(x),
                                   ...) {
  check_ellipsis(...)
  stopifnot(is_fully_named_list(vars))

  code <- code_from_script(code, script)
  x$mutate(code = code, vars = vars, keys = keys, ...)
}


#' @rdname mutate_dataset
#' @export
mutate_dataset.DatasetConnector <- function(x, # nolint
                                            code = character(0),
                                            script = character(0),
                                            vars = list(),
                                            keys = get_keys(x),
                                            ...) {
  check_ellipsis(...)
  stopifnot(is_fully_named_list(vars))

  code <- code_from_script(code, script)
  x$mutate(code = code, vars = vars, keys = keys, ...)
}


#' @rdname mutate_dataset
#' @export
mutate_dataset.DataAbstract <- function(x,
                                        dataname,
                                        code = character(0),
                                        script = character(0),
                                        vars = list(),
                                        keys = get_keys(x$get_items(dataname)),
                                        ...) {
  check_ellipsis(...)
  stopifnot(is_fully_named_list(vars))

  code <- code_from_script(code, script)
  x$mutate_dataset(dataname = dataname, code = code, vars = vars)
  set_keys(x, dataname, keys)
}



#' Mutate data by code
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param x (\code{DataAbstract})\cr
#'   object.
#' @inheritParams mutate_dataset
#'
#' @return modified `x` object
#'
#' @export
mutate_data <- function(x,
                        code = character(0),
                        script = character(0),
                        vars = list(),
                        keys = list()) {
  UseMethod("mutate_data")
}

#' @rdname mutate_data
#' @export
mutate_data.DataAbstract <- function(x,
                                     code = character(0),
                                     script = character(0),
                                     vars = list(),
                                     keys = list()) {
  stopifnot(is_fully_named_list(vars))
  stopifnot(identical(keys, list()) || (is_character_list(keys, min_length = 0) && is_fully_named_list(keys)))

  code <- code_from_script(code, script)
  x$mutate(code = code, vars = vars)
  if (!is_empty(keys)) {
    for (key_idx in seq_along(keys)) {
      key_dataname <- names(keys)[[key_idx]]
      key_val <- keys[[key_idx]]
      set_keys(x, key_dataname, key_val)
    }
  }
  return(invisible(x))
}
