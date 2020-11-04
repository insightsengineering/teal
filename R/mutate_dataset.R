#' Mutate dataset by code
#'
#' @md
#' @description `r lifecycle::badge("experimental")`
#'
#' @param x (`NamedDataset`)\cr
#'    object or `RelationalDataset` which inherited from it.
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
#'   `RelationalDataset`, `RelationalDatasetConnector` object(s) or other constant value,
#'   this/these object(s) should be included.
#' @param ... not used, only for support of S3
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
#' ADSL_dataset <- named_dataset(
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
mutate_dataset.NamedDataset <- function(x, code = character(0), script = character(0), vars = list(), ...) { #nolint
  stopifnot(is_fully_named_list(vars))

  code <- code_from_script(code, script)
  x$mutate(code = code, vars = vars)
}


#' @rdname mutate_dataset
#' @export
mutate_dataset.NamedDatasetConnector <- function(x, code = character(0), script = character(0), vars = list(), ...) { #nolint
  stopifnot(is_fully_named_list(vars))

  code <- code_from_script(code, script)
  x$mutate(code = code, vars = vars)
}


#' @rdname mutate_dataset
#' @export
mutate_dataset.RelationalDataCollection <- function(x, dataname, code = character(0), script = character(0), vars = list(), ...) { #nolint
  stopifnot(is_fully_named_list(vars))

  code <- code_from_script(code, script)
  x$mutate_dataset(dataname = dataname, code = code, vars = vars)
}



#' Mutate data by code
#'
#' @md
#' @description `r lifecycle::badge("experimental")`
#'
#' @param x (\code{RelationalData} or \code{RelationalDataConnector})\cr
#'   object.
#' @inheritParams mutate_dataset
#'
#' @export
mutate_data <- function(x, code = character(0), script = character(0), vars = list()) {
  UseMethod("mutate_data")
}

#' @rdname mutate_data
#' @export
mutate_data.RelationalDataCollection <- function(x, code = character(0), script = character(0), vars = list()) { #nolint
  stopifnot(is_fully_named_list(vars))

  code <- code_from_script(code, script)
  x$mutate(code = code, vars = vars)
  return(x)
}
