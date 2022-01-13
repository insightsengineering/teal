#' Mutate dataset by code
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param x (`TealDataset`)\cr
#'    object.
#' @param dataname (`character`)\cr
#'   Dataname to be mutated.
#' @param code (`character`)\cr
#'   Code to mutate the dataset. Must contain the `dataset$dataname`. Or can also be an object
#'   of class `PythonCodeClass` returned by [`python_code`].
#' @param script (`character`)\cr
#'   file that contains R Code that can be read using [`read_script`].
#'   Preferred before `code` argument.
#' @param vars (named `list`)) \cr
#'   In case when this object code depends on other `TealDataset` object(s) or
#'   other constant value, this/these object(s) should be included as named
#'   element(s) of the list. For example if this object code needs `ADSL`
#'   object we should specify `vars = list(ADSL = <adsl object>)`.
#'   It's recommended to include `TealDataset` or `TealDatasetConnector` objects to
#'   the `vars` list to preserve reproducibility. Please note that `vars`
#'   are included to this object as local `vars` and they cannot be modified
#'   within another dataset.
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
#' library(scda)
#' library(magrittr)
#'
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#'
#' ADSL_dataset <- dataset(
#'   dataname = "ADSL",
#'   x = ADSL,
#'   label = "AdAM subject-level dataset",
#'   code = "ADSL <- synthetic_cdisc_data(\"latest\")$adsl"
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
#' @export
mutate_dataset.TealDataset <- function(x,
                                       code = character(0),
                                       script = character(0),
                                       vars = list(),
                                       ...) {
  check_ellipsis(...)
  checkmate::assert_list(vars, min.len = 0, names = "unique")

  code <- code_from_script(code, script)
  x$mutate(code = code, vars = vars, ...)
}


#' @rdname mutate_dataset
#' @export
mutate_dataset.TealDatasetConnector <- function(x, # nolint
                                                code = character(0),
                                                script = character(0),
                                                vars = list(),
                                                ...) {
  check_ellipsis(...)
  checkmate::assert_list(vars, min.len = 0, names = "unique")
  code <- code_from_script(code, script)
  x$mutate(code = code, vars = vars, ...)
}


#' @rdname mutate_dataset
#' @export
mutate_dataset.TealDataAbstract <- function(x,
                                            dataname,
                                            code = character(0),
                                            script = character(0),
                                            vars = list(),
                                            ...) {
  check_ellipsis(...)
  checkmate::assert_list(vars, min.len = 0, names = "unique")

  code <- code_from_script(code, script)
  x$mutate_dataset(dataname = dataname, code = code, vars = vars)
}



#' Mutate data by code
#'
#' @description `r lifecycle::badge("experimental")`
#' Code used in this mutation is not linked to particular
#' but refers to all datasets.
#' Consequence of this is that when using `get_code(<dataset>)` this
#' part of the code will be returned for each dataset specified. This method
#' should be used only if particular call involve changing multiple datasets.
#' Otherwise please use `mutate_dataset`.
#' Execution of the code is delayed after datasets are pulled
#' (`isTRUE(is_pulled)`).
#'
#' @param x (`TealDataAbstract`)\cr
#'   object.
#' @inheritParams mutate_dataset
#'
#' @return modified `x` object
#'
#' @export
mutate_data <- function(x,
                        code = character(0),
                        script = character(0),
                        vars = list()) {
  UseMethod("mutate_data")
}

#' @rdname mutate_data
#' @export
mutate_data.TealDataAbstract <- function(x,
                                         code = character(0),
                                         script = character(0),
                                         vars = list()) {
  checkmate::assert_list(vars, min.len = 0, names = "unique")

  code <- code_from_script(code, script)
  x$mutate(code = code, vars = vars)
  return(invisible(x))
}
