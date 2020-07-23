#' Mutate dataset by code
#'
#' @param x (\code{NamedDataset})\cr
#'   object
#' @param code (\code{character})\cr
#'   Code to mutate the dataset. Must contain the \code{dataset$dataname}
#' @param script (\code{character})\cr
#'   file that contains R Code that can be read using \link{read_script}.
#'   Preferred before \code{code} argument
#' @param vars (list)\cr
#'   In case when this object code depends on the \code{raw_data} from the other
#'   \code{RelationalDataset}, \code{RelationalDatasetConnector} object(s) or other constant value,
#'   this/these object(s) should be included
#'
#' @rdname mutate_dataset
#' @export
mutate_dataset <- function(x, code = character(0), script = character(0), vars = list()) {
  stopifnot(is_character_single(code) || is_character_single(script))
  stopifnot(is_fully_named_list(vars))
  UseMethod("mutate_dataset")
}

#' @rdname mutate_dataset
#' @examples
#' library(random.cdisc.data)
#' library(magrittr)
#'
#' ADSL <- radsl(cached = TRUE)
#'
#' ADSL_dataset <- named_dataset(dataname = "ADSL",
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
#'     "library(dplyr)",
#'     "ADSL <- ADSL %>%
#'       mutate(new_variable = new_variable * 2)"
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
#' @importFrom methods is
#' @export
mutate_dataset.NamedDataset <- function(x, code = character(0), script = character(0), vars = list()) { #nolint
  code <- code_from_script(code, script) # nolint
  if (!any(grepl(x$get_dataname(), code))) {
    stop("You did not use the dataname inside the code. It does not mutate the 'x'")
  }

  execution_environment <- execute_script_code(x, code, vars) # nolint

  NamedDataset$new(
    x = execution_environment[[x$get_dataname()]],
    dataname = x$get_dataname(),
    code = paste0(c(x$get_code(), get_code_vars(vars), code), collapse = "\n"),
    label = x$get_dataset_label()
  )
}

#' @rdname mutate_dataset
#' @export
mutate_dataset.RelationalDataset <- function(x, code = character(0), script = character(0), vars = list()) { #nolint
  code <- code_from_script(code, script) # nolint
  if (!any(grepl(x$get_dataname(), code))) {
    stop("You did not use the dataname inside the code. It does not mutate the 'x'")
  }

  execution_environment <- execute_script_code(x, code, vars) # nolint

  RelationalDataset$new(
    x = execution_environment[[x$get_dataname()]],
    dataname = x$get_dataname(),
    code = paste0(c(x$get_code(), get_code_vars(vars), code), collapse = "\n"),
    label = x$get_dataset_label(),
    keys = x$get_keys()
  )
}

#' @rdname mutate_dataset
#' @export
mutate_dataset.NamedDatasetConnector <- function(x, code = character(0), script = character(0), vars = list()) { #nolint
  code <- code_from_script(code, script)
  if (!any(grepl(x$get_dataname(), code))) {
    stop("You did not use the dataname inside the code. It does not mutate the 'x'")
  }

  x$mutate(code = code, vars = vars)
}




#' Mutate dataset by code
#'
#' @param x (\code{RelationalData})\cr
#'   object
#' @inheritParams mutate_dataset
#'
#' @rdname mutate_data
#' @export
mutate_data <- function(x, code = character(0), script = character(0), vars = list()) {
  stopifnot(is_character_single(code) || is_character_single(script))
  UseMethod("mutate_data")
}

#' @rdname mutate_data
#' @export
mutate_data.RelationalData <- function(x, code = character(0), script = character(0), vars = list()) { #nolint
  code <- code_from_script(code, script) # nolint

  x$set_code(code = paste0(c(get_code_vars(vars), code), collapse = "\n"))
  return(x)
}
