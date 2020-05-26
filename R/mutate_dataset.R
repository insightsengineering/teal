#' Mutate dataset by code
#'
#' @param x (\code{NamedDataset})\cr
#'   object
#' @param code (\code{character})\cr
#'   Code to mutate the dataset. Must contain the \code{dataset$dataname}
#' @param script (\code{character})\cr
#'   file that contains R Code that can be read using \link{read_script}.
#'   Preferred before \code{code} argument
#'
#' @rdname mutate_dataset
#' @export
mutate_dataset <- function(x, code = character(0), script = character(0)) {
  stopifnot(is_character_single(code) || is_character_single(script))
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
mutate_dataset.NamedDataset <- function(x, code = character(0), script = character(0)) { #nolint
  code <- code_from_script(code, script) # nolint
  if (!any(grepl(x$get_dataname(), code))) {
    stop("You did not use the dataname inside the code. It does not mutate the 'x'")
  }

  execution_environment <- execute_script_code(x, code) # nolint

  NamedDataset$new(
    x = execution_environment[[x$get_dataname()]],
    dataname = x$get_dataname(),
    code =  paste0(x$get_code(), "\n\n", code),
    label = x$get_dataset_label()
  )
}

#' @rdname mutate_dataset
#' @export
mutate_dataset.RelationalDataset <- function(x, code = character(0), script = character(0)) { #nolint
  code <- code_from_script(code, script) # nolint
  if (!any(grepl(x$get_dataname(), code))) {
    stop("You did not use the dataname inside the code. It does not mutate the 'x'")
  }

  execution_environment <- execute_script_code(x, code) # nolint

  RelationalDataset$new(
    x = execution_environment[[x$get_dataname()]],
    dataname = x$get_dataname(),
    code = paste0(x$get_code(), "\n\n", code),
    label = x$get_dataset_label(),
    keys = x$get_keys()
  )
}

#' @rdname mutate_dataset
#' @export
mutate_dataset.RelationalDatasetConnector <- function(x, code = character(0), script = character(0)) { #nolint
  code <- code_from_script(code, script) # nolint
  if (!any(grepl(x$get_dataname(), code))) {
    stop("You did not use the dataname inside the code. It does not mutate the 'x'")
  }

  x$mutate_dataset(code = code)
}
