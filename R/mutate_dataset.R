#' Mutate dataset by code
#'
#' @md
#' @description `r lifecycle::badge("experimental")`
#'
#' @param x (\code{NamedDataset})\cr
#'    object or \code{RelationalDataset} which inherited from it.
#' @param dataname (\code{character})\cr
#'   Dataname to be mutated.
#' @param code (\code{character})\cr
#'   Code to mutate the dataset. Must contain the \code{dataset$dataname}.
#' @param script (\code{character})\cr
#'   file that contains R Code that can be read using \link{read_script}.
#'   Preferred before \code{code} argument.
#' @param vars (list)\cr
#'   In case when this object code depends on the \code{raw_data} from the other
#'   \code{RelationalDataset}, \code{RelationalDatasetConnector} object(s) or other constant value,
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
  stopifnot(is_character_single(code) || is_character_single(script))
  stopifnot(is_fully_named_list(vars))

  code <- code_from_script(code, script)
  x$mutate(code = code, vars = vars)
}


#' @rdname mutate_dataset
#' @export
mutate_dataset.NamedDatasetConnector <- function(x, code = character(0), script = character(0), vars = list(), ...) { #nolint
  stopifnot(is_character_single(code) || is_character_single(script))
  stopifnot(is_fully_named_list(vars))

  code <- code_from_script(code, script)
  x$mutate(code = code, vars = vars)
}


#' @rdname mutate_dataset
#' @export
mutate_dataset.RelationalDataCollection <- function(x, dataname, code = character(0), script = character(0), vars = list(), ...) { #nolint
  stopifnot(is_character_single(code) || is_character_single(script))
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
  stopifnot(is_character_single(code) || is_character_single(script))
  UseMethod("mutate_data")
}

#' @rdname mutate_data
#' @export
mutate_data.RelationalDataCollection <- function(x, code = character(0), script = character(0), vars = list()) { #nolint
  code <- code_from_script(code, script) # nolint

  x$mutate(code = code, vars = vars)
  return(x)
}
