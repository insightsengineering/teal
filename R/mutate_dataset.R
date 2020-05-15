#' Mutate dataset by code
#'
#' @param dataset (\code{NamedDataset}) object
#' @param code (\code{character}) Code to mutate the dataset. Must contain the
#'  \code{dataset$dataname}
#' @param script (\code{character}) file that contains R Code that can
#'   be read using \link{read_script}. Preferred before \code{code} argument
#'
#' @importFrom methods is
#' @export
#' @examples
#' library(random.cdisc.data)
#' library(magrittr)
#'
#' ADSL <- radsl(cached = TRUE)
#'
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
mutate_dataset <- function(dataset, code = character(0), script = character(0)) {
  stopifnot(is(dataset, "NamedDataset"))

  if (is_character_single(script)) {
    code <- read_script(file = script)
  }
  stopifnot(is_character_vector(code, min_length = 1, max_length = 1))
  if (!any(grepl(dataset$get_dataname(), code))) {
    stop("You did not use the dataname inside the code. It does not mutate the dataset")
  }

  execution_environment <- new.env()
  assign(envir = execution_environment, x = dataset$get_dataname(), value = dataset$get_raw_data())

  eval(parse(text = code), envir = execution_environment)

  if (!is.data.frame(execution_environment[[dataset$get_dataname()]])) {
    stop("Mutations need to lead to a data.frame again.")
  }
  if (is(dataset, "RelationalDataset")) {
    RelationalDataset$new(
      x = execution_environment[[dataset$get_dataname()]],
      dataname = dataset$get_dataname(),
      code = paste0(dataset$get_code(), "\n\n", code),
      label = dataset$get_dataset_label(),
      keys = dataset$get_keys()
    )
  } else {
    NamedDataset$new(
      x = execution_environment[[dataset$get_dataname()]],
      dataname = dataset$get_dataname(),
      code =  paste0(dataset$get_code(), "\n\n", code),
      label = dataset$get_dataset_label()
    )
  }
}
