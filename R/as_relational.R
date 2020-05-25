#' Convert a \code{Raw<...>} to a \code{Relational<...>}
#'
#' @param x (\code{RawDataset} or \code{RawDatasetConnector}) object
#'
#' @param dataname (\code{character})\cr
#'   A given name for the dataset it may not contain spaces
#'
#' @param keys (\code{keys})\cr
#'   object of S3 class keys containing foreign, primary keys and parent information
#'
#' @param code (\code{character})\cr
#'   A character string defining the code needed to produce the data set in \code{x}
#'
#' @param script (\code{character})\cr
#'   file that contains R Code that can be read using \link{read_script}.
#'   Preferred before \code{code} argument
#'
#' @param label (\code{character})\cr
#'   Label to describe the dataset
#'
#' @return \code{RelationalDataset} or \code{RelationalDatasetConnector} object
#'
#' @rdname as_relational
#' @export
#' @importFrom methods is
as_relational <- function(x,
                          dataname,
                          keys,
                          code,
                          script,
                          label) {
  UseMethod("as_relational")
}


#' @rdname as_relational
#' @examples
#' library(random.cdisc.data)
#' ADSL <- radsl(cached = TRUE)
#' ADSL_raw <- raw_dataset(x = ADSL)
#'
#' ADSL_relational <- as_relational(ADSL_raw,
#'   dataname = "ADSL",
#'   keys = keys(primary = c("USUBJID", "STUDYID"), foreign = NULL, parent = NULL)
#' )
#' get_raw_data(ADSL_relational)
#' ADSL_relational$keys
#' @export
as_relational.RawDataset <- function(x,
                                     dataname,
                                     keys,
                                     code = character(0),
                                     script = character(0),
                                     label = character(0)) { #nolint
  code <- code_from_script(code, script) # nolint

  return(
    RelationalDataset$new(
        x = x$get_raw_data(),
        dataname = dataname,
        keys = keys,
        code = code,
        label = label
    )
  )
}

#' @rdname as_relational
#' @export
as_relational.NamedDataset <- function(x,
                                       dataname,
                                       keys,
                                       code = character(0),
                                       script = character(0),
                                       label = character(0)) { #nolint
  warning("Only raw_data of 'x' will be used. All other fields get lost by using 'as_relational'.")
  code <- code_from_script(code, script) # nolint

  return(
    RelationalDataset$new(
      x = x$get_raw_data(),
      dataname = dataname,
      keys = keys,
      code = code,
      label = label
    )
  )
}

#' @rdname as_relational
#' @examples
#' library(random.cdisc.data)
#' fun <- callable_function(radsl)
#' fun$set_args(list(N = 5, seed = 1, cached = TRUE))
#' x <- raw_dataset_connector(fun)
#'
#' x2 <- as_relational(x, dataname = "ADSL", keys = get_cdisc_keys("ADSL"))
#' @importFrom utils.nest is_character_empty
#' @export
as_relational.RawDatasetConnector <- function(x,  #nolint
                                              dataname,
                                              keys,
                                              code = character(0),
                                              script = character(0),
                                              label = character(0)) {
  code <- code_from_script(code, script) # nolint
  ds <- tryCatch(expr = get_dataset(x),
                 error = function(e) NULL)
  if (!is.null(ds)) {
    warning(
      "Pulled 'dataset' from 'x' will not be passed to RelationalDatasetConnector.
      Avoid pulling before conversion."
    )
  }
  return(
    relational_dataset_connector(
      pull_fun = x$get_pull_fun(),
      dataname = dataname,
      keys = keys,
      code = code,
      label = label
    )
  )
}


#' @rdname as_relational
#'
#' @details
#' \code{as_cdisc_relational} will derive the keys by the \code{dataname} and therefore
#'   does not need the \code{keys} argument to be specified
#'
#' @export
as_cdisc_relational <- function(x,
                                dataname,
                                code = character(0),
                                script = character(0),
                                label = character(0)) {
  code <- code_from_script(code, script) # nolint

  return(
    as_relational(
      x = x,
      dataname = dataname,
      keys = get_cdisc_keys(dataname),
      code = code,
      label = label
    )
  )
}

#' Get code from script
#'
#' Get code from script. Switches between \code{code} and \code{script arguments}
#' to return non-empty one to pass it further to constructors.
#' @inheritParams as_relational
#' @return code (\code{character})
code_from_script <- function(code, script) {
  stopifnot(is_character_vector(code, min_length = 0, max_length = 1))
  stopifnot(is_character_vector(script, min_length = 0, max_length = 1))
  if (length(code) == 0 && length(script) == 0) {
    return(character(0))
  }

  if (is_character_single(code) && is_character_single(script)) {
    stop("Function doesn't accept 'code' and 'script' at the same time.
         Please specify either 'code' or 'script'", call. = FALSE)
  }

  if (is_character_single(script)) {
    code <- read_script(file = script) # nolint
  }

  return(code)
}

#' Evaluate script code to modify data
#'
#' Evaluate script code to modify data
#' @inheritParams as_relational
#' @return (\code{environment}) which stores modified \code{x}
execute_script_code <- function(x, code) {
  stopifnot(is_character_vector(code, min_length = 0, max_length = 1))
  stopifnot(is(x, "NamedDataset"))

  execution_environment <- new.env()
  assign(envir = execution_environment, x = x$get_dataname(), value = x$get_raw_data())

  eval(parse(text = code), envir = execution_environment)

  if (!is.data.frame(execution_environment[[x$get_dataname()]])) {
    stop("Mutations need to lead to a data.frame again.")
  }

  return(execution_environment)
}
