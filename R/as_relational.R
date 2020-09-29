#' Convert a \code{Raw<...>} to a \code{Relational<...>}
#'
#' @md
#' @description `r lifecycle::badge("experimental")`
#' Convert a \code{Raw<...>} to a \code{Relational<...>}
#'
#' @inheritParams relational_dataset_connector
#' @param x (\code{RawDataset}, \code{NamedDataset}, \code{RawDatasetConnector})
#'
#' @return \code{RelationalDataset} or \code{RelationalDatasetConnector} object
#'
#' @export
as_relational <- function(x,
                          dataname,
                          keys,
                          label) {
  UseMethod("as_relational")
}


#' @rdname as_relational
#' @examples
#' library(random.cdisc.data)
#' ADSL <- radsl(cached = TRUE)
#' ADSL_raw <- raw_dataset(x = ADSL)
#'
#' ADSL_relational <- as_relational(
#'   ADSL_raw,
#'   dataname = "ADSL",
#'   keys = keys(primary = c("USUBJID", "STUDYID"), foreign = NULL, parent = NULL)
#' )
#' get_raw_data(ADSL_relational)
#' ADSL_relational$keys
#' @export
as_relational.RawDataset <- function(x,
                                     dataname,
                                     keys,
                                     label = character(0)) {
  return(
    relational_dataset(
      x = get_raw_data(x),
      dataname = dataname,
      keys = keys,
      label = label
    )
  )
}

#' @rdname as_relational
#' @export
as_relational.NamedDataset <- function(x,
                                       dataname,
                                       keys,
                                       label = character(0)) {
  warning("Only raw_data of 'x' will be used. All other fields get lost by using 'as_relational'.")

  return(
    relational_dataset(
      dataname = dataname,
      x = get_raw_data(x),
      keys = keys,
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
#' @export
as_relational.RawDatasetConnector <- function(x, # nolint
                                              dataname,
                                              keys,
                                              label = character(0)) {
  ds <- tryCatch(
    expr = get_dataset(x),
    error = function(e) NULL
  )
  if (!is.null(ds)) {
    warning(
      "Pulled 'dataset' from 'x' will not be passed to RelationalDatasetConnector.
      Avoid pulling before conversion."
    )
  }
  return(
    relational_dataset_connector(
      dataname = dataname,
      pull_callable = x$get_pull_callable(),
      keys = keys,
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
                                label = character(0)) {
  return(
    as_relational(
      x = x,
      dataname = dataname,
      keys = get_cdisc_keys(dataname),
      label = label
    )
  )
}

#' Get code from script
#'
#' Get code from script. Switches between \code{code} and \code{script arguments}
#' to return non-empty one to pass it further to constructors.
#' @param code (\code{character} value)\cr
#'   an R code to be evaluated.
#' @inheritParams relational_dataset_connector
#' @return code (\code{character})
code_from_script <- function(code, script, dataname = NULL) {
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
    code <- read_script(file = script, dataname = dataname)
  }

  return(code)
}
