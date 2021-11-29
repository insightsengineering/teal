#' Teal data
#'
#' @description `r lifecycle::badge("experimental")`
#' Universal function to pass data to teal application
#'
#' @param ... (`TealDataConnector`, `TealDataset`, `TealDatasetConnector`)\cr
#'   objects
#' @param join_keys (`JoinKeys`) or a single (`JoinKeySet`)\cr
#'   (optional) object with dataset column relationships used for joining.
#'   If empty then no joins between pairs of objects
#' @param code (`character`) code to reproduce the datasets.
#' @param check (`logical`) reproducibility check - whether evaluated preprocessing code gives the same objects
#'   as provided in arguments. Check is run only if flag is true and preprocessing code is not empty.
#'
#' @return (`TealData`)
#'
#' @export
#'
#' @examples
#' x1 <- dataset(
#'   "x1",
#'   iris,
#'   code = "x1 <- iris"
#' )
#'
#' x2 <- dataset(
#'   "x2",
#'   mtcars,
#'   code = "x2 <- mtcars"
#' )
#'
#' teal_data(x1, x2)
teal_data <- function(...,
                      join_keys,
                      code = "",
                      check = FALSE) {
  data_objects <- list(...)

  is_cdisc <- vapply(
    X = data_objects,
    FUN.VALUE = logical(1),
    function(x) {
      is(x, "CDISCTealDataConnector") || is(x, "CDISCTealDatasetConnector") || is(x, "CDISCTealDataset")
    }
  )

  x <- if (any(is_cdisc)) {
    CDISCTealData$new(..., check = check, join_keys = join_keys)
  } else {
    TealData$new(..., check = check, join_keys = join_keys)
  }

  if (length(code) > 0 && !identical(code, "")) {
    x$set_pull_code(code = code)
  }

  x$check_reproducibility()
  x$check_metadata()

  return(x)
}


#' Load `TealData` object from a file
#'
#' @description `r lifecycle::badge("experimental")`
#' Please note that the script has to end with a call creating desired object. The error will be raised otherwise.
#'
#' @param path A (`connection`) or a (`character`)\cr
#'   string giving the pathname of the file or URL to read from. "" indicates the connection `stdin`.
#' @param code (`character`)\cr
#'   reproducible code to re-create object
#'
#' @return `TealData` object
#'
#'
#' @export
#'
#' @examples
#' # simple example
#' file_example <- tempfile(fileext = ".R")
#' writeLines(
#'   text = c(
#'     "library(teal)
#'
#'      x1 <- dataset(dataname = \"IRIS\",
#'                    x = iris,
#'                    code = \"IRIS <- iris\")
#'
#'      x2 <- dataset(dataname = \"MTCARS\",
#'                    x = mtcars,
#'                    code = \"MTCARS <- mtcars\")
#'
#'      teal_data(x1, x2)"
#'   ),
#'   con = file_example
#' )
#' teal_data_file(file_example, code = character(0))
teal_data_file <- function(path, code = get_code(path)) {
  object <- object_file(path, "TealData")
  object$mutate(code)
  return(object)
}
