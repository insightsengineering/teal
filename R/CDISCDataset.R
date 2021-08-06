## CDISCDataset ====
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @title R6 Class representing a dataset with parent attribute
#'
#' @description
#' Any \code{data.frame} object can be stored inside this object.
#'
#' The difference compared to the inherited class is a parent field that
#' indicates name of the parent dataset. Note that the parent field might
#' be empty (i.e. `character(0)`).
#'
#' @param dataname (`character`)\cr
#'  A given name for the dataset it may not contain spaces
#'
#' @param x (`data.frame`)\cr
#'
#' @param keys (`character`)\cr
#'   vector with primary keys
#'
#' @param parent optional, (`character`) \cr
#'   parent dataset name
#'
#' @param code (`character`)\cr
#'   A character string defining the code needed to produce the data set in \code{x}
#'
#' @param label (`character`)\cr
#'   Label to describe the dataset
#'
#' @param vars (named `list`)) \cr
#'   In case when this object code depends on other `Dataset` object(s) or
#'   other constant value, this/these object(s) should be included as named
#'   element(s) of the list. For example if this object code needs `ADSL`
#'   object we should specify `vars = list(ADSL = <adsl object>)`.
#'   It's recommended to include `Dataset` or `DatasetConnector` objects to
#'   the `vars` list to preserve reproducibility. Please note that `vars`
#'   are included to this object as local `vars` and they cannot be modified
#'   within another dataset.
#'
#' @examples
#' x <- cdisc_dataset(
#'   dataname = "XYZ",
#'   x = data.frame(x = c(1, 2), y = c("a", "b"), stringsAsFactors = FALSE),
#'   keys = "y",
#'   parent = "ABC",
#'   code = "XYZ <- data.frame(x = c(1, 2), y = c('aa', 'bb'),
#'                             stringsAsFactors = FALSE)"
#' )
#'
#' x$ncol
#' x$get_code()
#' x$get_dataname()
#' x$get_keys()
#' x$get_parent()
CDISCDataset <- R6::R6Class( # nolint
  "CDISCDataset",
  inherit = Dataset,
  ## __Public Methods ====
  public = list(
    #' @description
    #' Create a new object of `CDISCDataset` class
    initialize = function(dataname, x, keys, parent, code = character(0), label = character(0), vars = list()) {
      stopifnot(is_character_empty(parent) || is_character_single(parent))
      super$initialize(dataname = dataname, x = x, keys = keys, code = code, label = label, vars = vars)

      self$set_parent(parent)
      return(invisible(self))
    },
    #' @description
    #' Recreate a dataset with its current attributes
    #' This is useful way to have access to class initialize method basing on class object
    #'
    #' @return a new object of `CDISCDataset` class
    recreate = function(dataname = self$get_dataname(),
                        x = self$get_raw_data(),
                        keys = self$get_keys(),
                        parent = self$get_parent(),
                        code = self$get_code_class(),
                        label = self$get_dataset_label(),
                        vars = list()) {
      res <- self$initialize(
        dataname = dataname,
        x = x,
        keys = keys,
        parent = parent,
        code = code,
        label = label,
        vars = vars
      )

      return(res)
    },
    #' @description
    #' Get all dataset attributes
    #' @return (named `list`) with dataset attributes
    get_attrs = function() {
      x <- super$get_attrs()
      x <- append(
        x,
        list(
          parent = self$get_parent()
        )
      )
      return(x)
    },
    #' @description
    #' Get parent dataset name
    #' @return (`character`) indicating parent dataname
    get_parent = function() {
      return(private$parent)
    },
    #' @description
    #' Set parent dataset name
    #' @param parent (`character`) indicating parent dataname
    #' @return (`self`) invisibly for chaining
    set_parent = function(parent) {
      stopifnot(is_character_empty(parent) || is_character_single(parent))
      private$parent <- parent
      return(invisible(self))
    }
  ),
  ## __Private Fields ====
  private = list(
    parent = character(0)
  )
)

# constructors ====
#' Create a new object of `CDISCDataset` class
#'
#' @description `r lifecycle::badge("experimental")`
#' Function that creates `CDISCDataset` object
#'
#' @inheritParams dataset
#' @param parent optional, (`character`) \cr
#'   parent dataset name
#'
#' @return (`CDISCDataset`) a dataset with connected metadata
#'
#' @export
#'
#' @examples
#' library(scda)
#'
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#'
#' cdisc_dataset("ADSL", ADSL)
cdisc_dataset <- function(dataname,
                          x,
                          keys = get_cdisc_keys(dataname),
                          parent = `if`(identical(dataname, "ADSL"), character(0), "ADSL"),
                          label = data_label(x),
                          code = character(0),
                          vars = list()) {
  CDISCDataset$new(
    dataname = dataname,
    x = x,
    keys = keys,
    parent = parent,
    label = label,
    code = code,
    vars = vars
  )
}

#' Load \code{CDISCDataset} object from a file
#'
#' @description `r lifecycle::badge("experimental")`
#' Please note that the script has to end with a call creating desired object. The error will be raised otherwise.
#'
#' @inheritParams dataset_file
#'
#' @return (`CDISCDataset`) object
#'
#' @export
#'
#' @examples
#' # simple example
#' file_example <- tempfile(fileext = ".R")
#' writeLines(
#'   text = c(
#'     "library(teal)
#'      library(scda)
#'
#'      cdisc_dataset(dataname = \"ADSL\",
#'                    x = synthetic_cdisc_data('latest')$adsl,
#'                    code = \"library(scda)\nADSL <- synthetic_cdisc_data('latest')$adsl\")"
#'   ),
#'   con = file_example
#' )
#' x <- cdisc_dataset_file(file_example, code = character(0))
#' get_code(x)
cdisc_dataset_file <- function(path, code = get_code(path)) {
  object <- object_file(path, "CDISCDataset")
  object$set_code(code)
  return(object)
}
