# RelationalData ------
#' @title \code{RelationalData} class
#' @description
#' Class combines multiple \code{RelationalDataset} objects.
#'
#' @importFrom R6 R6Class
#'
#' @examples
#' x <- teal:::RelationalDataset$new(
#'   x = data.frame(x = c(1, 2), y = c("a", "b"), stringsAsFactors = FALSE),
#'   keys = keys(primary = "y", foreign = NULL, parent = NULL),
#'   dataname = "XY",
#'   code = "XY <- data.frame(x = c(1, 2), y = c('aa', 'bb'),
#'                            stringsAsFactors = FALSE)"
#' )
#'
#' x2 <- teal:::RelationalDataset$new(
#'   x = data.frame(x = c(1, 2), y = c("a", "b"), stringsAsFactors = FALSE),
#'   keys = keys(primary = "y", foreign = NULL, parent = NULL),
#'   dataname = "XYZ",
#'   code = "XYZ <- data.frame(x = c(1, 2), y = c('aa', 'bb'),
#'                            stringsAsFactors = FALSE)"
#' )
#'
#' rd <- teal:::RelationalData$new(x, x2)
RelationalData <- R6::R6Class( #nolint
  classname = "RelationalData",
  public = list(
    #' @description
    #' Create a new \code{RelationalData} object from multiple
    #' \code{RelationalDataset} objects.
    #'
    #' @param ... (\code{RelationalDataset})\cr
    #'  at least one object.
    #'
    #' @return new \code{RelationalData} object
    initialize = function(...) {
      datasets <- list(...)

      is_teal_data <- is_any_class_list(datasets, "RelationalDataset")
      if (!all(is_teal_data)) {
        stop("All data elements should be RelationalDataset")
      }

      dataset_names <- vapply(datasets, get_dataname, character(1))
      if (any(duplicated(dataset_names))) {
        stop("Dataset names should be unique")
      }

      names(datasets) <- dataset_names
      private$datasets <- datasets

      return(invisible(self))
    },
    #' @description
    #' Get \code{cdisc_data} object from multiple \code{RelationalDataset} objects.
    #'
    #' @return \code{cdisc_data} object.
    get_cdisc_data = function() {
      if (is.null(private$cdisc_code)) {
        do.call("cdisc_data", private$datasets)
      } else {
        do.call("cdisc_data", c(private$datasets, code = private$cdisc_code))
      }
    },
    #' @description
    #' Get names of the datasets.
    #'
    #' @return \code{character} vector with names of all datasets.
    get_datanames = function() {
      vapply(private$datasets, get_dataname, character(1))
    },
    #' @description
    #'
    #' Get code for all datasets.
    #'
    #' @return (\code{character}) vector of code to generate datasets.
    get_code = function() {
      vapply(private$datasets, get_code, character(1))
    },
    #' @description
    #' Get \code{RelationalDataset} objects.
    #' @param dataname (\code{character} value)\cr
    #'   name of dataset to be returned. If \code{NULL}, all datasets are returned.
    #'
    #' @return \code{RelationalDataset} for single \code{dataname} or
    #'   \code{list} of \code{RelationalDataset} if \code{dataname = NULL}.
    get_datasets = function(dataname = NULL) {
      stopifnot(is.null(dataname) || is_character_single(dataname))

      if (is.null(dataname)) {
        private$datasets
      } else {
        private$datasets[[dataname]]
      }
    }
  ),
  # ..private ------
  private = list(
    # .... fields: ------
    # .... ... ------
    datasets = NULL
  )
)
