# RelationalData ------
#' @title \code{RelationalData} class
#' @description
#' Class combines multiple \code{RelationalDataset} objects.
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
#'   dataname = "XY",
#'   code = "XY <- data.frame(x = c(1, 2), y = c('aa', 'bb'),
#'                            stringsAsFactors = FALSE)"
#' )
#'
#' rd <- teal:::RelationalData$new(x, x2)
#' @importFrom R6 R6Class
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

      # sort elements by class name
      private$datasets <- datasets

      return(invisible(self))
    },
    #' @description
    #' Get \code{cdisc_data} object from multiple \code{RelationalDataset} objects.
    #'
    #' @return \code{cdisc_data} object.
    get_cdisc_data = function() {
      do.call("cdisc_data", private$datasets)
    },
    #' @description
    #' Get names of the datasets.
    #'
    #' @return \code{character} vector with names of all datasets.
    get_datanames = function() {
      vapply(private$datasets, get_dataname.RelationalData, character(1))
    },
    #' @description
    #' Get \code{RelationalDataset} objects.
    #'
    #' @return list of \code{RelationalDataset}.
    get_datasets = function() {
      private$datasets
    }
  ),
  # ..private ------
  private = list(
    # .... fields: ------
    # .... ... ------
    datasets = NULL
  )
)
