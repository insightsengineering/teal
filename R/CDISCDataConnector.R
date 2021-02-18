# CDISCDataConnector ------
#' @title Manage multiple and \code{DatasetConnector} of the same type.
#' @description
#' Class manages \code{DatasetConnector} to specify additional dynamic arguments and to
#' open/close connection.
#'
#' @param connection (\code{DataConnection})\cr
#'   connection to data source
#' @param connectors (\code{list} of \code{DatasetConnector} elements)\cr
#'   list with dataset connectors
#'
#' @importFrom R6 R6Class
#' @importFrom methods is
CDISCDataConnector <- R6::R6Class( #nolint
  classname = "CDISCDataConnector",
  inherit = RelationalDataConnector,

  ## __Public Methods ====
  public = list(
    #' @description
    #' Create a new `CDISCDataConnector` object
    initialize = function(connection, connectors) {
      super$initialize(connection = connection, connectors = connectors)

      new_parent <- list()
      for (x in connectors) {
        x_dataname <- x$get_dataname()
        new_parent[[x_dataname]] <- if (is(x, "CDISCDatasetConnector")) {
          x$get_parent()
        } else {
          character(0L)
        }
      }

      if (is_dag(new_parent)) {
        stop("Cycle detected in a parent and child dataset graph.")
      }

      private$parent <- new_parent

      return(invisible(self))
    },
    #' @description
    #' Get all datasets parent names
    #' @return (named `list`) with dataset name and its corresponding parent dataset name
    get_parent = function() {
      private$parent
    }
  ),

  ## __Private Fields ====
  private = list(
    parent = list() # list with dataset names and its parent dataset names
  )
)
