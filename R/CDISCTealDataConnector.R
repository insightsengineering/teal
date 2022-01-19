# CDISCTealDataConnector ------
#'
#' @title Manage multiple and `TealDatasetConnector` of the same type.
#'
#' @description `r lifecycle::badge("stable")`
#' Class manages `TealDatasetConnector` to specify additional dynamic arguments and to
#' open/close connection.
#'
#' @param connection (`TealDataConnection`)\cr
#'   connection to data source
#' @param connectors (`list` of `TealDatasetConnector` elements)\cr
#'   list with dataset connectors
#'
CDISCTealDataConnector <- R6::R6Class( # nolint
  classname = "CDISCTealDataConnector",
  inherit = TealDataConnector,

  ## __Public Methods ====
  public = list(
    #' @description
    #' Create a new `CDISCTealDataConnector` object
    initialize = function(connection, connectors) {
      super$initialize(connection = connection, connectors = connectors)

      new_parent <- list()
      for (x in connectors) {
        x_dataname <- x$get_dataname()
        new_parent[[x_dataname]] <- if (is(x, "CDISCTealDatasetConnector")) {
          x$get_parent()
        } else {
          character(0L)
        }
      }

      if (is_dag(new_parent)) {
        stop("Cycle detected in a parent and child dataset graph.")
      }

      private$parent <- new_parent
      logger::log_trace(
        "CDISCTealDataConnector initialized with data: { paste(self$get_datanames(), collapse = ' ') }"
      )
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

#' The constructor of `CDISCTealDataConnector` objects.
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @param connection (`TealDataConnection`)\cr
#'   connection to data source
#' @param connectors (`list` of `TealDatasetConnector` elements)\cr
#'   list with dataset connectors
#'
#' @examples
#' \dontrun{
#' adsl_cf <- CallableFunction$new(
#'   function() as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADSL"))))
#' )
#' adae_cf <- CallableFunction$new(
#'   function() as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADAE"))))
#' )
#' adsl <- CDISCTealDatasetConnector$new(
#'   "ADSL", adsl_cf,
#'   keys = get_cdisc_keys("ADSL"), parent = character(0)
#' )
#' adae <- CDISCTealDatasetConnector$new(
#'   "ADAE", adae_cf,
#'   keys = get_cdisc_keys("ADAE"), parent = "ADSL"
#' )
#' data <- cdisc_data_connector(
#'   connection = data_connection(open_fun = CallableFunction$new(function() "open function")),
#'   connectors = list(adsl, adae)
#' )
#' }
#' @return `CDISCTealDataConnector` object
#' @export
cdisc_data_connector <- function(connection, connectors) {
  stopifnot(is(connection, "TealDataConnection"))
  checkmate::assert_list(connectors, types = "TealDatasetConnector", min.len = 1)
  CDISCTealDataConnector$new(connection, connectors)
}
