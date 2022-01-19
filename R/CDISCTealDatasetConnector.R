## CDISCTealDatasetConnector ====
#'
#' @title A `CDISCTealDatasetConnector` class of objects
#'
#' Objects of this class store the connection function to fetch a single dataset.
#'
#' The difference compared to the inherited class is a parent field that
#' indicates name of the parent dataset. Note that the parent field might
#' be empty (i.e. `character(0)`).
#'
#' @param dataname (`character`)\cr
#'  A given name for the dataset it may not contain spaces
#'
#' @param pull_callable (`CallableFunction`)\cr
#'   function with necessary arguments set to fetch data from connection.
#'
#' @param keys (`character`)\cr
#'  vector of dataset primary keys column names
#'
#' @param parent optional, (`character`) \cr
#'   parent dataset name
#'
#' @param label (`character`)\cr
#'   Label to describe the dataset.
#'
#' @param code (`character`)\cr
#'  A character string defining code to modify `raw_data` from this dataset. To modify
#'  current dataset code should contain at least one assignment to object defined in `dataname`
#'  argument. For example if `dataname = ADSL` example code should contain
#'  `ADSL <- <some R code>`. Can't be used simultaneously with `script`
#'
#' @param script (`character`)\cr
#'   Alternatively to `code` - location of the file containing modification code.
#'   Can't be used simultaneously with `script`.
#'
#' @param vars (named `list`)) \cr
#'   In case when this object code depends on other `TealDataset` object(s) or
#'   other constant value, this/these object(s) should be included as named
#'   element(s) of the list. For example if this object code needs `ADSL`
#'   object we should specify `vars = list(ADSL = <adsl object>)`.
#'   It's recommended to include `TealDataset` or `TealDatasetConnector` objects to
#'   the `vars` list to preserve reproducibility. Please note that `vars`
#'   are included to this object as local `vars` and they cannot be modified
#'   within another dataset.
CDISCTealDatasetConnector <- R6::R6Class( # nolint
  classname = "CDISCTealDatasetConnector",
  inherit = TealDatasetConnector,

  ## __Public Methods ====
  public = list(
    #' @description
    #' Create a new `TealDatasetConnector` object. Set the pulling function
    #' `CallableFunction` which returns a `data.frame`, e.g. by reading
    #' from a function or creating it on the fly.
    initialize = function(dataname,
                          pull_callable,
                          keys, parent,
                          code = character(0),
                          label = character(0),
                          vars = list()) {
      super$initialize(
        dataname = dataname,
        pull_callable = pull_callable,
        keys = keys,
        code = code,
        label = label,
        vars = vars
      )
      private$set_parent(parent)
      logger::log_trace("CDISCTealDatasetConnector initialized for dataset: { deparse1(self$get_dataname()) }")

      return(invisible(self))
    },
    #' @description
    #' Get parent dataset name
    #' @return (`character`) indicating parent dataname
    get_parent = function() {
      private$parent
    },

    #' @description
    #' Pull the data
    #'
    #' Read or create the data using `pull_callable` specified in the constructor.
    #'
    #' @param args (`NULL` or named `list`)\cr
    #'  additional dynamic arguments for pull function. `args` can be omitted if `pull_callable`
    #'  from constructor already contains all necessary arguments to pull data. One can try
    #'  to execute `pull_callable` directly by `x$pull_callable$run()` or to get code using
    #'  `x$pull_callable$get_code()`. `args` specified in pull are used temporary to get data but
    #'  not saved in code.
    #' @param try (`logical` value)\cr
    #'  whether perform function evaluation inside `try` clause
    #'
    #' @return `self` invisibly for chaining.
    pull = function(args = NULL, try = FALSE) {
      logger::log_trace("CDISCTealDatasetConnector$pull pulling dataset: { deparse1(self$get_dataname()) }.")
      super$pull(args = args, try = try)

      if (!self$is_failed()) {
        private$dataset <- as_cdisc(
          private$dataset,
          parent = self$get_parent()
        )
        logger::log_trace("CDISCTealDatasetConnector$pull pulled dataset: { deparse1(self$get_dataname()) }.")
      } else {
        logger::log_error("CDISCTealDatasetConnector$pull failed to pull dataset: { deparse1(self$get_dataname()) }.")
      }
      return(invisible(self))
    }
  ),

  ## __Private Fields ====
  private = list(
    parent = character(0),

    ## __Private Methods ====
    set_parent = function(parent) {
      checkmate::assert_character(parent, max.len = 1, any.missing = FALSE)
      private$parent <- parent
      return(invisible(self))
    }
  )
)
