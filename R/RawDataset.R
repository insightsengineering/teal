## RawDataset ====
#' @title  R6 Class representing a raw data set
#' @description
#' Any \code{data.frame} or \code{rtable} object can be
#' stored inside this object. Some attributes like colnames,
#' dimension or column names for a specific type will
#' be automatically derived.
#'
#' @importFrom R6 R6Class
RawDataset <- R6::R6Class( # nolint
  "RawDataset",

  ## __Public Methods ====
  public = list(
    #' @importFrom rtables var_labels
    #'
    #' @description
    #' initialize a \code{RawDataset} class object
    #' @param x (\code{data.frame}) object
    #' @return \code{RawDataset} object
    initialize = function(x) {
      stopifnot(is.data.frame(x))

      private$.raw_data <- x
      private$.ncol <- ncol(x)
      private$.nrow <- nrow(x)
      private$.dim <- c(private$.nrow, private$.ncol)
      private$.colnames <- colnames(x)
      private$.rownames <- rownames(x)
      private$.col_labels <- rtables::var_labels(x)
      row_labels <- c() # not yet defined in rtables
      return(invisible(self))
    },
    #' @description
    #' Derive the raw data frame inside this object
    #' @return
    #' \code{data.frame} or \code{rtable}
    get_raw_data = function() {
      private$.raw_data
    },
    #' @description
    #' Derive the names of all \code{numeric} columns
    #' @param include_factors \code{logical} Whether to include
    #'   factor variables
    #' @return \code{character} vector.
    get_numeric_colnames = function(include_factors = FALSE) {
      private$get_class_colnames("numeric", include_factors = FALSE)
    },
    #' @description
    #' Derive the names of all \code{character} columns
    #' @return \code{character} vector.
    get_character_colnames = function() {
      private$get_class_colnames("character", include_factors = FALSE)
    },
    #' @description
    #' Derive the names of all \code{factor} columns
    #' @return \code{character} vector.
    get_factor_colnames = function() {
      private$get_class_colnames("factor")
    },
    #' @description
    #' Derive the column names
    #' @return \code{character} vector.
    get_colnames = function() {
      private$.colnames
    },
    #' @description
    #' Derive the column labels
    #' @return \code{character} vector.
    get_column_labels = function() {
      private$.col_labels
    },
    #' @description
    #' Derive the row names
    #' @return \code{character} vector.
    get_rownames = function() {
      private$.rownames
    },
    #' @description
    #' Derive the row labels
    #' @return \code{character} vector.
    get_row_labels = function() {
      private$.row_labels
    },
    #' @description
    #' Check if dataset has already been pulled.
    #'
    #' @return \code{TRUE} if dataset has been already pulled, else \code{FALSE}
    is_pulled = function() {
      return(TRUE)
    }

  ),
  ## __Private Fields ====
  private = list(
    .ncol = 0L,
    .nrow = 0L,
    .dim = c(0L, 0L),
    .raw_data = data.frame(),
    .rownames = character(),
    .colnames = character(),
    .col_labels = character(),
    .row_labels = character(),

    ## __Private Methods ====
    #' @import utils.nest
    get_class_colnames = function(class_type = "character", include_factors = FALSE) {
      stopifnot(utils.nest::is_character_single(class_type))

      if (class_type == "factor") {
        include_factors <- TRUE
      }
      return_cols <- private$.colnames[which(vapply(
        lapply(private$.raw_data, class),
        function(x, target_class_name) any(x %in% target_class_name),
        logical(1),
        target_class_name = class_type))]

      if (!include_factors) {
        factor_columns <- private$.colnames[which(vapply(
          lapply(private$.raw_data, class),
          function(x) any(x %in% "factor"),
          logical(1)))]
        return_cols <- setdiff(return_cols, factor_columns)
      }
      return(return_cols)
    }
  ),
  ## __Active Fields ====
  active = list(
    #' @field ncol Number of columns
    ncol = function() {
      private$.ncol
    },
    #' @field nrow Number of rows
    nrow = function() {
      private$.nrow
    },
    #' @field dim Dimension \code{c(x, y)}
    dim = function() {
      private$.dim
    },
    #' @field colnames The column names of the data
    colnames = function() {
      private$.colnames
    },
    #' @field rownames The rownames of the data
    rownames = function() {
      private$.rownames
    },
    #' @field raw_data The data.frame behind this R6 class
    raw_data = function() {
      private$.raw_data
    },
    #' @field data The data.frame behind this R6 class
    data = function() {
      private$.raw_data
    },
    #' @field column_labels for backwards compatibility
    #' will be deprecated in future
    column_labels = function() {
      warning("'column_labels'will be deprecated in future. Use 'get_column_labels()' instead.")
      private$.col_labels
    },
    #' @field var_names The column names of the data
    var_names = function() {
      private$.colnames
    },
    #' @field row_labels Row labels (can have spaces)
    row_labels = function() {
      private$.row_labels
    }
  )
)

## Constructors ====

#' Constructor for \link{RawDataset} object
#'
#' @md
#' @description `r lifecycle::badge("experimental")`
#'
#' @param x (\code{data.frame} or \code{rtable}) object
#'
#' @examples
#' library(random.cdisc.data)
#' ADSL <- radsl(cached = TRUE)
#' ADSL_dataset <- raw_dataset(x = ADSL)
#'
#' ADSL_dataset$get_raw_data()
#'
#' ADSL_dataset$colnames
#'
#' @export
raw_dataset <- function(x) {
  stopifnot(is.data.frame(x))
  RawDataset$new(x)
}
