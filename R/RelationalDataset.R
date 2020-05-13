#' @title R6 Class representing a dataset including keys
#' @description
#' Any \code{data.frame} or \code{rtable} object can be
#' stored inside this object.
#'
#' It inherits the \link{NamedDataset} object. This needs the
#' code that was used to generate this object to be handed over.
#' Additionally it is possible to label the data set
#' by handing over a \code{label} argument to the
#' \code{new} method
#'
#' Additionally it contains the \code{keys} that allow
#' this dataset to be merged with other datasets
#' that are in any relation to this.
#'
#' @examples
#' rel_data <- RelationalDataset$new(
#'   x = data.frame(x = c(1, 2), y = c("a", "b"), stringsAsFactors = FALSE),
#'   keys = list(primary = "B"),
#'   name = "XY",
#'   code = "XY <- data.frame(x = c(1, 2), y = c('aa', 'bb'),
#'                            stringsAsFactors = FALSE)"
#' )
#'
#' rel_data$ncol
#' rel_data$get_code()
#' rel_data$get_name()
#' rel_data$get_keys()
#' rel_data$set_keys(list(primary = c("b")))
#'
#' @export
RelationalDataset <- R6::R6Class( # nolint
  "RelationalDataset",
  inherit = NamedDataset,
  ## NamedDataset ====
  ## __Public Methods ====
  public = list(
    #' @param x (\code{data.frame})
    #' @param name (\code{character}) A given name for the dataset
    #'   it may not contain spaces
    #' @param keys (\code{keys}) object of S3 class keys containing
    #'   foreign, primary keys and parent information
    #' @param code (\code{character}) A character string defining the code
    #'   needed to produce the data set in \code{x}
    #' @param label (\code{character}) Label to describe the dataset
    initialize = function(x, name, keys, code = character(0), label = character(0)) {
      super$initialize(x, name = name, code = code, label = label)

      self$set_keys(keys)
    },
    #' @description
    #' Derive the keys
    get_keys = function() {
      private$.keys
    },
    #' @description
    #' Set new keys
    #' @param keys (\code{keys}) Set the keys
    set_keys = function(keys) {
      stopifnot(is.list(keys))
      private$.keys <- keys
      invisible(NULL)
    }
  ),
  ## __Public Methods ====
  private = list(
    .keys = NULL
  ),
  ## __Active Fields ====
  active = list(
    #' @field keys (\code{keys}) Derive the keys of the dataset
    keys = function() {
      private$.keys
    }
  )
)
