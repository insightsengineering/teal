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
#' rel_data <- teal:::RelationalDataset$new(
#'   x = data.frame(x = c(1, 2), y = c("a", "b"), stringsAsFactors = FALSE),
#'   keys = keys(primary = "y", foreign = NULL, parent = NULL),
#'   dataname = "XY",
#'   code = "XY <- data.frame(x = c(1, 2), y = c('aa', 'bb'),
#'                            stringsAsFactors = FALSE)"
#' )
#'
#' rel_data$ncol
#' rel_data$get_code()
#' rel_data$get_dataname()
#' rel_data$get_keys()
#' rel_data$set_keys(keys(primary = "x", foreign = NULL, parent = NULL))
#' @importFrom R6 R6Class
RelationalDataset <- R6::R6Class( # nolint
  "RelationalDataset",
  inherit = NamedDataset,
  ## RelationalDataset ====
  ## __Public Methods ====
  public = list(
    #' @description
    #' initialize a \code{RelationalDataset} class object
    #' @param x (\code{data.frame})
    #' @param dataname (\code{character}) A given name for the dataset
    #'   it may not contain spaces
    #' @param keys (\code{keys}) object of S3 class keys containing
    #'   foreign, primary keys and parent information
    #' @param code (\code{character}) A character string defining the code
    #'   needed to produce the data set in \code{x}
    #' @param label (\code{character}) Label to describe the dataset
    initialize = function(x, dataname, keys, code = character(0), label = character(0)) {
      super$initialize(x, dataname = dataname, code = code, label = label)

      self$set_keys(keys)
      return(invisible(self))
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
      stopifnot(is(keys, "keys"))
      private$.keys <- keys
      invisible(NULL)
    },
    #' @description
    #' Check if dataset has already been pulled.
    #'
    #' @return \code{TRUE} if dataset has been already pulled, else \code{FALSE}
    is_pulled = function() {
      return(TRUE)
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

#' Create \code{RelationalDataset} object
#'
#' Create \code{RelationalDataset} object
#' @param x (\code{data.frame})
#' @param dataname (\code{character}) A given name for the dataset
#'   it may not contain spaces
#' @param keys (\code{keys}) object of S3 class keys containing
#'   foreign, primary keys and parent information
#' @param code (\code{character}) A character string defining the code
#'   needed to produce the data set in \code{x}
#' @param label (\code{character}) Label to describe the dataset
#' @return object of class \code{RelaionalDataset}
#' @examples
#' library(random.cdisc.data)
#' relational_dataset(
#'   x = radsl(cached = TRUE),
#'   dataname = "ADSL",
#'   keys = get_cdisc_keys("ADSL"),
#'   code = "ADSL <- radsl(cached = TRUE)",
#'   label = "ADSL dataset"
#' )
#' @export
relational_dataset <- function(x, dataname, keys, code, label) {
  RelationalDataset$new(x = x,
                        dataname = dataname,
                        keys = keys,
                        code = code,
                        label = label)
}
