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
#'
#' @export
RelationalDataset <- R6::R6Class( # nolint
  "RelationalDataset",
  inherit = NamedDataset,
  ## RelationalDataset ====
  ## __Public Methods ====
  public = list(
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

#' Make a \link{RawDataset} a \link{RelationalDataset}
#'
#' @param dataset (\link{RawDataset}) object
#' @param dataname (\link{RawDataset}) name (\code{character}) A given name for the dataset
#'   it may not contain spaces
#' @param keys (\code{keys}) object of S3 class keys containing
#'   foreign, primary keys and parent information
#' @param code (\code{character}) A character string defining the code
#'   needed to produce the data set in \code{x}
#' @param label (\code{character}) Label to describe the dataset
#' @return \link{RelationalDataset} object
#'
#' @export
#' @importFrom methods is
#' @examples
#' library(random.cdisc.data)
#' ADSL <- radsl(cached = TRUE)
#' ADSL_raw <- raw_dataset(x = ADSL)
#'
#' ADSL_relational <- as_relational(ADSL_raw,
#'   dataname = "ADSL",
#'   keys = keys(primary = c("USUBJID", "STUDYID"), foreign = NULL, parent = NULL)
#' )
#' get_raw_data(ADSL_relational)
#' ADSL_relational$keys
as_relational <- function(dataset, dataname, keys, code = character(0), label = character(0)) {
  stopifnot(is(dataset, "RawDataset"))

  if (is(dataset, "NamedDataset")) {
    warning("Only raw_data of 'dataset' will be used. All other fields get lost by using 'as_relational'.")
  }

  return(RelationalDataset$new(x = dataset$get_raw_data(), dataname = dataname,
                        keys = keys, code = code, label = label))
}

#' @rdname as_relational
#'
#' @details
#' \code{as_cdisc_relational} will derive the keys by the \code{dataname} and therefore
#'   does not need the \code{keys} argument to be specified
#'
#' @export
as_cdisc_relational <- function(dataset, dataname, code = character(0), label = character(0)) {
  as_relational(dataset = dataset, dataname = dataname,
                keys = get_cdisc_keys(dataname), code = code, label = label)
}
