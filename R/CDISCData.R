## CDISCData ====
#' @title Manage multiple `CDISCDataConnector`, `CDISCDatasetConnector` and `CDISCDataset` objects.
#' @description
#' Class manages `CDISCDataConnector`, `CDISCDatasetConnector` and
#' `CDISCDataset` objects and aggregate them in one collection.
#' Class also decides whether to launch app before initialize teal application.
#'
#' @param ... (\code{RelationalDataConnector}, \code{Dataset} or
#'   \code{DatasetConnector}) elements to include where `ADSL` data is mandatory.
#' @param join_keys (`JoinKeys`) or a single (`JoinKeySet`)\cr
#'   (optional) object with datasets column names used for joining.
#'   If empty then it would be automatically derived basing on intersection of datasets primary keys
#'
#' @importFrom R6 R6Class
#' @importFrom methods is
CDISCData <- R6::R6Class( # nolint
  classname = "CDISCData",
  inherit = RelationalData,

  ## __Public Methods ====
  public = list(
    #' @description
    #' Create a new object of `CDISCData` class
    initialize = function(..., join_keys) {
      dot_args <- list(...)

      super$initialize(..., join_keys = join_keys)

      new_parent <- list()
      for (x in dot_args) {
        if (is(x, "Dataset") || is(x, "DatasetConnector")) {
          x_dataname <- x$get_dataname()
          new_parent[[x_dataname]] <- if (is(x, "CDISCDataset") || is(x, "CDISCDatasetConnector")) {
            x$get_parent()
          } else {
            character(0L)
          }
        } else if (is(x, "RelationalDataConnector")) {
          added_parent <- if (is(x, "CDISCDataConnector")) {
            x$get_parent()
          } else {
            sapply(x$get_datanames(), function(i) character(0), USE.NAMES = TRUE, simplify = FALSE)
          }
          tmp_new_parent <- c(new_parent, added_parent)
          new_parent <<- tmp_new_parent
        } else {
          stop(paste("The child elements of CDISCData should be only of Dataset or DatasetConnector or",
                     "RelationalDataConnector class."))
        }
      }

      if (is_dag(new_parent)) {
        stop("Cycle detected in a parent and child dataset graph.")
      }

      private$parent <- new_parent

      # set up join keys as parent keys
      datanames <- self$get_datanames()
      for (d1 in datanames) {
        d1_pk <- get_keys(self$get_items(d1))
        d1_parent <- self$get_parent()[[d1]]
        for (d2 in datanames) {
          if (is_empty(self$get_join_keys()$get(d1, d2))) {
            d2_parent <- self$get_parent()[[d2]]
            d2_pk <- get_keys(self$get_items(d2))

            fk <- if (identical(d1, d2_parent)) {
              # first is parent of second -> parent keys -> first keys
              d1_pk
            } else if (identical(d1_parent, d2)) {
              # second is parent of first -> parent keys -> second keys
              d2_pk
            } else if (identical(d1_parent, d2_parent) && !is_empty(d1_parent)) {
              # both has the same parent -> parent keys
              get_keys(self$get_items(d1_parent))
            } else {
              # cant find connection - leave empty
              next
            }
            self$mutate_join_keys(d1, d2, fk)
          }
        }
      }

      return(invisible(self))
    },
    #' @description
    #' Get all datasets parent names
    #' @return (named `list`) with dataset name and its corresponding parent dataset name
    get_parent = function() {
      private$parent
    },

    # ___ check ====
    #' @description
    #' Check correctness of stored joining keys and presence of keys to parent
    #' @return raise and error or invisible `TRUE`
    check_metadata = function() {

      if (!("ADSL" %in% self$get_datanames())) {
        stop("ADSL dataset is missing.")
      }

      super$check_metadata()

      for (idx1 in seq_along(private$parent)) {
        name_from <- names(private$parent)[[idx1]]
        for (idx2 in seq_along(private$parent[[idx1]])) {
          name_to <- private$parent[[idx1]][[idx2]]
          keys_from <- self$get_join_keys()$get(name_from, name_to)
          keys_to <- self$get_join_keys()$get(name_to, name_from)

          if (is_empty(keys_from) && is_empty(keys_to)) {
            stop(paste0("No join keys from ", name_from, " to its parent (", name_to, ") and vice versa"))
          }
          if (is_empty(keys_from)) {
            stop(paste0("No join keys from ", name_from, " to its parent (", name_to, ")"))
          }
          if (is_empty(keys_to)) {
            stop(paste0("No join keys from ", name_from, " parent name (", name_to, ") to ", name_from))
          }
        }
      }

      return(invisible(TRUE))
    }
  ),

  ## __Private Fields ====
  private = list(
    parent = list() # list with dataset names and its parent dataset names
  )
)

# CONSTRUCTORS ====
#' Data input for teal app
#'
#' @description `r lifecycle::badge("maturing")`
#' Function passes datasets to teal application with option to read preprocessing code and reproducibility checking.
#'
#' @param ... (\code{RelationalDataConnector}, \code{Dataset} or
#'   \code{DatasetConnector}) elements to include where `ADSL` data is mandatory.
#' @param join_keys (`JoinKeys`) or a single (`JoinKeySet`)\cr
#'   (optional) object with datasets column names used for joining.
#'   If empty then it would be automatically derived basing on intersection of datasets primary keys
#' @param code (\code{character}) code to reproduce the datasets.
#' @param check (\code{logical}) reproducibility check - whether evaluated preprocessing code gives the same objects
#'   as provided in arguments. Check is run only if flag is true and preprocessing code is not empty.
#'
#' @return a `CDISCData` object
#'
#' @details This function checks if there were keys added to all data
#'   sets that shall be analyzed inside a teal app.
#'
#' @export
#'
#' @examples
#' library(random.cdisc.data)
#'
#' ADSL <- radsl(cached = TRUE)
#' ADTTE <- radtte(cached = TRUE)
#'
#' # basic example
#' cdisc_data(
#'   cdisc_dataset("ADSL", ADSL),
#'   cdisc_dataset("ADTTE", ADTTE),
#'   code = 'ADSL <- radsl(cached = TRUE)
#'           ADTTE <- radtte(cached = TRUE)',
#'   check = TRUE
#' )
#'
#' # Example with keys
#' cdisc_data(
#'   cdisc_dataset("ADSL", ADSL,
#'     keys = c("STUDYID", "USUBJID")
#'   ),
#'   cdisc_dataset("ADTTE", ADTTE,
#'     keys = c("STUDYID", "USUBJID", "PARAMCD"),
#'     parent = "ADSL"
#'   ),
#'   join_keys = join_keys(
#'     join_key(
#'       "ADSL",
#'       "ADTTE",
#'       c("STUDYID" = "STUDYID", "USUBJID" = "USUBJID")
#'     )
#'   ),
#'   code = "ADSL <- radsl(cached = TRUE)
#'           ADTTE <- radtte(cached = TRUE)",
#'   check = TRUE
#' )
cdisc_data <- function(...,
                       join_keys,
                       code = "",
                       check = FALSE) {
  stopifnot(is_logical_single(check))

  x <- CDISCData$new(..., join_keys = join_keys)
  if (length(code) > 0 && !identical(code, "")) {
    x$set_pull_code(code = code)
  }
  x$set_check(check)

  if (check && is_pulled(x)) {
    x$check()
    if (isFALSE(x$get_check_result())) {
      stop("Reproducibility check failed.")
    }
  }

  return(x)
}

#' Load `CDISCData` object from a file
#'
#' @description `r lifecycle::badge("maturing")`
#'
#' @inheritParams teal_data_file
#'
#' @return `CDISCData` object
#'
#' @importFrom methods is
#'
#' @export
#'
#' @examples
#' file_example <- tempfile(fileext = ".R")
#' writeLines(
#'   text = c(
#'     "library(random.cdisc.data)
#'
#'      # code>
#'      ADSL <- radsl(cached = TRUE)
#'      ADTTE <- radtte(ADSL, cached = TRUE)
#'
#'      cdisc_data(
#'           cdisc_dataset(\"ADSL\", ADSL), cdisc_dataset(\"ADTTE\", ADTTE),
#'           code = \"ADSL <- radsl(cached = TRUE)
#'                   ADTTE <- radtte(ADSL, cached = TRUE)\",
#'           check = FALSE
#'      )
#'      # <code"
#'   ),
#'   con = file_example
#' )
#'
#' cdisc_data_file(file_example)
cdisc_data_file <- function(path, code = get_code(path)) {
  object <- object_file(path, "CDISCData")
  object$mutate(code)
  return(object)
}
