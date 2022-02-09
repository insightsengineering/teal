## CDISCTealData ====
#'
#' @title Manage multiple `CDISCTealDataConnector`, `CDISCTealDatasetConnector` and `CDISCTealDataset` objects.
#'
#' @description `r lifecycle::badge("stable")`
#' Class manages `CDISCTealDataConnector`, `CDISCTealDatasetConnector` and
#' `CDISCTealDataset` objects and aggregate them in one collection.
#' Class also decides whether to launch app before initialize teal application.
#'
#' @param ... (`TealDataConnector`, `TealDataset` or
#'   `TealDatasetConnector`) elements to include where `ADSL` data is mandatory.
#' @param join_keys (`JoinKeys`) or a single (`JoinKeySet`)\cr
#'   (optional) object with datasets column names used for joining.
#'   If empty then it would be automatically derived basing on intersection of datasets primary keys
#' @param check (`logical`) reproducibility check - whether evaluated preprocessing code gives the same objects
#'   as provided in arguments. Check is run only if flag is true and preprocessing code is not empty.
#'
CDISCTealData <- R6::R6Class( # nolint
  classname = "CDISCTealData",
  inherit = TealData,

  ## __Public Methods ====
  public = list(
    #' @description
    #' Create a new object of `CDISCTealData` class
    initialize = function(..., check = FALSE, join_keys) {
      dot_args <- list(...)

      super$initialize(..., check = check, join_keys = join_keys)

      new_parent <- list()
      for (x in dot_args) {
        if (is(x, "TealDataset") || is(x, "TealDatasetConnector")) {
          x_dataname <- x$get_dataname()
          new_parent[[x_dataname]] <- if (is(x, "CDISCTealDataset") || is(x, "CDISCTealDatasetConnector")) {
            x$get_parent()
          } else {
            character(0L)
          }
        } else if (is(x, "TealDataConnector")) {
          added_parent <- if (is(x, "CDISCTealDataConnector")) {
            x$get_parent()
          } else {
            sapply(x$get_datanames(), function(i) character(0), USE.NAMES = TRUE, simplify = FALSE)
          }
          new_parent <- c(new_parent, added_parent)
        } else {
          stop(paste(
            "The child elements of CDISCTealData should be only of TealDataset or TealDatasetConnector or",
            "TealDataConnector class."
          ))
        }
      }

      if (is_dag(new_parent)) {
        stop("Cycle detected in a parent and child dataset graph.")
      }

      private$parent <- new_parent

      # for performance, get_join_keys should be called once outside of any loop
      join_keys <- self$get_join_keys()

      # set up join keys as parent keys
      datanames <- self$get_datanames()
      for (d1 in datanames) {
        d1_pk <- get_keys(self$get_items(d1))
        d1_parent <- self$get_parent()[[d1]]
        for (d2 in datanames) {
          if (length(join_keys$get(d1, d2)) == 0) {
            d2_parent <- self$get_parent()[[d2]]
            d2_pk <- get_keys(self$get_items(d2))

            fk <- if (identical(d1, d2_parent)) {
              # first is parent of second -> parent keys -> first keys
              d1_pk
            } else if (identical(d1_parent, d2)) {
              # second is parent of first -> parent keys -> second keys
              d2_pk
            } else if (identical(d1_parent, d2_parent) && length(d1_parent) > 0) {
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

      logger::log_trace("CDISCTealData initialized with data: { paste(self$get_datanames(), collapse = ' ') }.")
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
      logger::log_trace("CDISCTealData$check_metadata checking metadata...")
      if (!("ADSL" %in% self$get_datanames())) {
        stop("ADSL dataset is missing.")
      }

      super$check_metadata()
      # for performance, get_join_keys should be called once outside of any loopp
      join_keys <- self$get_join_keys()
      for (idx1 in seq_along(private$parent)) {
        name_from <- names(private$parent)[[idx1]]
        for (idx2 in seq_along(private$parent[[idx1]])) {
          name_to <- private$parent[[idx1]][[idx2]]
          keys_from <- join_keys$get(name_from, name_to)
          keys_to <- join_keys$get(name_to, name_from)

          if (length(keys_from) == 0 && length(keys_to) == 0) {
            stop(sprintf("No join keys from %s to its parent (%s) and vice versa", name_from, name_to))
          }
          if (length(keys_from) == 0) {
            stop(sprintf("No join keys from %s to its parent (%s)", name_from, name_to))
          }
          if (length(keys_to) == 0) {
            stop(sprintf("No join keys from %s parent name (%s) to %s", name_from, name_to, name_from))
          }
        }
      }
      logger::log_trace("CDISCTealData$check_metadata metadata check passed.")

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
#' @description `r lifecycle::badge("stable")`
#' Function passes datasets to teal application with option to read preprocessing code and reproducibility checking.
#'
#' @note This function does not automatically assign keys to `TealDataset`
#' and `TealDatasetConnector` objects passed to it. If the keys are needed
#' they should be assigned before calling `cdisc_data`. See example:
#' ```
# library(scda)
# test_dataset <- dataset("ADAE", synthetic_cdisc_data("latest")$adae) # does not have keys
# test_adsl <- cdisc_dataset("ADSL", synthetic_cdisc_data("latest")$adsl)
# test_data <- cdisc_data(test_dataset, test_adsl)
# get_keys(test_data, "ADAE") # returns character(0)
#
# test_dataset <- cdisc_dataset("ADAE", synthetic_cdisc_data("latest")$adae)
# test_data <- cdisc_data(test_dataset, test_adsl)
# get_keys(test_data, "ADAE") # returns [1] "STUDYID" "USUBJID" "ASTDTM"  "AETERM"  "AESEQ"
#' ```
#' @inheritParams teal_data
#' @param ... (`TealDataConnector`, `TealDataset` or
#'   `TealDatasetConnector`) elements to include where `ADSL` data is mandatory.
#' @param join_keys (`JoinKeys`) or a single (`JoinKeySet`)\cr
#'   (optional) object with datasets column names used for joining.
#'   If empty then it would be automatically derived basing on intersection of datasets primary keys
#'
#' @return a `CDISCTealData` object
#'
#' @details This function checks if there were keys added to all data
#'   sets that shall be analyzed inside a teal app.
#'
#' @export
#'
#' @examples
#' library(scda)
#'
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#' ADTTE <- synthetic_cdisc_data("latest")$adtte
#'
#' # basic example
#' cdisc_data(
#'   cdisc_dataset("ADSL", ADSL),
#'   cdisc_dataset("ADTTE", ADTTE),
#'   code = 'ADSL <- synthetic_cdisc_data("latest")$adsl
#'           ADTTE <- synthetic_cdisc_data("latest")$adtte',
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
#'   code = 'ADSL <- synthetic_cdisc_data("latest")$adsl
#'           ADTTE <- synthetic_cdisc_data("latest")$adtte',
#'   check = TRUE
#' )
cdisc_data <- function(...,
                       join_keys,
                       code = "",
                       check = FALSE) {
  x <- CDISCTealData$new(..., check = check, join_keys = join_keys)
  if (length(code) > 0 && !identical(code, "")) {
    x$set_pull_code(code = code)
  }

  x$check_reproducibility()
  x$check_metadata()

  return(x)
}

#' Load `CDISCTealData` object from a file
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @inheritParams teal_data_file
#'
#' @return `CDISCTealData` object
#'
#' @export
#'
#' @examples
#' file_example <- tempfile(fileext = ".R")
#' writeLines(
#'   text = c(
#'     "library(scda)
#'
#'      # code>
#'      ADSL <- synthetic_cdisc_data('latest')$adsl
#'      ADTTE <- synthetic_cdisc_data('latest')$adtte
#'
#'      cdisc_data(
#'           cdisc_dataset(\"ADSL\", ADSL), cdisc_dataset(\"ADTTE\", ADTTE),
#'           code = \"ADSL <- synthetic_cdisc_data('latest')$adsl
#'                   ADTTE <- synthetic_cdisc_data('latest')$adtte\",
#'           check = FALSE
#'      )
#'      # <code"
#'   ),
#'   con = file_example
#' )
#'
#' cdisc_data_file(file_example)
cdisc_data_file <- function(path, code = get_code(path)) {
  object <- object_file(path, "CDISCTealData")
  object$mutate(code)
  return(object)
}
