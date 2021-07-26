## MAEDataset ====
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @title  R6 Class representing a \code{MultiAssayExperiment} object with its attributes
#'
#' @description
#' Any \code{MultiAssayExperiment} object can be stored inside this MAEDataset.
#' Some attributes like colnames, dimension or column names for a specific type will
#' be automatically derived.
#'
#' @rdname dataset_class
#'
#' @param dataname (`character`)\cr
#'  A given name for the dataset it may not contain spaces
#'
#' @param x (`MultiAssayExperiment`)\cr
#'
#' @param keys optional, (`character`)\cr
#'   vector with primary keys
#'
#' @param code (`character` or `CodeClass`)\cr
#'   A character string defining the code needed to produce the data set in \code{x}.
#'   \code{initialize()} and \code{recreate()} accept code as \code{CodeClass} also
#'   which is needed to preserve the code uniqueness and correct order.
#'
#' @param label (`character`)\cr
#'   Label to describe the dataset
#'
#' @param vars (named `list`)) \cr
#'   In case when this object code depends on other `Dataset` object(s) or
#'   other constant value, this/these object(s) should be included as named
#'   element(s) of the list. For example if this object code needs `ADSL`
#'   object we should specify `vars = list(ADSL = <adsl object>)`.
#'   It's recommended to include `Dataset` or `DatasetConnector` objects to
#'   the `vars` list to preserve reproducibility. Please note that `vars`
#'   are included to this object as local `vars` and they cannot be modified
#'   within another dataset.
#'
MAEDataset <- R6::R6Class( # nolint
  "MAEDataset",
  inherit = Dataset,
  ## __Public Methods ====
  public = list(
    #' @description
    #' Create a new object of `MAEDataset` class
    #'
    initialize = function(dataname,
                          x,
                          keys = character(0),
                          code = character(0),
                          label = character(0),
                          vars = list()) {
      stopifnot(is_character_single(dataname))
      stopifnot(is(x, "MultiAssayExperiment"))
      stopifnot(is_character_vector(keys, min_length = 0))
      stopifnot(is_character_vector(code, min_length = 0, max_length = 1) || is(code, "CodeClass"))
      stopifnot(is.null(label) || is_character_vector(label, min_length = 0, max_length = 1))
      stopifnot(is.list(vars))

      private$.raw_data <- x
      private$.ncol <- ncol(SummarizedExperiment::colData(x))
      private$.nrow <- nrow(SummarizedExperiment::colData(x))
      private$.dim <- c(private$.nrow, private$.ncol)
      private$.colnames <- colnames(SummarizedExperiment::colData(x))
      private$.rownames <- rownames(SummarizedExperiment::colData(x))
      private$.col_labels <- vapply(
        X = colData(x),
        FUN = function(x) {
          label <- attr(x, "label")
          if (length(label) != 1) {
            NA_character_
          } else {
            label
          }
        },
        FUN.VALUE = character(1)
      )

      private$set_dataname(dataname)
      self$set_vars(vars)
      self$set_dataset_label(label)
      self$set_keys(keys)

      # needed if recreating dataset - we need to preserve code order and uniqueness
      private$code <- CodeClass$new()
      if (is.character(code)) {
        self$set_code(code)
      } else {
        private$code$append(code)
      }

      return(invisible(self))
    },
    # ___ check ====
    #' @description
    #'   Check to determine if the raw data is reproducible from the \code{get_code()} code.
    #' @return
    #'   \code{TRUE} if the dataset generated from evaluating the
    #'   \code{get_code()} code is identical to the raw data, else \code{FALSE}.
    check = function() {
      if (!is_character_single(self$get_code()) || !grepl("\\w+", self$get_code())) {
        stop(
          sprintf(
            "Cannot check preprocessing code of '%s' - code is empty.",
            self$get_dataname()
          )
        )
      }

      new_set <- private$execute_code(
        code = self$get_code_class(),
        vars = private$vars
      )
      res_check <- tryCatch({
        identical(self$get_raw_data(), new_set)
      }, error = function(e) {
        FALSE
      })

      return(res_check)
    },
    #' Check if keys has been specified correctly for dataset. Set of \code{keys}
    #' should distinguish unique rows or be `character(0)`.
    #'
    #' @return \code{TRUE} if dataset has been already pulled, else \code{FALSE}
    check_keys = function(keys = private$.keys) {
      if (!is_empty(keys)) {
        stop_if_not(list(
          all(keys %in% self$get_colnames()),
          paste("Primary keys specifed for", self$get_dataname(), "do not exist in the data.")
        ))

        duplicates <- get_key_duplicates(as.data.frame(SummarizedExperiment::colData(self$get_raw_data())), keys)
        if (nrow(duplicates) > 0) {
          stop(
            "Duplicate primary key values found in the dataset '", self$get_dataname(), "' :\n",
            paste0(utils::capture.output(print(duplicates))[-c(1, 3)], collapse = "\n"),
            call. = FALSE
          )
        }
      }
    },
    #' Prints this MAEDataset.
    #' @param ... additional arguments to the printing method
    #'
    #' @return invisibly self
    print = function(...) {
      cat(sprintf("A MAEDataset object containing data of %d subjects.", private$.nrow))
      invisible(self)
    }
  ),
  ## __Private Fields ====
  private = list(
    .raw_data = MultiAssayExperiment::MultiAssayExperiment(),

    get_class_colnames = function(class_type = "character") {
      stopifnot(is_character_single(class_type))

      return_cols <- private$.colnames[which(vapply(
        lapply(SummarizedExperiment::colData(private$.raw_data), class),
        function(x, target_class_name) any(x %in% target_class_name),
        logical(1),
        target_class_name = class_type))]

      return(return_cols)
    },

    # Evaluate script code to modify data or to reproduce data
    #
    # @param code (\code{CodeClass}) the object storing the code to execute
    # @param vars (named \code{list}) additional pre-requisite vars to execute code
    # @return (\code{environment}) which stores modified \code{x}
    execute_code = function(code, vars = list()) {
      stopifnot(is(code, "CodeClass"))
      stopifnot(is_fully_named_list(vars))

      execution_environment <- new.env(parent = parent.env(globalenv()))

      # set up environment for execution
      for (vars_idx in seq_along(vars)) {
        var_name <- names(vars)[[vars_idx]]
        var_value <- vars[[vars_idx]]
        if (is(var_value, "DatasetConnector") || is(var_value, "Dataset")) {
          var_value <- get_raw_data(var_value)
        }
        assign(envir = execution_environment, x = var_name, value = var_value)
      }

      # execute
      code$eval(envir = execution_environment)

      if (!is(execution_environment[[self$get_dataname()]], "MultiAssayExperiment")) {
        out_msg <- sprintf(
          "\n%s\n\n - Code from %s needs to return a MultiAssayExperiment assigned to an object of dataset name.",
          self$get_code(),
          self$get_dataname()
        )

        rlang::with_options(
          .expr = stop(out_msg, call. = FALSE),
          warning.length = max(min(8170, nchar(out_msg) + 30), 100)
        )
      }

      new_set <- execution_environment[[self$get_dataname()]]

      return(new_set)
    }
  )
)


#' S3 method to construct an MAEDataset object from MultiAssayExperiment
#'
#' @inheritParams dataset
#'
#' @examples
#' # Simple example
#' \dontrun{
#' library(MultiAssayExperiment)
#' MAE_dataset <- dataset("MAE", miniACC, keys = c("STUDYID", "USUBJID"))
#' MAE_dataset$get_dataname()
#' MAE_dataset$get_dataset_label()
#' MAE_dataset$get_code()
#' MAE_dataset$get_raw_data()
#' }
#' @export
dataset.MultiAssayExperiment <- function(dataname, # nousage
                                         x,
                                         keys = character(0),
                                         label = data_label(x),
                                         code = character(0),
                                         vars = list()) {
  stopifnot(is_character_single(dataname))
  stopifnot(is_character_vector(code, min_length = 0, max_length = 1) || is(code, "CodeClass"))
  stopifnot(identical(vars, list()) || is_fully_named_list(vars))

  MAEDataset$new(
    dataname = dataname,
    x = x,
    keys = keys,
    code = code,
    label = label,
    vars = vars
  )
}
