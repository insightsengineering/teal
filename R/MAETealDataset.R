## MAETealDataset ====
#'
#' @title  R6 Class representing a `MultiAssayExperiment` object with its attributes
#'
#' @description `r lifecycle::badge("experimental")`
#' Any `MultiAssayExperiment` object can be stored inside this `MAETealDataset`.
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
#'   A character string defining the code needed to produce the data set in `x`.
#'   `initialize()` and `recreate()` accept code as `CodeClass` also
#'   which is needed to preserve the code uniqueness and correct order.
#'
#' @param label (`character`)\cr
#'   Label to describe the dataset
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
#'
MAETealDataset <- R6::R6Class( # nolint
  "MAETealDataset",
  inherit = TealDataset,
  ## __Public Methods ====
  public = list(
    #' @description
    #' Create a new object of `MAETealDataset` class
    #'
    initialize = function(dataname,
                          x,
                          keys = character(0),
                          code = character(0),
                          label = character(0),
                          vars = list()) {
      checkmate::assert_string(dataname)
      stopifnot(is(x, "MultiAssayExperiment"))
      checkmate::assert_character(keys, any.missing = FALSE)
      checkmate::assert(
        checkmate::check_character(code, max.len = 1, any.missing = FALSE),
        checkmate::check_class(code, "CodeClass")
      )
      checkmate::assert_character(label, max.len = 1, null.ok = TRUE, any.missing = FALSE)
      checkmate::assert_list(vars, min.len = 0, names = "unique")

      private$.raw_data <- x
      private$.ncol <- ncol(SummarizedExperiment::colData(x))
      private$.nrow <- nrow(SummarizedExperiment::colData(x))
      private$.dim <- c(private$.nrow, private$.ncol)
      private$.colnames <- colnames(SummarizedExperiment::colData(x))
      private$.rownames <- rownames(SummarizedExperiment::colData(x))
      private$.col_labels <- vapply(
        X = SummarizedExperiment::colData(x),
        FUN.VALUE = character(1),
        FUN = function(x) {
          label <- attr(x, "label")
          if (length(label) != 1) {
            NA_character_
          } else {
            label
          }
        }
      )
      private$.row_labels <- c()

      private$set_dataname(dataname)
      self$set_vars(vars)
      self$set_dataset_label(label)
      self$set_keys(keys)
      private$calculate_hash()

      # needed if recreating dataset - we need to preserve code order and uniqueness
      private$code <- CodeClass$new()
      if (is.character(code)) {
        self$set_code(code)
      } else {
        private$code$append(code)
      }

      logger::log_trace("MAETealDataset$initialize initialized dataset: { deparse1(self$get_dataname()) }.")

      return(invisible(self))
    },
    # ___ check ====
    #' @description
    #' Check to determine if the raw data is reproducible from the `get_code()` code.
    #' @return
    #' `TRUE` if the dataset generated from evaluating the
    #' `get_code()` code is identical to the raw data, else `FALSE`.
    check = function() {
      logger::log_trace("TealDataset$check executing the code to reproduce dataset: { deparse1(self$get_dataname()) }...")
      if (!checkmate::test_character(self$get_code(), len = 1, pattern = "\\w+")) {
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
      res_check <- tryCatch(
        {
          identical(self$get_raw_data(), new_set)
        },
        error = function(e) {
          FALSE
        }
      )
      logger::log_trace("TealDataset$check { deparse1(self$get_dataname()) } reproducibility result: { res_check }.")

      return(res_check)
    },
    #' @description
    #' Check if keys has been specified correctly for dataset. Set of `keys`
    #' should distinguish unique rows or be `character(0)`.
    #'
    #' @return `TRUE` if dataset has been already pulled, else `FALSE`
    check_keys = function(keys = private$.keys) {
      if (length(keys) > 0) {
        if (!all(keys %in% self$get_colnames())) {
          stop("Primary keys specifed for ", self$get_dataname(), " do not exist in the data.")
        }

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
    #' @description
    #' Prints this `MAETealDataset`.
    #' @param ... additional arguments to the printing method
    #'
    #' @return invisibly self
    print = function(...) {
      cat(sprintf("A MAETealDataset object containing data of %d subjects.\n", private$.nrow))
      print(experiments(private$.raw_data))
      invisible(self)
    }
  ),
  ## __Private Fields ====
  private = list(
    .raw_data = MultiAssayExperiment::MultiAssayExperiment(),
    get_class_colnames = function(class_type = "character") {
      checkmate::assert_string(class_type)

      return_cols <- private$.colnames[which(vapply(
        lapply(SummarizedExperiment::colData(private$.raw_data), class),
        function(x, target_class_name) any(x %in% target_class_name),
        logical(1),
        target_class_name = class_type
      ))]

      return(return_cols)
    },

    # Evaluate script code to modify data or to reproduce data
    #
    # @param code (`CodeClass`) the object storing the code to execute
    # @param vars (named `list`) additional pre-requisite vars to execute code
    # @return (`environment`) which stores modified `x`
    execute_code = function(code, vars = list()) {
      stopifnot(is(code, "CodeClass"))
      checkmate::assert_list(vars, min.len = 0, names = "unique")

      execution_environment <- new.env(parent = parent.env(globalenv()))

      # set up environment for execution
      for (vars_idx in seq_along(vars)) {
        var_name <- names(vars)[[vars_idx]]
        var_value <- vars[[vars_idx]]
        if (is(var_value, "TealDatasetConnector") || is(var_value, "TealDataset")) {
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

#' S3 method to construct an `MAETealDataset` object from `MultiAssayExperiment`
#'
#' @rdname dataset
#'
#' @examples
#' # Simple example
#' mae_d <- dataset("MAE", MultiAssayExperiment::miniACC, keys = c("STUDYID", "USUBJID"))
#' mae_d$get_dataname()
#' mae_d$get_dataset_label()
#' mae_d$get_code()
#' mae_d$get_raw_data()
#' @export
dataset.MultiAssayExperiment <- function(dataname,
                                         x,
                                         keys = character(0),
                                         label = data_label(x),
                                         code = character(0),
                                         vars = list()) {
  checkmate::assert_string(dataname)
  checkmate::assert(
    checkmate::check_character(code, max.len = 1, any.missing = FALSE),
    checkmate::check_class(code, "CodeClass")
  )
  checkmate::assert_list(vars, min.len = 0, names = "unique")

  MAETealDataset$new(
    dataname = dataname,
    x = x,
    keys = keys,
    code = code,
    label = label,
    vars = vars
  )
}

#' The constructor of `MAETealDataset`
#'
#' @description `r lifecycle::badge("deprecated")`
#'
#' @inheritParams dataset
#' @param x (`MultiAssayExperiment`)
#'
#' @examples
#' # Simple example
#' mae_d <- dataset("MAE", MultiAssayExperiment::miniACC)
#' mae_d$get_dataname()
#' mae_d$get_dataset_label()
#' mae_d$get_code()
#' mae_d$get_raw_data()
#' @export
mae_dataset <- function(dataname,
                        x,
                        label = data_label(x),
                        code = character(0),
                        vars = list()) {
  lifecycle::deprecate_soft(
    when = "0.10.1",
    what = "teal::mae_dataset()",
    with = "teal::dataset()"
  )

  if (!is(x, "MultiAssayExperiment")) {
    stop("Argument x must be a MultiAssayExperiment object")
  }

  dataset(
    dataname = dataname,
    x = x,
    code = code,
    label = label,
    vars = vars
  )
}
