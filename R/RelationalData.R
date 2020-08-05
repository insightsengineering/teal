## RelationalData ====
#' @title \code{RelationalData} class
#' @description
#' Class combines multiple \code{RelationalDataset} objects.
#'
#' @importFrom R6 R6Class
#' @importFrom rlang with_options
#'
#' @examples
#' x <- relational_dataset(
#'   x = data.frame(x = c(1, 2), y = c("a", "b"), stringsAsFactors = FALSE),
#'   keys = keys(primary = "y", foreign = NULL, parent = NULL),
#'   dataname = "XY",
#'   code = "XY <- data.frame(x = c(1, 2), y = c('a', 'b'),
#'                            stringsAsFactors = FALSE)",
#'   label = character(0)
#' )
#'
#' x2 <- relational_dataset(
#'   x = data.frame(x = c(1, 2), y = c("a", "b"), stringsAsFactors = FALSE),
#'   keys = keys(primary = "y", foreign = NULL, parent = NULL),
#'   dataname = "XYZ",
#'   code = "XYZ <- data.frame(x = c(1, 2), y = c('a', 'b'),
#'                            stringsAsFactors = FALSE)",
#'   label = character(0)
#' )
#'
#' rd <- teal:::RelationalData$new(x, x2)
RelationalData <- R6::R6Class( #nolint
  classname = "RelationalData",
  ## __Public Methods ====
  public = list(
    #' @description
    #' Create a new \code{RelationalData} object from multiple
    #' \code{RelationalDataset} objects.
    #'
    #' @param ... (\code{RelationalDataset})\cr
    #'  at least one object.
    #'
    #' @return new \code{RelationalData} object
    initialize = function(...) {
      datasets <- list(...)
      stopifnot(is_class_list("RelationalDataset")(datasets))

      dataset_names <- vapply(datasets, get_dataname, character(1))
      names(datasets) <- dataset_names

      private$check_names(datasets)

      private$datasets <- datasets

      private$code <- CodeClass$new()

      return(invisible(self))
    },
    #' @description
    #'   Check if the object raw data is reproducible from the \code{get_code()} code.
    #' @return
    #'   \code{TRUE} if all the datasets generated from evaluating the
    #'   \code{get_code()} code are identical to the raw data, else \code{FALSE}.
    check = function() {
      # code can be put only to the mutate with empty code in datasets
      if (!is_empty(private$code$code)) {
        private$check_combined_code()
      } else {
        all(vapply(private$datasets, function(x) x$check(), logical(1)))
      }
    },
    #' @description
    #' Get all datasets and all dataset connectors
    #'
    #'   name of dataset connector to be returned. If \code{NULL}, all connectors are returned.
    #' @param dataname (\code{character} value)\cr
    #'
    #' @return \code{list} with all datasets and all connectors
    get_all_datasets = function(dataname = NULL) {
      stopifnot(is.null(dataname) || is_character_single(dataname))
      if (is.null(dataname)) {
        private$datasets
      } else {
        private$datasets[[dataname]]
      }
    },
    #' @description
    #' Get names of the datasets.
    #'
    #' @return \code{character} vector with names of all datasets.
    get_datanames = function() {
      vapply(private$datasets, get_dataname, character(1))
    },
    #' @description
    #' Get code for all datasets.
    #' @param dataname (\code{character}) dataname or \code{NULL} for all datasets
    #' @param deparse (\code{logical}) whether to return the deparsed form of a call
    #' @return (\code{character}) vector of code to generate datasets.
    get_code = function(dataname = NULL, deparse = TRUE) {
      stopifnot(is.null(dataname) || is_character_vector(dataname))
      stopifnot(is_logical_single(deparse))

      return(self$get_code_class()$get_code(dataname = dataname, deparse = deparse))
    },
    #' @description
    #' Get internal \code{CodeClass} object
    #'
    #' @return \code{CodeClass}
    get_code_class = function() {
      all_code_class <- CodeClass$new()

      datasets_code_class <- private$get_datasets_code_class()
      all_code_class$append(datasets_code_class)

      mutate_code_class <- private$get_mutate_code_class()
      all_code_class$append(mutate_code_class)

      return(all_code_class)
    },

    #' @description
    #' Mutate data by code
    #'
    #' Either code or script must be provided, but not both.
    #'
    #' @param code (\code{character}) Code to mutate the dataset. Must contain the
    #'  \code{dataset$dataname}
    #' @param vars (list)\cr
    #'   In case when this object code depends on the \code{raw_data} from the other
    #'   \code{RelationalDataset}, \code{RelationalDatasetConnector} object(s) or other constant value,
    #'   this/these object(s) should be included
    #'
    #' @return self invisibly for chaining
    mutate = function(code, vars = list()) {
      private$set_vars(vars)
      private$set_code(code)

      return(invisible(self))
    },
    #' @description
    #' Get \code{RelationalDataset} object.
    #'
    #' @param dataname (\code{character} value)\cr
    #'   name of dataset to be returned. If \code{NULL}, all datasets are returned.
    #'
    #' @return \code{RelationalDataset}.
    get_dataset = function(dataname = NULL) {
      stopifnot(is.null(dataname) || is_character_single(dataname))

      if (is_character_single(dataname)) {
        if (!(dataname %in% self$get_datanames())) {
          stop(paste("dataset", dataname, "not found"))
        }
        return(self$get_datasets()[[dataname]])
      }

      return(self$get_datasets())
    },
    #' @description
    #' Get \code{list} of \code{RelationalDataset} objects.
    #'
    #' @return \code{list} of \code{RelationalDataset}.
    get_datasets = function() {

      if (is.null(private$code)) {
        private$datasets
      } else {
        # have to evaluate post-processing code (i.e. private$code) before returning dataset
        new_env <- new.env(parent = parent.env(globalenv()))
        for (dataset in private$datasets) {
          assign(dataset$get_dataname(), get_raw_data(dataset), envir = new_env)
        }

        private$code$eval(envir = new_env)

        lapply(
          private$datasets,
          function(x) {
            x_name <- x$get_dataname()
            relational_dataset(
              dataname = x_name,
              x = get(x_name, new_env),
              keys = x$get_keys(),
              code = x$get_code(),
              label = x$get_dataset_label()
            )
          }
        )
      }
    },
    #' @description
    #' Check if dataset has already been pulled.
    #'
    #' @return \code{TRUE} if dataset has been already pulled, else \code{FALSE}
    is_pulled = function() {
      all(vapply(private$datasets, is_pulled, logical(1)))
    },
    #' @description
    #' Set reproducibility check
    #'
    #' @param check (\code{logical}) whether to perform reproducibility check.
    #'
    #' @return \code{self} invisibly for chaining.
    set_check = function(check = FALSE) {
      stopifnot(is_logical_single(check))
      private$.check <- check
      return(invisible(self))
    }
  ),

  ## __Private Fields ====
  private = list(
    datasets = NULL,
    code = NULL, # CodeClass after initialization
    mutate_vars = list(), # named list with vars used to mutate object
    .check = FALSE,

    ## __Private Methods ====
    check_combined_code = function() {
      execution_environment <- new.env(parent = parent.env(globalenv()))
      eval(parse(text = self$get_code()), execution_environment)
      lapply(
        self$get_all_datasets(),
        function(dataset) {

          is_identical <- identical(
            get_raw_data(dataset),
            get(get_dataname(dataset), execution_environment)
          )

          if (!is_identical) {
            out_msg <- sprintf(
              "\n%s\n\n - code doesn't reproduce '%s' correctly",
              self$get_code(),
              get_dataname(dataset)
            )

            rlang::with_options(
              .expr = stop(out_msg, call. = FALSE),
              warning.length = max(min(8170, nchar(out_msg) + 30), 100)
            )
          }
        }
      )

      return(TRUE)
    },
    get_mutate_code_class = function() {
      res <- CodeClass$new()
      res$append(list_to_code_class(private$mutate_vars))
      res$append(private$code)
      return(res)
    },
    get_datasets_code_class = function() {
      res <- CodeClass$new()
      if (is.null(private$datasets)) {
        return(res)
      }
      for (dataset in private$datasets) {
        res$append(dataset$get_code_class())
      }
      return(res)
    },
    set_code = function(code) {
      stopifnot(is_character_vector(code, 0, 1))

      if (length(code) > 0 && code != "") {
        private$code$set_code(code = code,
                              dataname = self$get_datanames())
      }

      return(invisible(NULL))
    },
    set_vars = function(vars) {
      stopifnot(is_fully_named_list(vars))

      if (length(vars) > 0) {
        private$mutate_vars <- c(private$mutate_vars, vars)
      }

      return(invisible(NULL))
    },
    check_names = function(x) {
      x_names <- names(x)
      if (any(vapply(x_names, is_empty_string, logical(1)))) {
        stop("Cannot extract some dataset names")
      }
      if (any(duplicated(x_names))) {
        stop("Datasets names should be unique")
      }
      if (any(x_names %in% self$get_datanames())) {
        stop("Some datanames already exists")
      }
      return(invisible(NULL))
    },
    check_dataset_all_code = function(dataset) {
      new_env <- new.env(parent = parent.env(.GlobalEnv))
      tryCatch({
        self$get_code_class()$eval(envir = new_env)
      }, error = function(e) {
        error_dialog(e)
      })

      res_check <- tryCatch({
        identical(get_raw_data(dataset), get(get_dataname(dataset), envir = new_env))
      }, error = function(e) {
        FALSE
      })

      return(res_check)
    }
  )
)
