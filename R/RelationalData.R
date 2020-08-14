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

      private$pull_code <- CodeClass$new()
      private$mutate_code <- CodeClass$new()

      return(invisible(self))
    },
    #' @description
    #'   Check if the object raw data is reproducible from the \code{get_code()} code.
    #' @return
    #'   \code{NULL} if check step has been disabled
    #'   \code{TRUE} if all the datasets generated from evaluating the
    #'   \code{get_code()} code are identical to the raw data, else \code{FALSE}.
    check = function() {
      # code can be put only to the mutate with empty code in datasets
      res <- if (isFALSE(private$.check)) {
        NULL
      } else {
        if (!is_empty(private$pull_code$code)) {
          private$check_combined_code()
        } else {
          all(vapply(private$datasets, function(x) x$check(), logical(1)))
        }
      }
      private$check_result <- res
      return(res)
    },
    #' @description
    #' Get result of reproducibility check
    #' @return \code{NULL} if check has not been called yet, \code{TRUE} / \code{FALSE} otherwise
    get_check_result = function() {
      private$check_result
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
    #' @param only_pull (\code{logical} value)\cr
    #'   if \code{TRUE} only code to pull datasets will be returned without mutate code.
    #'
    #' @return \code{CodeClass}
    get_code_class = function(only_pull = FALSE) {
      all_code_class <- CodeClass$new()

      pull_code_class <- private$get_pull_code_class()
      all_code_class$append(pull_code_class)

      datasets_code_class <- private$get_datasets_code_class()
      all_code_class$append(datasets_code_class)

      if (isFALSE(only_pull)) {
        mutate_code_class <- private$get_mutate_code_class()
        all_code_class$append(mutate_code_class)
      }

      return(all_code_class)
    },
    #' @description
    #' Get names of the datasets.
    #'
    #' @return \code{character} vector with names of all datasets.
    get_datanames = function() {
      vapply(private$datasets, get_dataname, character(1))
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

        res <- self$get_datasets()[[dataname]]
        return(res)
      } else {
        return(self$get_datasets())
      }
    },
    #' @description
    #' Get \code{list} of \code{RelationalDataset} objects.
    #'
    #' @return \code{list} of \code{RelationalDataset}.
    get_datasets = function() {
      if (!self$is_pulled()) {
        stop("Not all datasets have been pulled yet.\n",
             "- Please use `load_datasets()` to retrieve complete results.")
      }

      if (is_empty(private$mutate_code$code)) {
        res <- ulapply(
          private$datasets,
          function(x) {
            if (is_pulled(x)) {
              get_datasets(x)
            } else {
              NULL
            }
          }
        )
      } else {
        # have to evaluate post-processing code (i.e. private$mutate_code) before returning dataset
        new_env <- new.env(parent = parent.env(globalenv()))
        for (dataset in self$get_items()) {
          assign(get_dataname(dataset), get_raw_data(dataset), envir = new_env)
        }

        for (var_idx in seq_along(private$mutate_vars)) {
          mutate_var <- private$mutate_vars[[var_idx]]
          if (is(dataset, "RawDataset") || is(dataset, "RawDatasetConnector")) {
            assign(
              x = names(private$mutate_vars)[[var_idx]],
              value = get_raw_data(mutate_var),
              envir = new_env
            )
          } else {
            assign(
              x = names(private$mutate_vars)[[var_idx]],
              value = mutate_var,
              envir = new_env
            )
          }
        }

        private$mutate_code$eval(envir = new_env)
        res <- sapply(
          self$get_items(),
          function(x) {
            x_name <- x$get_dataname()
            relational_dataset(
              dataname = x_name,
              x = get(x_name, new_env),
              keys = x$get_keys(),
              code = x$get_code(),
              label = x$get_dataset_label()
            )
          },
          USE.NAMES = TRUE,
          simplify = FALSE
        )
      }
      return(if_not_null(res, setNames(res, vapply(res, get_dataname, character(1)))))
    },
    #' @description
    #' Get all datasets and all dataset connectors
    #'
    #'   name of dataset connector to be returned. If \code{NULL}, all connectors are returned.
    #' @param dataname (\code{character} value)\cr
    #'
    #' @return \code{list} with all datasets and all connectors
    get_items = function(dataname = NULL) {
      stopifnot(is.null(dataname) || is_character_single(dataname))

      if (is_character_single(dataname)) {
        if (!(dataname %in% self$get_datanames())) {
          stop(paste("dataset", dataname, "not found"))
        }
        return(private$datasets[[dataname]])
      } else {
        return(private$datasets)
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
    #' Mutate data by code
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
      private$set_mutate_vars(vars)
      private$set_mutate_code(
        code = code,
        deps = names(vars)
      )

      private$check_result <- NULL

      return(invisible(self))
    },
    #' @description
    #' Mutate dataset by code
    #'
    #' @param dataname (\code{character}) Dataname to be mutated
    #' @param code (\code{character}) Code to mutate the dataset. Must contain the
    #'  \code{dataset$dataname}
    #' @param vars (list)\cr
    #'   In case when this object code depends on the \code{raw_data} from the other
    #'   \code{RelationalDataset}, \code{RelationalDatasetConnector} object(s) or other constant value,
    #'   this/these object(s) should be included
    #'
    #' @return self invisibly for chaining
    mutate_dataset = function(dataname, code, vars = list()) {
      stopifnot(is_character_vector(dataname))
      stopifnot(all(dataname %in% self$get_datanames()))

      private$set_mutate_vars(vars = vars)
      private$set_mutate_code(
        code = code,
        dataname = dataname,
        deps = names(vars)
      )

      private$check_result <- NULL

      return(invisible(self))
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
    },
    #' @description
    #' Set pull code
    #'
    #' @param code (\code{character} value)\cr
    #'   code to reproduce \code{data} in \code{RawDataset} objects. Can't be set if any dataset
    #'   has \code{code} set already.
    #'
    #' @return \code{self} invisibly for chaining.
    set_pull_code = function(code) {
      stopifnot(is_character_single(code))
      is_code_set <- vapply(
        X = self$get_items(),
        FUN = function(item) {
          get_code(item, deparse = TRUE) != ""
        },
        FUN.VALUE = logical(1)
      )

      is_dataset <- vapply(
        X = self$get_items(),
        FUN = function(item) {
          is(item, "RawDataset")
        },
        FUN.VALUE = logical(1)
      )

      if (any(is_code_set & is_dataset)) {
        stop(
          "'code' argument should be specified only in the 'cdisc_data' or in 'cdisc_dataset' but not in both",
          call. = FALSE
        )
      }

      if (all(!is_dataset)) {
        stop(
          "Connectors are reproducible by default and setting 'code' argument might break it",
          call. = FALSE
        )
      }

      private$pull_code <- private$pull_code$set_code(
        code = code,
        dataname = self$get_datanames()
      )
      return(invisible(self))
    }
  ),

  ## __Private Fields ====
  private = list(
    datasets = NULL,
    .check = FALSE,
    check_result = NULL, # TRUE / FALSE after calling check()
    mutate_code = NULL, # CodeClass after initialization
    mutate_vars = list(), # named list with vars used to mutate object
    pull_code = NULL, # code to reproduce loading of NamedDataset(s) only

    ## __Private Methods ====
    check_combined_code = function() {
      execution_environment <- new.env(parent = parent.env(globalenv()))
      self$get_code_class(only_pull = TRUE)$eval(envir = execution_environment)
      all(vapply(
        self$get_items(),
        function(dataset) {
          data <- get_raw_data(dataset)
          data_from_code <- get(get_dataname(dataset), execution_environment)
          identical(data, data_from_code)
        },
        logical(1)
      ))
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
    get_mutate_code_class = function() {
      res <- CodeClass$new()
      res$append(list_to_code_class(private$mutate_vars))
      res$append(private$mutate_code)
      return(res)
    },
    get_pull_code_class = function() {
      res <- CodeClass$new()
      res$append(private$pull_code)
      return(res)
    },
    set_mutate_code = function(code, dataname = self$get_datanames(), deps = names(private_mutate_vars)) {
      stopifnot(is_character_vector(code, 0, 1))

      if (length(code) > 0 && code != "") {
        private$mutate_code$set_code(code = code, dataname = dataname, deps = deps)
      }

      return(invisible(self))
    },
    set_mutate_vars = function(vars) {
      stopifnot(is_fully_named_list(vars))

      if (length(vars) > 0) {
        private$mutate_vars <- c(
          private$mutate_vars,
          vars[!names(vars) %in% names(private$mutate_vars)]
        )
      }

      return(invisible(self))
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
      return(invisible(self))
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
