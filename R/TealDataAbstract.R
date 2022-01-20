## TealDataAbstract ====
#' @title `TealDataAbstract` class
#'
#' @description
#' Abstract class containing code for handling set of datasets.
#' @keywords internal
TealDataAbstract <- R6::R6Class( # nolint
  classname = "TealDataAbstract",
  ## __Public Methods ====
  public = list(
    #' @description
    #' Cannot create a `TealDataAbstract` object
    #'
    #' @return throws error
    initialize = function() {
      stop("Pure virtual method")
    },
    #' @description
    #' Check if the object raw data is reproducible from the `get_code()` code.
    #' @return
    #'   `NULL` if check step has been disabled
    #'   `TRUE` if all the datasets generated from evaluating the
    #'   `get_code()` code are identical to the raw data, else `FALSE`.
    check = function() {
      # code can be put only to the mutate with empty code in datasets
      res <- if (isFALSE(private$.check)) {
        NULL
      } else {
        if (length(private$pull_code$code) > 0) {
          private$check_combined_code()
        } else {
          all(vapply(
            private$datasets,
            function(x) {
              check_res <- x$check()
              # NULL is still ok
              is.null(check_res) || isTRUE(check_res)
            },
            logical(1)
          ))
        }
      }
      private$check_result <- res
      logger::log_trace("TealDataAbstract$check executed the code to reproduce the data - result: { res }.")
      res
    },
    #' @description
    #' Execute `check()` and raise an error if it's not reproducible.
    #' @return error if code is not reproducible else invisibly nothing
    check_reproducibility = function() {
      self$check()
      if (isFALSE(self$get_check_result())) {
        stop("Reproducibility check failed.")
      }
      logger::log_trace("TealDataAbstract$check_reproducibility reproducibility check passed.")
      return(invisible(NULL))
    },
    #' @description
    #' Execute mutate code. Using `mutate_data(set).TealDataAbstract`
    #' does not cause instant execution, the `mutate_code` is
    #' delayed and can be evaluated using this method.
    execute_mutate = function() {
      logger::log_trace("TealDataAbstract$execute_mutate evaluating mutate code...")
      # this will be pulled already! - not needed?
      if (length(private$mutate_code$code) == 0) {
        res <- unlist(lapply(
          private$datasets,
          function(x) {
            if (is_pulled(x)) {
              get_datasets(x)
            } else {
              NULL
            }
          }
        ))
        # exit early if mutate isn't required
        logger::log_trace("TealDataAbstract$execute_mutate no code to evaluate.")
        if (!is.null(res)) {
          res <- stats::setNames(res, vapply(res, get_dataname, character(1)))
        }
        return(res)
      }


      if (inherits(private$mutate_code, "PythonCodeClass")) {
        items <- lapply(self$get_items(), get_raw_data)
        datasets <- stats::setNames(items, vapply(self$get_items(), get_dataname, character(1)))

        new_env <- private$mutate_code$eval(vars = c(datasets, private$mutate_vars))
      } else {
        # have to evaluate post-processing code (i.e. private$mutate_code) before returning dataset
        new_env <- new.env(parent = parent.env(globalenv()))
        for (dataset in self$get_items()) {
          assign(get_dataname(dataset), get_raw_data(dataset), envir = new_env)
        }

        for (var_idx in seq_along(private$mutate_vars)) {
          mutate_var <- private$mutate_vars[[var_idx]]
          assign(
            x = names(private$mutate_vars)[[var_idx]],
            value = `if`(
              is(mutate_var, "TealDataset") || is(mutate_var, "TealDatasetConnector"),
              get_raw_data(mutate_var),
              mutate_var
            ),
            envir = new_env
          )
        }

        private$mutate_code$eval(envir = new_env)
      }

      lapply(
        self$get_datasets(),
        function(x) {
          x$recreate(
            x = get(get_dataname(x), new_env)
          )
        }
      )
      logger::log_trace("TealDataAbstract$execute_mutate evaluated mutate code.")
      return(invisible(NULL))
    },
    #' @description
    #' Get result of reproducibility check
    #' @return `NULL` if check has not been called yet, `TRUE` / `FALSE` otherwise
    get_check_result = function() {
      private$check_result
    },
    #' @description
    #' Get code for all datasets.
    #' @param dataname (`character`) dataname or `NULL` for all datasets
    #' @param deparse (`logical`) whether to return the deparsed form of a call
    #' @return (`character`) vector of code to generate datasets.
    get_code = function(dataname = NULL, deparse = TRUE) {
      checkmate::assert_character(dataname, min.len = 1, null.ok = TRUE, any.missing = FALSE)
      checkmate::assert_flag(deparse)

      return(self$get_code_class()$get_code(dataname = dataname, deparse = deparse))
    },
    #' @description
    #' Get internal `CodeClass` object
    #' @param only_pull (`logical` value)\cr
    #'   if `TRUE` only code to pull datasets will be returned without the mutate code.
    #'
    #' @return `CodeClass`
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
    #' @return `character` vector with names of all datasets.
    get_datanames = function() {
      vapply(private$datasets, get_dataname, character(1))
    },
    #' @description
    #' Get `TealDataset` object.
    #'
    #' @param dataname (`character` value)\cr
    #'   name of dataset to be returned. If `NULL`, all datasets are returned.
    #'
    #' @return `TealDataset`.
    get_dataset = function(dataname = NULL) {
      checkmate::assert_string(dataname, null.ok = TRUE)

      if (length(dataname) == 1) {
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
    #' Get `list` of `TealDataset` objects.
    #'
    #' @return `list` of `TealDataset`.
    get_datasets = function() {
      if (!self$is_pulled()) {
        stop(
          "Not all datasets have been pulled yet.\n",
          "- Please use `load_datasets()` to retrieve complete results."
        )
      }
      unlist(lapply(self$get_items(), get_dataset))
    },
    #' @description
    #' Get all datasets and all dataset connectors
    #'
    #' @param dataname (`character` value)\cr
    #'   name of dataset connector to be returned. If `NULL`, all connectors are returned.
    #' @return `list` with all datasets and all connectors
    get_items = function(dataname = NULL) {
      checkmate::assert_string(dataname, null.ok = TRUE)

      if (length(dataname) == 1) {
        if (!(dataname %in% self$get_datanames())) {
          stop(paste("dataset", dataname, "not found"))
        }
        return(private$datasets[[dataname]])
      } else {
        return(private$datasets)
      }
    },
    #' @field id String used to create unique GUI elements
    id = NULL,
    #' @description
    #' Check if dataset has already been pulled.
    #'
    #' @return `TRUE` if dataset has been already pulled, else `FALSE`
    is_pulled = function() {
      all(vapply(private$datasets, is_pulled, logical(1)))
    },
    #' @description
    #' Mutate data by code. Code used in this mutation is not linked to particular
    #' but refers to all datasets.
    #' Consequence of this is that when using `get_code(<dataset>)` this
    #' part of the code will be returned for each specified dataset. This method
    #' should be used only if particular call involve changing multiple datasets.
    #' Otherwise please use `mutate_dataset`.
    #' Execution of `mutate_code` is delayed after datasets are pulled
    #' (`isTRUE(is_pulled)`).
    #'
    #' @param code (`character`) Code to mutate the dataset. Must contain the
    #'  `dataset$dataname`
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
    #' @return self invisibly for chaining
    mutate = function(code, vars = list()) {
      private$set_mutate_vars(vars)
      private$set_mutate_code(
        code = code,
        deps = names(vars)
      )
      private$check_result <- NULL
      logger::log_trace(
        sprintf(
          "TealDataAbstract$mutate code (%s lines) and vars (%s) set.",
          length(parse(text = code, keep.source = FALSE)),
          paste(names(vars), collapse = ", ")
        )
      )
      return(invisible(self))
    },
    #' @description
    #' Mutate dataset by code.
    #' Execution of `mutate_code` is delayed after datasets are pulled
    #' (`isTRUE(is_pulled)`).
    #'
    #' @param dataname (`character`) Dataname to be mutated
    #' @param code (`character`) Code to mutate the dataset. Must contain the
    #'  `dataset$dataname`
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
    #' @return self invisibly for chaining
    mutate_dataset = function(dataname, code, vars = list()) {
      checkmate::assert_character(dataname, min.len = 1, any.missing = FALSE)
      stopifnot(all(dataname %in% self$get_datanames()))

      private$set_mutate_vars(vars = vars)
      private$set_mutate_code(
        code = code,
        dataname = dataname,
        deps = names(vars)
      )

      private$check_result <- NULL
      logger::log_trace(
        sprintf(
          "TealDataAbstract$mutate code (%s lines) and vars (%s) set for dataset: %s.",
          length(parse(text = code, keep.source = FALSE)),
          paste(names(vars), collapse = ", "),
          dataname
        )
      )

      return(invisible(self))
    },
    #' @description
    #' Set reproducibility check
    #'
    #' @param check (`logical`) whether to perform reproducibility check.
    #'
    #' @return (`self`) invisibly for chaining.
    set_check = function(check = FALSE) {
      checkmate::assert_flag(check)
      private$.check <- check
      logger::log_trace("TealDataAbstract$set_check check set to: { check }.")
      return(invisible(self))
    },
    #' @description
    #' Set pull code
    #'
    #' @param code (`character` value)\cr
    #'   code to reproduce `data` in `TealDataset` objects. Can't be set if any dataset
    #'   has `code` set already.
    #'
    #' @return (`self`) invisibly for chaining.
    set_pull_code = function(code) {
      checkmate::assert_string(code)
      is_code_set <- vapply(
        self$get_items(),
        function(item) {
          get_code(item, deparse = TRUE) != ""
        },
        logical(1)
      )

      is_dataset <- vapply(
        self$get_items(),
        function(item) {
          is(item, "TealDataset")
        },
        logical(1)
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
      logger::log_trace("TealDataAbstract$set_pull_code pull code set.")

      return(invisible(self))
    },

    #' @description
    #' Reassign `vars` in `TealDataset` and `TealDatasetConnector` objects
    #' to keep the valid reference after deep cloning
    #' For example if `TealDatasetConnector` has a dependency on some `TealDataset`, this
    #' `TealDataset` is reassigned inside of `TealDatasetConnector`.
    reassign_datasets_vars = function() {
      for (dataset in self$get_items()) {
        dataset$reassign_datasets_vars(
          datasets = self$get_items()
        )
      }
      logger::log_trace("TealDataAbstract$reassign_datasets_vars reassigned vars.")
      invisible(NULL)
    }
  ),

  ## __Private Fields ====
  private = list(
    datasets = NULL,
    .check = FALSE,
    check_result = NULL, # TRUE / FALSE after calling check()
    mutate_code = NULL, # CodeClass after initialization
    mutate_vars = list(), # named list with vars used to mutate object
    pull_code = NULL, # CodeClass - code to reproduce loading of TealDataset(s) only

    ## __Private Methods ====
    # need to have a custom deep_clone because one of the key fields are reference-type object
    # in particular: datasets is a list of R6 objects that wouldn't be cloned using default clone(deep = T)
    deep_clone = function(name, value) {
      deep_clone_r6(name, value)
    },
    check_combined_code = function() {
      execution_environment <- new.env(parent = parent.env(globalenv()))
      self$get_code_class(only_pull = TRUE)$eval(envir = execution_environment)
      res <- all(vapply(
        Filter(is_pulled, self$get_items()),
        function(dataset) {
          data <- get_raw_data(dataset)
          data_from_code <- get(get_dataname(dataset), execution_environment)
          identical(data, data_from_code)
        },
        logical(1)
      ))
      logger::log_trace("TealDataAbstract$check_combined_code reproducibility result of the combined code: { res }.")
      res
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
      checkmate::assert(
        checkmate::check_character(code, max.len = 1, any.missing = FALSE),
        checkmate::check_class(code, "PythonCodeClass")
      )

      if (inherits(code, "PythonCodeClass")) {
        r <- PythonCodeClass$new()
        r$append(private$mutate_code)
        private$mutate_code <- r

        code <- code$get_code()
      }

      if (length(code) > 0 && code != "") {
        private$mutate_code$set_code(code = code, dataname = dataname, deps = deps)
      }

      return(invisible(self))
    },
    set_mutate_vars = function(vars) {
      checkmate::assert_list(vars, min.len = 0, names = "unique")
      if (length(vars) > 0) {
        private$mutate_vars <- c(
          private$mutate_vars,
          vars[!names(vars) %in% names(private$mutate_vars)]
        )
      }

      return(invisible(self))
    },
    check_names = function(x) {
      if (any(vapply(x, identical, logical(1), y = ""))) {
        stop("Cannot extract some dataset names")
      }
      if (any(duplicated(x))) {
        stop("TealDatasets names should be unique")
      }
      if (any(x %in% self$get_datanames())) {
        stop("Some datanames already exists")
      }
      return(TRUE)
    }
  )
)
