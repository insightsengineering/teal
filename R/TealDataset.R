## TealDataset ====
#'
#'
#' @title  R6 Class representing a dataset with its attributes
#'
#' @description
#' Any `data.frame` object can be stored inside this object.
#' Some attributes like colnames, dimension or column names for a specific type will
#' be automatically derived.
#'
#' @rdname dataset_class
#'
#' @param dataname (`character`)\cr
#'  A given name for the dataset it may not contain spaces
#'
#' @param x (`data.frame`)\cr
#'
#' @param keys optional, (`character`)\cr
#'   Vector with primary keys
#'
#' @param code (`character`)\cr
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
TealDataset <- R6::R6Class( # nolint
  "TealDataset",

  ## __Public Methods ====
  public = list(
    #' @description
    #' Create a new object of `TealDataset` class
    initialize = function(dataname,
                          x,
                          keys = character(0),
                          code = character(0),
                          label = character(0),
                          vars = list()) {
      checkmate::assert_string(dataname)
      stopifnot(is.data.frame(x))
      checkmate::assert_character(keys, any.missing = FALSE)
      checkmate::assert(
        checkmate::check_character(code, max.len = 1, any.missing = FALSE),
        checkmate::check_class(code, "CodeClass")
      )
      # label might be NULL also because of taking label attribute from data.frame - missing attr is NULL
      checkmate::assert_character(label, max.len = 1, null.ok = TRUE, any.missing = FALSE)
      stopifnot(is.list(vars))


      private$.raw_data <- x
      private$.ncol <- ncol(x)
      private$.nrow <- nrow(x)
      private$.dim <- c(private$.nrow, private$.ncol)
      private$.colnames <- colnames(x)
      private$.rownames <- rownames(x)
      private$.col_labels <- rtables::var_labels(x)
      private$.row_labels <- c() # not yet defined in rtables

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

      logger::log_trace("TealDataset initialized for dataset: { self$get_dataname() }.")
      return(invisible(self))
    },

    #' @description
    #' Recreate this `TealDataset` with its current attributes.
    #'
    #' @return a new object of the `TealDataset` class
    recreate = function(dataname = self$get_dataname(),
                        x = self$get_raw_data(),
                        keys = self$get_keys(),
                        code = private$code,
                        label = self$get_dataset_label(),
                        vars = list()) {
      res <- self$initialize(
        dataname = dataname,
        x = x,
        keys = keys,
        code = code,
        label = label,
        vars = vars
      )
      logger::log_trace("TealDataset$recreate recreated dataset: { self$get_dataname() }.")
      return(res)
    },
    #' @description
    #' Prints this `TealDataset`.
    #'
    #' @param ... additional arguments to the printing method
    #' @return invisibly self
    print = function(...) {
      check_ellipsis(...)
      cat(sprintf(
        "A TealDataset object containing the following data.frame (%s rows and %s columns):\n",
        private$.nrow,
        private$.ncol
      ))
      print(head(as.data.frame(private$.raw_data)))
      if (private$.nrow > 6) {
        cat("\n...\n")
        print(tail(as.data.frame(private$.raw_data)))
      }
      invisible(self)
    },
    # ___ getters ====
    #' @description
    #' Performs any delayed mutate calls before returning self.
    #'
    #' @return dataset (`TealDataset`)
    get_dataset = function() {
      if (self$is_mutate_delayed() && !private$is_any_dependency_delayed()) {
        private$mutate_eager()
      }
      return(self)
    },
    #' @description
    #' Get all dataset attributes
    #' @return (named `list`) with dataset attributes
    get_attrs = function() {
      x <- append(
        attributes(self$get_raw_data()),
        list(
          column_labels = self$get_column_labels(),
          row_labels = self$get_row_labels(),
          dataname = self$get_dataname(),
          dataset_label = self$get_dataset_label(),
          keys = self$get_keys()
        )
      )
      return(x)
    },
    #' @description
    #' Derive the raw data frame inside this object
    #' @return `data.frame`
    get_raw_data = function() {
      private$.raw_data
    },
    #' @description
    #' Derive the names of all `numeric` columns
    #' @return `character` vector.
    get_numeric_colnames = function() {
      private$get_class_colnames("numeric")
    },
    #' @description
    #' Derive the names of all `character` columns
    #' @return `character` vector.
    get_character_colnames = function() {
      private$get_class_colnames("character")
    },
    #' @description
    #' Derive the names of all `factor` columns
    #' @return `character` vector.
    get_factor_colnames = function() {
      private$get_class_colnames("factor")
    },
    #' @description
    #' Derive the column names
    #' @return `character` vector.
    get_colnames = function() {
      private$.colnames
    },
    #' @description
    #' Derive the column labels
    #' @return `character` vector.
    get_column_labels = function() {
      private$.col_labels
    },
    #' @description
    #' Derive the row names
    #' @return `character` vector.
    get_rownames = function() {
      private$.rownames
    },
    #' @description
    #' Derive the row labels
    #' @return `character` vector.
    get_row_labels = function() {
      private$.row_labels
    },
    #' @description
    #' Derive the `name` which was formerly called `dataname`
    #' @return `character` name of the dataset
    get_dataname = function() {
      private$dataname
    },
    #' @description
    #' Derive the dataname
    #' @return `character` name of the dataset
    get_datanames = function() {
      private$dataname
    },
    #' @description
    #' Derive the `label` which was former called `datalabel`
    #' @return `character` label of the dataset
    get_dataset_label = function() {
      private$dataset_label
    },
    #' @description
    #' Get primary keys of dataset
    #' @return (`character` vector) with dataset primary keys
    get_keys = function() {
      private$.keys
    },
    #' @description
    #' Get `JoinKeys` object with keys used for joining.
    #' @return (`JoinKeys`)
    get_join_keys = function() {
      if (is.null(private$join_keys)) {
        private$join_keys <- join_keys()
      }
      private$join_keys
    },
    #' @description
    #' Returns the string representation of the raw data hashed with the MD5 hash algorithm.
    #' @return `character` the hash of the raw data
    get_hash = function() {
      private$data_hash
    },
    #' @description
    #' Get the list of dependencies that are `TealDataset` or `TealDatasetConnector` objects
    #'
    #' @return `list`
    get_var_r6 = function() {
      return(private$var_r6)
    },
    # ___ setters ====
    #' @description
    #' Overwrites `TealDataset` or `TealDatasetConnector` dependencies of this `TealDataset` with
    #' those found in `datasets`. Reassignment
    #' refers only to the provided `datasets`, other `vars` remains the same.
    #' @details
    #' Reassign `vars` in this object to keep references up to date after deep clone.
    #' Update is done based on the objects passed in `datasets` argument.
    #' Overwrites dependencies with names matching the names of the objects passed
    #' in `datasets`.
    #' @param datasets (`named list` of `TealDataset(s)` or `TealDatasetConnector(s)`)\cr
    #'   objects with valid pointers.
    #' @return NULL invisible
    #' @examples
    #' test_dataset <- teal:::TealDataset$new(
    #'   dataname = "iris",
    #'   x = iris,
    #'   vars = list(dep = teal:::TealDataset$new("iris2", iris))
    #' )
    #' test_dataset$reassign_datasets_vars(
    #'   list(iris2 = teal:::TealDataset$new("iris2", head(iris)))
    #' )
    #'
    reassign_datasets_vars = function(datasets) {
      checkmate::assert_list(datasets, min.len = 0, names = "unique")

      common_var_r6 <- intersect(names(datasets), names(private$var_r6))
      private$var_r6[common_var_r6] <- datasets[common_var_r6]

      common_vars <- intersect(names(datasets), names(private$vars))
      private$vars[common_vars] <- datasets[common_vars]

      common_mutate_vars <- intersect(names(datasets), names(private$mutate_vars))
      private$mutate_vars[common_mutate_vars] <- datasets[common_mutate_vars]

      logger::log_trace("TealDataset$reassign_datasets_vars reassigned vars for dataset: { self$get_dataname() }.")
      invisible(NULL)
    },
    #' @description
    #' Set the label for the dataset
    #' @return (`self`) invisibly for chaining
    set_dataset_label = function(label) {
      if (is.null(label)) {
        label <- character(0)
      }
      checkmate::assert_character(label, max.len = 1, any.missing = FALSE)
      private$dataset_label <- label

      logger::log_trace("TealDataset$set_dataset_label dataset_label set for dataset: { self$get_dataname() }.")
      return(invisible(self))
    },
    #' @description
    #' Set new keys
    #' @return (`self`) invisibly for chaining.
    set_keys = function(keys) {
      checkmate::assert_character(keys, any.missing = FALSE)
      private$.keys <- keys
      logger::log_trace(sprintf(
        "TealDataset$set_keys set the keys %s for dataset: %s",
        paste(keys, collapse = ", "),
        self$get_dataname()
      ))
      return(invisible(self))
    },
    #' @description
    #' set join_keys for a given dataset and object
    #' @param x `list` of `JoinKeySet` objects (which are created using the `join_key` function)
    #' or single `JoinKeySet` objects
    #' @return (`self`) invisibly for chaining
    set_join_keys = function(x) {
      self$get_join_keys()$set(x)
      logger::log_trace("TealDataset$set_join_keys join_keys set for dataset: { self$get_dataname() }.")
      return(invisible(self))
    },
    #' @description
    #' merge input join key with join key inside of object
    #' @param x `list` of `JoinKeys` objects or single `JoinKeys` object
    #' @return (`self`) invisibly for chaining
    merge_join_keys = function(x) {
      self$get_join_keys()$merge(x)
      logger::log_trace("TealDataset$merge_join_keys join_keys merged for dataset: { self$get_dataname() }.")

      return(invisible(self))
    },
    #' @description
    #' mutate the join_keys for a given dataset and object
    #' @param dataset (`character`) dataset for which join_keys are to be set against self
    #' @param val (named `character`) column names used to join
    #' @return (`self`) invisibly for chaining
    mutate_join_keys = function(dataset, val) {
      self$get_join_keys()$mutate(private$dataname, dataset, val)
      logger::log_trace(
        "TealDatasetConnector$mutate_join_keys join_keys modified keys of { self$get_dataname() } against { dataset }."
      )
      return(invisible(self))
    },
    #' @description
    #' Adds variables which code depends on
    #'
    #' @param vars (`named list`) contains any R object which code depends on
    #' @return (`self`) invisibly for chaining
    set_vars = function(vars) {
      private$set_vars_internal(vars, is_mutate_vars = FALSE)
      logger::log_trace("TealDataset$set_vars vars set for dataset: { self$get_dataname() }.")

      return(invisible(NULL))
    },
    #' @description
    #' Sets reproducible code
    #'
    #' @return (`self`) invisibly for chaining
    set_code = function(code) {
      checkmate::assert_character(code, max.len = 1, any.missing = FALSE)

      if (length(code) > 0 && code != "") {
        private$code$set_code(
          code = code,
          dataname = self$get_datanames(),
          deps = names(private$vars)
        )
      }
      logger::log_trace("TealDataset$set_code code set for dataset: { self$get_dataname() }.")
      return(invisible(NULL))
    },

    # ___ get_code ====
    #' @description
    #' Get code to get data
    #'
    #' @param deparse (`logical`) whether return deparsed form of a call
    #'
    #' @return optionally deparsed `call` object
    get_code = function(deparse = TRUE) {
      checkmate::assert_flag(deparse)
      res <- self$get_code_class()$get_code(deparse = deparse)
      return(res)
    },
    #' @description
    #' Get internal `CodeClass` object
    #' @param nodeps (`logical(1)`) whether `CodeClass` should not contain the code
    #' of the dependent `vars`
    #' the `mutate`
    #' @return `CodeClass`
    get_code_class = function(nodeps = FALSE) {
      res <- CodeClass$new()
      # precise order matters
      if (!nodeps) {
        res$append(list_to_code_class(private$vars))
        res$append(list_to_code_class(private$mutate_vars))
      }
      res$append(private$code)
      res$append(private$mutate_list_to_code_class())

      return(res)
    },
    #' @description
    #' Get internal `CodeClass` object
    #'
    #' @return `CodeClass`
    get_mutate_code_class = function() {
      res <- CodeClass$new()
      res$append(list_to_code_class(private$mutate_vars))
      res$append(private$mutate_list_to_code_class())

      return(res)
    },
    #' @description
    #' Get internal `vars` object
    #'
    #' @return `list`
    get_vars = function() {
      return(c(
        private$vars,
        private$mutate_vars[!names(private$mutate_vars) %in% names(private$vars)]
      ))
    },
    #' @description
    #' Get internal `mutate_vars` object
    #'
    #' @return `list`
    get_mutate_vars = function() {
      return(private$mutate_vars)
    },

    #' @description
    #' Whether mutate code has delayed evaluation.
    #' @return `logical`
    is_mutate_delayed = function() {
      return(length(private$mutate_code) > 0)
    },

    # ___ mutate ====
    #' @description
    #' Mutate dataset by code
    #'
    #' @param code (`CodeClass`) or (`character`) R expressions to be executed
    #' @param vars a named list of R objects that `code` depends on to execute
    #' @param force_delay (`logical`) used by the containing `TealDatasetConnector` object
    #'
    #' Either code or script must be provided, but not both.
    #'
    #' @return (`self`) invisibly for chaining
    mutate = function(code, vars = list(), force_delay = FALSE) {
      logger::log_trace(
        sprintf(
          "TealDatasetConnector$mutate mutating dataset '%s' using the code (%s lines) and vars (%s).",
          self$get_dataname(),
          length(parse(text = if (is(code, "CodeClass")) code$get_code() else code, keep.source = FALSE)),
          paste(names(vars), collapse = ", ")
        )
      )

      checkmate::assert_flag(force_delay)
      checkmate::assert_list(vars, min.len = 0, names = "unique")
      checkmate::assert(
        checkmate::check_string(code),
        checkmate::check_class(code, "CodeClass")
      )

      if (inherits(code, "PythonCodeClass")) {
        self$set_vars(vars)
        self$set_code(code$get_code())
        new_df <- code$eval(dataname = self$get_dataname())

        # dataset is recreated by replacing data by mutated object
        # mutation code is added to the code which replicates the data
        self$recreate(
          x = new_df,
          vars = list()
        )
      } else {
        private$mutate_delayed(code, vars)
        if (!(private$is_any_dependency_delayed(vars) || force_delay)) {
          private$mutate_eager()
        }
      }
      logger::log_trace(
        sprintf(
          "TealDataset$mutate mutated dataset '%s' using the code (%s lines) and vars (%s).",
          self$get_dataname(),
          length(parse(text = if (is(code, "CodeClass")) code$get_code() else code, keep.source = FALSE)),
          paste(names(vars), collapse = ", ")
        )
      )

      return(invisible(self))
    },

    # ___ check ====
    #' @description
    #' Check to determine if the raw data is reproducible from the `get_code()` code.
    #' @return
    #' `TRUE` if the dataset generated from evaluating the
    #' `get_code()` code is identical to the raw data, else `FALSE`.
    check = function() {
      logger::log_trace("TealDataset$check executing the code to reproduce dataset: { self$get_dataname() }...")
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
        vars = c(
          list(), # list() in the beginning to ensure c.list
          private$vars,
          setNames(list(self), self$get_dataname())
        )
      )

      res_check <- tryCatch(
        {
          identical(self$get_raw_data(), new_set)
        },
        error = function(e) {
          FALSE
        }
      )
      logger::log_trace("TealDataset$check { self$get_dataname() } reproducibility result: { res_check }.")

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

        duplicates <- get_key_duplicates(self$get_raw_data(), keys)
        if (nrow(duplicates) > 0) {
          stop(
            "Duplicate primary key values found in the dataset '", self$get_dataname(), "' :\n",
            paste0(utils::capture.output(print(duplicates))[-c(1, 3)], collapse = "\n"),
            call. = FALSE
          )
        }
      }
      logger::log_trace("TealDataset$check_keys keys checking passed for dataset: { self$get_dataname() }.")
    },
    #' @description
    #' Check if dataset has already been pulled.
    #'
    #' @return `TRUE` if dataset has been already pulled, else `FALSE`
    is_pulled = function() {
      return(TRUE)
    }
  ),
  ## __Private Fields ====
  private = list(
    .ncol = 0L,
    .nrow = 0L,
    .dim = c(0L, 0L),
    .raw_data = data.frame(),
    .rownames = character(),
    .colnames = character(),
    .col_labels = character(),
    .row_labels = character(),
    dataname = character(0),
    code = NULL, # CodeClass after initialization
    vars = list(),
    var_r6 = list(),
    dataset_label = character(0),
    .keys = character(0),
    mutate_code = list(),
    mutate_vars = list(),
    data_hash = character(0),
    join_keys = NULL,

    ## __Private Methods ====
    mutate_delayed = function(code, vars) {
      private$set_vars_internal(vars, is_mutate_vars = TRUE)
      private$mutate_code[[length(private$mutate_code) + 1]] <- list(code = code, deps = names(vars))
      logger::log_trace(
        sprintf(
          "TealDatasetConnector$mutate_delayed set the code (%s lines) and vars (%s) for dataset: %s.",
          length(parse(text = if (is(code, "CodeClass")) code$get_code() else code, keep.source = FALSE)),
          paste(names(vars), collapse = ", "),
          self$get_dataname()
        )
      )
      return(invisible(self))
    },
    mutate_eager = function() {
      logger::log_trace("TealDatasetConnector$mutate_eager executing mutate code for dataset: { self$get_dataname() }...")
      new_df <- private$execute_code(
        code = private$mutate_list_to_code_class(),
        vars = c(
          list(), # list() in the beginning to ensure c.list
          private$vars,
          # if they have the same name, then they are guaranteed to be identical objects.
          private$mutate_vars[!names(private$mutate_vars) %in% names(private$vars)],
          setNames(list(self), self$get_dataname())
        )
      )

      # code set after successful evaluation
      # otherwise code != dataset
      # private$code$append(private$mutate_code)
      private$append_mutate_code()
      self$set_vars(private$mutate_vars)
      private$mutate_code <- list()
      private$mutate_vars <- list()

      # dataset is recreated by replacing data by mutated object
      # mutation code is added to the code which replicates the data
      # because new_code contains also code of the
      new_self <- self$recreate(
        x = new_df,
        vars = list()
      )

      logger::log_trace("TealDatasetConnector$mutate_eager executed mutate code for dataset: { self$get_dataname() }.")

      new_self
    },

    # need to have a custom deep_clone because one of the key fields are reference-type object
    # in particular: code is a R6 object that wouldn't be cloned using default clone(deep = T)
    deep_clone = function(name, value) {
      deep_clone_r6(name, value)
    },
    get_class_colnames = function(class_type = "character") {
      checkmate::assert_string(class_type)

      return_cols <- private$.colnames[which(vapply(
        lapply(private$.raw_data, class),
        function(x, target_class_name) any(x %in% target_class_name),
        logical(1),
        target_class_name = class_type
      ))]

      return(return_cols)
    },
    mutate_list_to_code_class = function() {
      res <- CodeClass$new()
      for (mutate_code in private$mutate_code) {
        if (is(mutate_code$code, "CodeClass")) {
          res$append(mutate_code$code)
        } else {
          res$set_code(
            code = mutate_code$code,
            dataname = private$dataname,
            deps = mutate_code$deps
          )
        }
      }
      return(res)
    },
    append_mutate_code = function() {
      for (mutate_code in private$mutate_code) {
        if (is(mutate_code$code, "CodeClass")) {
          private$code$append(mutate_code$code)
        } else {
          private$code$set_code(
            code = mutate_code$code,
            dataname = private$dataname,
            deps = mutate_code$deps
          )
        }
      }
    },
    is_any_dependency_delayed = function(vars = list()) {
      any(vapply(
        c(list(), private$var_r6, vars),
        FUN.VALUE = logical(1),
        FUN = function(var) {
          if (is(var, "TealDatasetConnector")) {
            !var$is_pulled() || var$is_mutate_delayed()
          } else if (is(var, "TealDataset")) {
            var$is_mutate_delayed()
          } else {
            FALSE
          }
        }
      ))
    },

    # Set variables which code depends on
    # @param vars (`named list`) contains any R object which code depends on
    # @param is_mutate_vars (`logical(1)`) whether this var is used in mutate code
    set_vars_internal = function(vars, is_mutate_vars = FALSE) {
      checkmate::assert_flag(is_mutate_vars)
      checkmate::assert_list(vars, min.len = 0, names = "unique")

      total_vars <- c(list(), private$vars, private$mutate_vars)

      if (length(vars) > 0) {
        # not allowing overriding variable names
        over_rides <- names(vars)[vapply(
          names(vars),
          FUN.VALUE = logical(1),
          FUN = function(var_name) {
            var_name %in% names(total_vars) &&
              !identical(total_vars[[var_name]], vars[[var_name]])
          }
        )]
        if (length(over_rides) > 0) {
          stop(paste("Variable name(s) already used:", paste(over_rides, collapse = ", ")))
        }
        if (is_mutate_vars) {
          private$mutate_vars <- c(
            private$mutate_vars[!names(private$mutate_vars) %in% names(vars)],
            vars
          )
        } else {
          private$vars <- c(
            private$vars[!names(private$vars) %in% names(vars)],
            vars
          )
        }
      }
      # only adding dependencies if checks passed
      private$set_var_r6(vars)
      return(invisible(NULL))
    },

    # Evaluate script code to modify data or to reproduce data
    #
    # Evaluate script code to modify data or to reproduce data
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

      if (!is.data.frame(execution_environment[[self$get_dataname()]])) {
        out_msg <- sprintf(
          "\n%s\n\n - Code from %s need to return a data.frame assigned to an object of dataset name.",
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
    },

    # Calculates the MD5 hash of the raw data stored in this TealDataset.
    # @return NULL
    calculate_hash = function() {
      private$data_hash <- digest::digest(self$get_raw_data(), algo = "md5")
      NULL
    },

    # Set the name for the dataset
    # @param dataname (`character`) the new name
    # @return self invisibly for chaining
    set_dataname = function(dataname) {
      check_simple_name(dataname)
      private$dataname <- dataname
      return(invisible(self))
    },
    set_var_r6 = function(vars) {
      checkmate::assert_list(vars, min.len = 0, names = "unique")
      for (varname in names(vars)) {
        var <- vars[[varname]]

        if (is(var, "TealDatasetConnector") || is(var, "TealDataset")) {
          var_deps <- var$get_var_r6()
          var_deps[[varname]] <- var
          for (var_dep_name in names(var_deps)) {
            var_dep <- var_deps[[var_dep_name]]
            if (identical(self, var_dep)) {
              stop("Circular dependencies detected")
            }
            private$var_r6[[var_dep_name]] <- var_dep
          }
        }
      }
      return(invisible(self))
    }
  ),
  ## __Active Fields ====
  active = list(
    #' @field ncol Number of columns
    ncol = function() {
      private$.ncol
    },
    #' @field nrow Number of rows
    nrow = function() {
      private$.nrow
    },
    #' @field dim Dimension `c(x, y)`
    dim = function() {
      private$.dim
    },
    #' @field colnames The column names of the data
    colnames = function() {
      private$.colnames
    },
    #' @field rownames The rownames of the data
    rownames = function() {
      private$.rownames
    },
    #' @field raw_data The data.frame behind this R6 class
    raw_data = function() {
      private$.raw_data
    },
    #' @field data The data.frame behind this R6 class
    data = function() {
      private$.raw_data
    },
    #' @field var_names The column names of the data
    var_names = function() {
      private$.colnames
    },
    #' @field row_labels Row labels (can have spaces)
    row_labels = function() {
      private$.row_labels
    }
  )
)

## Constructors ====

#' Constructor for [`TealDataset`] class
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @param dataname (`character`)\cr
#'  A given name for the dataset it may not contain spaces
#'
#' @param x (`data.frame` or `MultiAssayExperiment`)\cr
#'
#' @param keys optional, (`character`)\cr
#'   vector with primary keys
#'
#' @param code (`character`)\cr
#'   A character string defining the code needed to produce the data set in `x`
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
#' @return [`TealDataset`] object
#'
#' @rdname dataset
#'
#' @export
#'
#' @examples
#' # Simple example
#' dataset("iris", iris)
#'
#' # Example with more arguments
#' library(scda)
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#' ADSL_dataset <- dataset(dataname = "ADSL", x = ADSL)
#'
#' ADSL_dataset$get_dataname()
#'
#' ADSL_dataset <- dataset(
#'   dataname = "ADSL",
#'   x = ADSL,
#'   label = "AdAM subject-level dataset",
#'   code = "ADSL <- synthetic_cdisc_data(\"latest\")$adsl"
#' )
#'
#' ADSL_dataset$get_dataset_label()
#' ADSL_dataset$get_code()
dataset <- function(dataname,
                    x,
                    keys = character(0),
                    label = data_label(x),
                    code = character(0),
                    vars = list()) {
  UseMethod("dataset", x)
}

#' @rdname dataset
#' @export
dataset.data.frame <- function(dataname,
                               x,
                               keys = character(0),
                               label = data_label(x),
                               code = character(0),
                               vars = list()) {
  checkmate::assert_string(dataname)
  stopifnot(is.data.frame(x))
  checkmate::assert(
    checkmate::check_character(code, max.len = 1, any.missing = FALSE),
    checkmate::check_class(code, "CodeClass")
  )
  checkmate::assert_list(vars, min.len = 0, names = "unique")

  TealDataset$new(
    dataname = dataname,
    x = x,
    keys = keys,
    code = code,
    label = label,
    vars = vars
  )
}

#' Load `TealDataset` object from a file
#'
#' @description `r lifecycle::badge("experimental")`
#' Please note that the script has to end with a call creating desired object. The error will be raised otherwise.
#'
#' @param path (`character`) string giving the pathname of the file to read from.
#' @param code (`character`) reproducible code to re-create object
#'
#' @return `TealDataset` object
#'
#' @export
#'
#' @examples
#' # simple example
#' file_example <- tempfile(fileext = ".R")
#' writeLines(
#'   text = c(
#'     "library(teal)
#'      dataset(dataname = \"iris\",
#'              x = iris,
#'              code = \"iris\")"
#'   ),
#'   con = file_example
#' )
#' x <- dataset_file(file_example, code = character(0))
#' get_code(x)
#'
#' # custom code
#' file_example <- tempfile(fileext = ".R")
#' writeLines(
#'   text = c(
#'     "library(teal)
#'
#'      # code>
#'      x <- iris
#'      x$a1 <- 1
#'      x$a2 <- 2
#'
#'      # <code
#'      dataset(dataname = \"iris_mod\", x = x)"
#'   ),
#'   con = file_example
#' )
#' x <- dataset_file(file_example)
#' get_code(x)
dataset_file <- function(path, code = get_code(path)) {
  object <- object_file(path, "TealDataset")
  object$set_code(code)
  return(object)
}
