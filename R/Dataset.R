## Dataset ====
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @title  R6 Class representing a dataset with its attributes
#'
#' @description
#' Any \code{data.frame} object can be stored inside this object.
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
Dataset <- R6::R6Class( # nolint
  "Dataset",

  ## __Public Methods ====
  public = list(
    #' @description
    #' Create a new object of `Dataset` class
    #'
    initialize = function(dataname,
                          x,
                          keys = character(0),
                          code = character(0),
                          label = character(0),
                          vars = list()) {
      stopifnot(is_character_single(dataname))
      stopifnot(is.data.frame(x))
      stopifnot(is_character_vector(keys, min_length = 0))
      stopifnot(is_character_vector(code, min_length = 0, max_length = 1) || is(code, "CodeClass"))
      # label might be NULL also because of taking label attribute from data.frame - missing attr is NULL
      stopifnot(is.null(label) || is_character_vector(label, min_length = 0, max_length = 1))
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
      private$set_var_r6(vars)
      self$set_vars(vars)
      self$set_dataset_label(label)
      self$set_keys(keys)
      private$mutate_code <- CodeClass$new()
      private$calculate_hash()

      # needed if recreating dataset - we need to preserve code order and uniqueness
      private$code <- CodeClass$new()
      if (is.character(code)) {
        self$set_code(code)
      } else {
        private$code$append(code)
      }

      return(invisible(self))
    },

    #' @description
    #' Recreate this Dataset with its current attributes.
    #'
    #' @return a new object of the `Dataset` class
    recreate = function(dataname = self$get_dataname(),
                        x = self$get_raw_data(),
                        keys = self$get_keys(),
                        code = self$get_code_class(),
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

      return(res)
    },
    #' Prints this Dataset.
    #'
    #' @param ... additional arguments to the printing method
    #' @return invisibly self
    print = function(...) {
      cat(sprintf(
        "A Dataset object containing the following data.frame (%s rows and %s columns):\n",
        private$.nrow,
        private$.ncol
      ))
      print(head(as.data.frame(private$.raw_data)))
      if(private$.nrow > 6) {
        cat("\n...\n")
        print(tail(as.data.frame(private$.raw_data)))
      }
      invisible(self)
    },
    # ___ getters ====
    #' @description
    #' Performs any delayed mutate calls before returning self.
    #'
    #' @return dataset (\code{Dataset})
    get_dataset = function() {
      if (self$is_mutate_delayed()) {
        private$mutate_eager(private$mutate_code)
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
    #' @return
    #' \code{data.frame} or \code{rtable}
    get_raw_data = function() {
      if (self$is_mutate_delayed()) {
        message("There are mutate statements that are delayed. Returned data may (or may not) reflect the mutations.")
      }
      private$.raw_data
    },
    #' @description
    #' Derive the names of all \code{numeric} columns
    #' @return \code{character} vector.
    get_numeric_colnames = function() {
      private$get_class_colnames("numeric")
    },
    #' @description
    #' Derive the names of all \code{character} columns
    #' @return \code{character} vector.
    get_character_colnames = function() {
      private$get_class_colnames("character")
    },
    #' @description
    #' Derive the names of all \code{factor} columns
    #' @return \code{character} vector.
    get_factor_colnames = function() {
      private$get_class_colnames("factor")
    },
    #' @description
    #' Derive the column names
    #' @return \code{character} vector.
    get_colnames = function() {
      private$.colnames
    },
    #' @description
    #' Derive the column labels
    #' @return \code{character} vector.
    get_column_labels = function() {
      private$.col_labels
    },
    #' @description
    #' Derive the row names
    #' @return \code{character} vector.
    get_rownames = function() {
      private$.rownames
    },
    #' @description
    #' Derive the row labels
    #' @return \code{character} vector.
    get_row_labels = function() {
      private$.row_labels
    },
    #' @description
    #' Derive the \code{name} which was formerly called \code{dataname}
    #' @return \code{character} name of the dataset
    get_dataname = function() {
      private$dataname
    },
    #' @description
    #' Derive the dataname
    #' @return \code{character} name of the dataset
    get_datanames = function() {
      private$dataname
    },
    #' @description
    #' Derive the \code{label} which was former called \code{datalabel}
    #' @return \code{character} label of the dataset
    get_dataset_label = function() {
      private$dataset_label
    },
    #' @description
    #' Get primary keys of dataset
    #' @return (\code{character} vector) with dataset primary keys
    get_keys = function() {
      private$.keys
    },
    #' @description
    #' Returns the string representation of the raw data hashed with the MD5 hash algorithm.
    #' @return \code{character} the hash of the raw data
    get_hash = function() {
      private$data_hash
    },
    #' @description
    #' Get the list of dependencies that are Dataset or DatasetConnector objects
    #'
    #' @return \code{list}
    get_var_r6 = function() {
      return(private$var_r6)
    },

    # ___ setters ====
    #' @description
    #' Set the label for the dataset
    #' @return (`self`) invisibly for chaining
    set_dataset_label = function(label) {
      if (is.null(label)) {
        label <- character(0)
      }
      stopifnot(is_character_vector(label, min_length = 0, max_length = 1))
      private$dataset_label <- label
      return(invisible(self))
    },
    #' @description
    #' Set new keys
    #' @return (`self`) invisibly for chaining.
    set_keys = function(keys) {
      stopifnot(is_character_vector(keys, min_length = 0))
      private$.keys <- keys
      return(invisible(self))
    },
    #' @description
    #' Adds variables which code depends on
    #'
    #' @return (`self`) invisibly for chaining
    set_vars = function(vars) {
      private$set_vars_internal(vars, is_mutate_vars = FALSE)
      return(invisible(NULL))
    },
    #' @description
    #' Sets reproducible code
    #'
    #' @return (`self`) invisibly for chaining
    set_code = function(code) {
      stopifnot(is_character_vector(code, 0, 1))

      if (length(code) > 0 && code != "") {
        private$code$set_code(
          code = code,
          dataname = self$get_datanames(),
          deps = names(private$vars)
        )
      }

      return(invisible(NULL))
    },

    # ___ get_code ====
    #' @description
    #' Get code to get data
    #'
    #' @param deparse (\code{logical}) whether return deparsed form of a call
    #'
    #' @return optionally deparsed \code{call} object
    get_code = function(deparse = TRUE) {
      stopifnot(is_logical_single(deparse))
      if (self$is_mutate_delayed()) {
        message("The output includes mutate code that are delayed")
      }
      executed <- self$get_code_class()$get_code(deparse = FALSE)
      delayed <- self$get_mutate_code_class()$get_code(deparse = FALSE)
      res <- c(executed, delayed)
      if (deparse) {
        return(paste(res, collapse = "\n"))
      }
      return(res)
    },
    #' @description
    #' Get internal \code{CodeClass} object
    #'
    #' @return `\code{CodeClass}`
    get_code_class = function() {
      res <- CodeClass$new()
      res$append(list_to_code_class(private$vars))
      res$append(private$code)

      return(res)
    },
    #' @description
    #' Get internal \code{CodeClass} object
    #'
    #' @return `\code{CodeClass}`
    get_mutate_code_class = function() {
      res <- CodeClass$new()
      res$append(list_to_code_class(private$mutate_vars))
      res$append(private$mutate_code)

      return(res)
    },
    #'
    #' @return \code{logical}
    is_mutate_delayed = function() {
      return(!is_empty(private$mutate_code$code))
    },

    # ___ mutate ====
    #' @description
    #' Mutate dataset by code
    #'
    #' Either code or script must be provided, but not both.
    #'
    #' @return (`self`) invisibly for chaining
    mutate = function(code, vars = list()) {
      stopifnot(is_fully_named_list(vars))

      if (inherits(code, "PythonCodeClass")) {
        self$set_vars(vars)
        self$set_code(code$get_code())
        new_df <- code$eval(dataname = self$get_dataname())

        # dataset is recreated by replacing data by mutated object
        # mutation code is added to the code which replicates the data
        # because new_code contains also code of the
        self$recreate(
          x = new_df,
          vars = list()
        )
      } else {
        delay_mutate <- any(vapply(
          c(private$var_r6, vars),
          FUN = function(var) {
            if (is(var, "DatasetConnector")) {
              (!var$is_pulled()) || var$is_mutate_delayed()
            } else if (is(var, "Dataset")) {
              var$is_mutate_delayed()
            } else {
              FALSE
            }
          },
          FUN.VALUE = logical(1)
          )
        )
        if (delay_mutate) {
          private$mutate_delayed(code, vars)
        } else {
          if (!is(code, "CodeClass")) {
            code_container <- CodeClass$new()
            code_container$set_code(
              code = code,
              dataname = self$get_dataname()
            )
          } else {
            code_container <- code
          }
          self$set_vars(vars)
          private$mutate_eager(code_container)
        }
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
        vars = c(private$vars, setNames(list(self), self$get_dataname()))
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

        duplicates <- get_key_duplicates(self$get_raw_data(), keys)
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
    #' Check if dataset has already been pulled.
    #'
    #' @return \code{TRUE} if dataset has been already pulled, else \code{FALSE}
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
    mutate_code = NULL, # CodeClass after initialization
    mutate_vars = list(),
    data_hash = character(0),

    ## __Private Methods ====
    mutate_delayed = function(code, vars) {
      message("Mutation is delayed")
      private$set_vars_internal(vars, is_mutate_vars = TRUE)
      private$mutate_code$set_code(
        code,
        dataname = self$get_dataname()
      )
      return(invisible(self))
    },

    mutate_eager = function(code_container) {
      stopifnot(is(code_container, "CodeClass"))
      new_df <- private$execute_code(
        code = code_container,
        vars = c(private$vars, private$mutate_vars, setNames(list(self), self$get_dataname()))
      )

      # code set after successful evaluation
      # otherwise code != dataset
      self$set_code(code_container$get_code())
      self$set_vars(private$mutate_vars)
      private$mutate_code <- CodeClass$new()
      private$mutate_vars <- list()

      # dataset is recreated by replacing data by mutated object
      # mutation code is added to the code which replicates the data
      # because new_code contains also code of the
      self$recreate(
        x = new_df,
        vars = list()
      )
    },

    # need to have a custom deep_clone because one of the key fields are reference-type object
    # in particular: code is a R6 object that wouldn't be cloned using default clone(deep = T)
    deep_clone = function(name, value) {
      deep_clone_r6(name, value)
    },

    get_class_colnames = function(class_type = "character") {
      stopifnot(is_character_single(class_type))

      return_cols <- private$.colnames[which(vapply(
        lapply(private$.raw_data, class),
        function(x, target_class_name) any(x %in% target_class_name),
        logical(1),
        target_class_name = class_type))]

      return(return_cols)
    },

    set_vars_internal = function(vars, is_mutate_vars = FALSE) {
      stopifnot(is_fully_named_list(vars))

      total_vars <- if (is_mutate_vars) {
        c(private$vars, private$mutate_vars)
      } else {
        private$vars
      }

      if (length(vars) > 0) {
        # now allowing overriding variable names
        over_rides <- names(vars)[vapply(
          names(vars), function(var_name) {
            var_name %in% names(total_vars) &&
              !identical(total_vars[[var_name]], vars[[var_name]])
          },
          FUN.VALUE = logical(1)
        )]
        if (length(over_rides) > 0) {
          stop(paste("Variable name(s) already used:", paste(over_rides, collapse = ", ")))
        }
        if (is_mutate_vars) {
          private$mutate_vars <- c(private$mutate_vars[!names(private$mutate_vars) %in% names(vars)], vars)
        } else {
          private$vars <- c(private$vars[!names(private$vars) %in% names(vars)], vars)
        }
      }
      # only adding dependencies if checks passed
      private$set_var_r6(vars)
      return(invisible(NULL))
    },


    # Evaluate script code to modify data or to reproduce data
    #
    # Evaluate script code to modify data or to reproduce data
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

    # Calculates the MD5 hash of the raw data stored in this Dataset.
    # @return NULL
    calculate_hash = function() {
        private$data_hash <- digest::digest(self$get_raw_data(), algo = "md5")
        NULL
    },

    # Set the name for the dataset
    # @param dataname (\code{character}) the new name
    # @return self invisibly for chaining
    set_dataname = function(dataname) {
      stopifnot(is_character_single(dataname))
      stopifnot(!grepl("\\s", dataname))
      private$dataname <- dataname
      return(invisible(self))
    },
    set_var_r6 = function(vars) {
      stopifnot(is_fully_named_list(vars))
      for (var in vars) {
        if (is(var, "DatasetConnector") || is(var, "Dataset")) {
          for (var_dep in c(var, var$get_var_r6())) {
            if (identical(self, var_dep)) {
              stop("Circular dependencies detected")
            }
          }
          private$var_r6 <- c(private$var_r6, var, var$get_var_r6())
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
    #' @field dim Dimension \code{c(x, y)}
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

#' Constructor for \code{\link{Dataset}} class
#'
#' @description `r lifecycle::badge("experimental")`
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
#'   A character string defining the code needed to produce the data set in \code{x}
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
#' @return \code{\link{Dataset}} object
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
#' library(random.cdisc.data)
#' ADSL <- radsl(cached = TRUE)
#' ADSL_dataset <- dataset(dataname = "ADSL", x = ADSL)
#'
#' ADSL_dataset$get_dataname()
#'
#' ADSL_dataset <- dataset(
#'   dataname = "ADSL",
#'   x = ADSL,
#'   label = "AdAM subject-level dataset",
#'   code = "ADSL <- radsl(cached = TRUE)"
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
  stopifnot(is_character_single(dataname))
  stopifnot(is.data.frame(x))
  stopifnot(is_character_vector(code, min_length = 0, max_length = 1) || is(code, "CodeClass"))
  stopifnot(identical(vars, list()) || is_fully_named_list(vars))

  Dataset$new(
    dataname = dataname,
    x = x,
    keys = keys,
    code = code,
    label = label,
    vars = vars
  )
}

#' @inherit dataset
#' @description `r lifecycle::badge("defunct")`
#' @export
raw_dataset <- function(x) {
  lifecycle::deprecate_stop(
    "0.9.2",
    "teal::raw_dataset()",
    details = "Please use `teal::dataset()` instead"
  )
}

#' @inherit dataset
#' @description `r lifecycle::badge("soft-deprecated")`
#' @export
named_dataset <- function(dataname,
                          x,
                          code = character(0),
                          label = character(0),
                          vars = list()) {
  lifecycle::deprecate_warn(
    "0.9.2",
    "teal::named_dataset()",
    details = "Please use teal::dataset() instead."
  )

  dataset(
    dataname = dataname,
    x = x,
    code = code,
    label = label,
    vars = vars
  )
}

#' @inherit dataset
#' @description `r lifecycle::badge("soft-deprecated")`
#' @export
relational_dataset <- function(dataname,
                               x,
                               keys = character(0),
                               code = character(0),
                               label = character(0),
                               vars = list()) {
  lifecycle::deprecate_warn(
    "0.9.2",
    "teal::relational_dataset()",
    details = "Please use teal::dataset() instead."
  )

  dataset(
    dataname = dataname,
    x = x,
    keys = keys,
    code = code,
    label = label,
    vars = vars
  )
}

#' Load \code{Dataset} object from a file
#'
#' @description `r lifecycle::badge("experimental")`
#' Please note that the script has to end with a call creating desired object. The error will be raised otherwise.
#'
#' @param path (\code{character}) string giving the pathname of the file to read from.
#' @param code (\code{character}) reproducible code to re-create object
#'
#' @return \code{Dataset} object
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
  object <- object_file(path, "Dataset")
  object$set_code(code)
  return(object)
}

#' @inherit dataset_file
#' @description `r lifecycle::badge("soft-deprecated")`
#' @export
named_dataset_file <- function(path, code = get_code(path)) {
  lifecycle::deprecate_warn(
    "0.9.2",
    "teal::named_dataset_file()",
    details = "Please use teal::dataset_file() instead."
  )
  dataset_file(path = path, code = code)
}

#' @inherit dataset_file
#' @description `r lifecycle::badge("soft-deprecated")`
#' @export
relational_dataset_file <- function(path, code = get_code(path)) {
  lifecycle::deprecate_warn(
    "0.9.2",
    "teal::relational_dataset_file()",
    details = "Please use teal::dataset_file() instead."
  )
  dataset_file(path = path, code = code)
}
