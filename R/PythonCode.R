#' Python Dataset Connector
#'
#' `r lifecycle::badge("experimental")`
#' Create a `RelationalDatasetConnector` from `.py` file
#' or through python code supplied directly.
#' @md
#'
#' @details
#'   Note that in addition to the `reticulate` package, support for python requires an
#'   existing python installation. By default, `reticulate` will attempt to use the
#'   location `Sys.which("python")`, however the path to the python installation can be
#'   supplied directly via `reticulate::use_python`.
#'
#'   The `teal` API for delayed data requires the python code or script to return a
#'   data.frame object. For this, the `pandas` package is required. This can be installed
#'   using `reticulate::py_install("pandas")`.
#'
#'   Please see the package documentation for more details.
#'
#' @inheritParams relational_dataset_connector
#' @inheritParams code_dataset_connector
#' @param file (`character`)\cr
#'   Path to the file location containing the python script used to generate the object.
#' @param code (`character`)\cr
#'   string containing the python code to be run using `reticulate`. Carefully consider
#'   indentation to follow proper python syntax.
#' @param object (`character`)\cr
#'   name of the object from the python script that is assigned to the dataset to be used.
#'
#' @note
#'   Raises an error when passed `code` and `file` are passed at the same time.
#'
#'   When using `code`, keep in mind that when using `reticulate` with delayed data, python
#'   functions do not have access to other objects in the `code` and must be self contained.
#'   In the following example, the function `makedata()` doesn't have access to variable `x`:
#'
#' \preformatted{import pandas as pd
#'
#' x = 1
#' def makedata():
#'   return pd.DataFrame({'x': [x, 2], 'y': [3, 4]})
#'
#' data = makedata()}
#'
#'   When using custom functions, the function environment must be entirely self contained:
#'
#' \preformatted{def makedata():
#'   import pandas as pd
#'   x = 1
#'   return pd.DataFrame({'x': [x, 2], 'y': [3, 4]})
#'
#' data = makedata()
#'   }
#'
#'   **Additional `reticulate` considerations:**
#'   1. Note that when using pull `vars`, `R` objects  referenced in the python
#'   code or script have to be prefixed with `r.`.
#'   2. `reticulate` isn't able to convert `POSIXct` objects. Please take extra
#'   care when working with `datetime` variables.
#'
#'   Please read the official documentation for the `reticulate` package for additional
#'   features and current limitations.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(reticulate)
#'
#' # supply python code directly in R
#'
#' x <- python_dataset_connector(
#'   "ADSL",
#'   code = "import pandas as pd
#' data = pd.DataFrame({'STUDYID':  [1, 2], 'USUBJID': [3, 4]})",
#'   object = "data",
#'   keys = get_cdisc_keys("ADSL")
#'   )
#'
#' x$pull()
#' x$get_raw_data()
#'
#' # supply an external python script
#'
#' python_file <- tempfile(fileext = ".py")
#' writeLines(
#'   text = "import pandas as pd
#' data = pd.DataFrame({'STUDYID':  [1, 2], 'USUBJID': [3, 4]})",
#'   con = python_file
#' )
#'
#' x <- python_dataset_connector(
#'   "ADSL",
#'   file = python_file,
#'   object = "data",
#'   keys = get_cdisc_keys("ADSL")
#'   )
#'
#' x$pull()
#' x$get_raw_data()
#'
#' # supply pull `vars` from R
#'
#' y <- 8
#' x <- python_dataset_connector(
#' "ADSL",
#' code = "import pandas as pd
#' data = pd.DataFrame({'STUDYID':  [r.y], 'USUBJID': [r.y]})",
#' object = "data",
#' keys = get_cdisc_keys("ADSL"),
#' vars = list(y = y)
#' )
#'
#' x$pull()
#' x$get_raw_data()
#' }
python_dataset_connector <- function(dataname,
                                     file,
                                     code,
                                     object,
                                     keys,
                                     mutate_code = character(0),
                                     mutate_script = character(0),
                                     label = character(0),
                                     vars = list()) {
  stopifnot(is_character_single(object))
  if (!xor(missing(code), missing(file))) stop("Exactly one of 'code' and 'script' is required")

  if (!missing(file)) {
    stop_if_not(
      is_character_single(file),
      list(file.exists(file), paste("File", file, "does not exist")),
      endsWith(file, ".py")
    )

    x_fun <- CallablePythonCode$new("py_run_file") # nolint
    x_fun$set_args(list(file = file, local = TRUE))

  } else {
    stopifnot(is_character_single(code))

    x_fun <- CallablePythonCode$new("py_run_string") # nolint
    x_fun$set_args(list(code = code, local = TRUE))
  }

  x_fun$set_object(object)

  x <- relational_dataset_connector(
    dataname = dataname,
    pull_callable = x_fun,
    keys = keys,
    code = code_from_script(mutate_code, mutate_script),
    label = label,
    vars = vars
  )

  return(x)
}

## CallablePythonCode ====
#' A `CallablePythonCode` class of objects
#' @md
#'
#' @importFrom R6 R6Class
CallablePythonCode <- R6::R6Class( #nolint

  ## __Public Methods ====
  classname = "CallablePythonCode",
  inherit = CallableFunction,

  public = list(
    #' @description
    #' Create a new `CallablePythonCode` object
    #' @md
    #'
    #' @param fun (`function`)\cr
    #'  function to be evaluated in class. Function should be named
    #'
    #' @return new `CallablePythonCode` object
    initialize = function(fun) {
      super$initialize(fun = fun, env = .GlobalEnv)

      return(invisible(self))
    },
    #' @description
    #'   For scripts and code that contain multiple objects, save the name
    #'   of the object that corresponds to the final dataset of interest.
    #'   This is required for running python scripts with `reticulate`.
    #' @md
    #'
    #' @param x (`character`) the name of the object produced by the code
    #'   or script.
    #'
    #' @return (`self`) invisibly for chaining.
    set_object = function(x) {
      private$object <- x
      private$refresh()

      return(invisible(self))
    },
    #' @description
    #'   Assigns `x <- value` object to `.GlobalEnv`.
    #' @details
    #'  Assignment is done to the global environment. Any variables that
    #'  are overwritten are saved in `private$duplicate_vars` and restored
    #'  after `$run`.
    #' @md
    #'
    #' @param x (`character` value)\cr
    #'  name of the variable in class environment
    #' @param value (`data.frame`)\cr
    #'  object to be assigned to `x`
    #'
    #' @return (`self`) invisibly for chaining.
    assign_to_env = function(x, value) {

      if (exists(x, .GlobalEnv)) {
        private$duplicate_vars[[x]] <- get(x, envir = .GlobalEnv)
      }

      private$vars_to_assign[[x]] <- value

      return(invisible(self))
    },
    #' @description
    #'   Execute `Callable` python code.
    #' @md
    #'
    #' @param return (`logical`)\cr
    #'  whether to return an object
    #' @param args (`NULL` or named `list`)\cr
    #'  supplied for callable functions only, these are dynamic arguments passed to
    #'  `reticulate::py_run_string` or `reticulate::py_run_file`. Dynamic arguments
    #'  are executed in this call and are not saved which means that `self$get_call()`
    #'  won't include them later.
    #' @param try (`logical` value)\cr
    #'  whether perform function evaluation inside `try` clause
    #'
    #' @return nothing or output from function depending on `return`
    #' argument. If `run` fails it will return object of class `simple-error` error
    #' when `try = TRUE` or will stop if `try = FALSE`.
    run = function(return = TRUE, args = NULL, try = FALSE) {
      on.exit({
        # clean up environment if global env was used
        # remove all newly assigned vars
        for (vars_idx in seq_along(private$vars_to_assign)) {
          var_name <- names(private$vars_to_assign)[[vars_idx]]
          rm(list = var_name, envir = .GlobalEnv)
        }

        # reassign duplicate vars
        for (idx in seq_along(private$duplicate_vars)) {
          var_name <- names(private$duplicate_vars)[idx]
          var_value <- private$duplicate_vars[[idx]]
          assign(var_name, var_value, envir = private$env)
        }
      })

      # for execution of pull function, assign pull vars to .GlobalEnv
      for (var in names(private$vars_to_assign)) {
        assign(var, private$vars_to_assign[[var]], envir = private$env)
      }

      super$run(return = return, args = args, try = try)
    }
  ),

  ## __Private Fields ====
  private = list(
    object = NULL,
    duplicate_vars = list(), # variables that already exist in the global env but were supplied as pull vars
    vars_to_assign = list(), # during $run, these variables will be temporarily assigned to the .GlobalEnv
    ## __Private Methods ====
    # @description
    # Refresh call with function name and saved arguments
    #
    # @return nothing
    refresh = function() {
      # replaced str2lang found at:
      # https://rlang.r-lib.org/reference/call2.html
      private$call <- as.call(
        c(rlang::parse_expr(private$fun_name), private$args)
      )

      private$call <- rlang::parse_expr(
        sprintf("%s[[%s]]", pdeparse(private$call), pdeparse(private$object))
        )
    }
  )
)
## PythonCodeClass ====
#' A `CallablePythonCode` class of objects
#' @md
#'
#' @importFrom R6 R6Class
PythonCodeClass <- R6::R6Class( # nolint
  classname = "PythonCodeClass",
  inherit = CodeClass,

  ## __Public Methods ====
  public = list(
    #' @description
    #'   Evaluates internal code within global environment
    #' @md
    #'
    #' @param vars (named `list`) additional pre-requisite vars to execute code
    #' @param dataname (`character`) name of the data frame object to be returned
    #' @param envir (`environment`) environment in which code will be evaluated
    #'
    #' @return `data.frame` containing the mutated dataset
    eval = function(vars = list(), dataname = NULL, envir = .GlobalEnv) {
      stopifnot(is_fully_named_list(vars))

      execution_environment <- .GlobalEnv

      dupl_vars <- list() # only if using global environment

      # set up environment for execution
      for (vars_idx in seq_along(vars)) {
        var_name <- names(vars)[[vars_idx]]
        var_value <- vars[[vars_idx]]
        if (is(var_value, "RawDatasetConnector") || is(var_value, "RawDataset")) {
          var_value <- get_raw_data(var_value)
        }

        if (var_name %in% ls(execution_environment)) {
          dupl_vars[[var_name]] <- get(var_name, envir = execution_environment)
        }
        assign(envir = execution_environment, x = var_name, value = var_value)
      }

      on.exit({
        # clean up environment if global env was used
        # remove all newly assigned vars
        for (vars_idx in seq_along(vars)) {
          var_name <- names(vars)[[vars_idx]]
          rm(list = var_name, envir = execution_environment)
        }

        # reassign duplicate vars
        for (idx in seq_along(dupl_vars)) {
          var_name <- names(dupl_vars)[idx]
          var_value <- dupl_vars[[idx]]
          if (is(var_value, "RawDatasetConnector") || is(var_value, "RawDataset")) {
            var_value <- get_raw_data(var_value)
          }
          assign(var_name, var_value, envir = execution_environment)
        }
      })

      # execute
      super$eval(envir = execution_environment)

      # return early if only executing and not grabbing the dataset
      if (is.null(dataname)) return(as.environment(as.list(execution_environment)))

      if (!is.data.frame(execution_environment[[dataname]])) {
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

      new_set <- execution_environment[[dataname]]

      return(new_set)
    }
  )
)

#' Python Code
#'
#' `r lifecycle::badge("experimental")`
#' Create a python code object directly from python code or a
#' script containing python code.
#' @md
#'
#' @details
#'   Used to mutate dataset connector objects with python code. See
#'   [`mutate_dataset`] or [`mutate_data`] for details.
#'
#' @param code (`character`)\cr
#'   Code to mutate the dataset. Must contain the `dataset$dataname`.
#' @param script (`character`)\cr
#'   file that contains python Code that can be read using `reticulate::py_run_script`.
#'
#' @return (`PythonCodeClass`) object containing python code
#' @export
#'
#' @examples
#' \dontrun{
#' library(random.cdisc.data)
#' library(reticulate)
#'
#' # mutate dataset object
#'
#' ADSL <- radsl(cached = TRUE)
#'
#' x <- rcd_cdisc_dataset_connector("ADSL", radsl, cached = TRUE)
#'
#' x %>% mutate_dataset(python_code("import pandas as pd
#' r.ADSL = pd.DataFrame({'x': [1]})"))
#'
#' x$get_code()
#' x$pull()
#' x$get_raw_data()
#'
#' # mutate data object
#'
#' y <- 8
#' x <- rcd_data( # RelationalDataConnector
#'   rcd_cdisc_dataset_connector("ADSL", radsl, cached = TRUE),
#'   rcd_cdisc_dataset_connector("ADLB", radlb, cached = TRUE)
#'   )
#'
#' tc <- teal:::RelationalData$new(x)
#' tc %>% mutate_data(python_code("import pandas as pd
#' r.ADSL = pd.DataFrame({'STUDYID': [r.y], 'USUBJID': [r.y]})"), vars = list(y = y))
#'
#' load_datasets(tc) # submit all
#' ds <- tc$get_dataset("ADSL")
#' ds$get_raw_data()
#' }
python_code <- function(code = character(0), script = character(0)) {
  if (!xor(missing(code), missing(script))) stop("Exactly one of 'code' and 'script' is required")

  if (!is_empty(script)) {
    code <- deparse(call("py_run_file", script))
  } else {
    code <- deparse(call("py_run_string", code))
  }
  py <- PythonCodeClass$new()
  py$set_code(code)

  return(py)
}
