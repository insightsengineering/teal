#' Create `RawDatasetConnector` object
#'
#'`r lifecycle::badge("experimental")`
#' Create \link{RawDatasetConnector} object to execute specific call to fetch data
#' @md
#'
#' @param pull_callable (`CallableFunction`)\cr
#'   function with necessary arguments set to fetch data from connection.
#' @examples
#' ds <- raw_dataset_connector(pull_callable = callable_function(data.frame))
#' set_args(ds, list(x = 1:5, y = letters[1:5], stringsAsFactors = FALSE))
#' ds$pull()
#' ds$get_raw_data()
#' ds$get_code()
#' @return `RawDatasetConnector` object
#' @export
raw_dataset_connector <- function(pull_callable) {
  stopifnot(is(pull_callable, "Callable"))

  RawDatasetConnector$new(pull_callable = pull_callable)
}

#' Create a new `NamedDatasetConnector` object
#'
#' `r lifecycle::badge("experimental")`
#' Create `NamedDatasetConnector` from \link{callable_function}.
#' @md
#'
#' @inheritParams raw_dataset_connector
#' @param dataname (`character`)\cr
#'  A given name for the dataset it may not contain spaces
#'
#' @param code (`character`)\cr
#'  A character string defining code to modify `raw_data` from this dataset. To modify
#'  current dataset code should contain at least one assignment to object defined in `dataname`
#'  argument. For example if `dataname = ADSL` example code should contain
#'  `ADSL <- <some R code>`. Can't be used simultaneously with `script`
#'
#' @param script (`character`)\cr
#'   Alternatively to `code` - location of the file containing modification code.
#'   Can't be used simultaneously with `script`.
#'
#' @param label (`character`)\cr
#'  Label to describe the dataset.
#'
#' @param vars (named `list`)\cr
#'   In case when this object code depends on the `raw_data` from the other
#'   `NamedDataset`, `NamedDatasetConnector` object(s) or other constant value,
#'   this/these object(s) should be included. Please note that `vars`
#'   are included to this object as local `vars` and they cannot be modified
#'   within another dataset.
#' @return new `NamedDatasetConnector` object
#'
#' @export
named_dataset_connector <- function(dataname,
                                    pull_callable,
                                    code = character(0),
                                    script = character(0),
                                    label = character(0),
                                    vars = list()) {
  stopifnot(is_character_single(dataname))
  stopifnot(is(pull_callable, "Callable"))
  stopifnot(is_character_empty(code) || is_character_vector(code))
  stopifnot(is_character_empty(label) || is_character_vector(label))

  x <- NamedDatasetConnector$new(
    dataname = dataname,
    pull_callable = pull_callable,
    code = code_from_script(code, script),
    label = label,
    vars = vars
  )

  return(x)
}


#' Create a new `RelationalDatasetConnector` object
#'
#' `r lifecycle::badge("experimental")`
#' Create `RelationalDatasetConnector` from \link{callable_function}.
#' @md
#'
#' @inheritParams named_dataset_connector
#'
#' @param keys (`keys`)\cr
#'  object of S3 class keys containing foreign, primary keys and parent information
#'
#' @return new `RelationalDatasetConnector` object
#'
#' @export
relational_dataset_connector <- function(dataname,
                                         pull_callable,
                                         keys,
                                         code = character(0),
                                         script = character(0),
                                         label = character(0),
                                         vars = list()) {
  stopifnot(is_character_single(dataname))
  stopifnot(is(pull_callable, "Callable"))
  stopifnot(is(keys, "keys"))
  stopifnot(is_character_empty(code) || is_character_vector(code))
  stopifnot(is_character_empty(label) || is_character_vector(label))

  x <- RelationalDatasetConnector$new(
    dataname = dataname,
    pull_callable = pull_callable,
    keys = keys,
    code = code_from_script(code, script),
    label = label,
    vars = vars
  )

  return(x)
}

#' Load `RelationalDatasetConnector` object from a file
#'
#' `r lifecycle::badge("experimental")`
#' Please note that the script has to end with a call creating desired object. The error will
#' be raised otherwise.
#' @md
#'
#' @inheritParams relational_dataset_file
#'
#' @return `RelationalDatasetConnector` object
#'
#' @importFrom methods is
#'
#' @export
#'
#' @examples
#' # simple example
#' file_example <- tempfile(fileext = ".R")
#' writeLines(
#'   text = c(
#'     "library(teal)
#'      library(random.cdisc.data)
#'
#'      pull_callable <- callable_function(radsl)
#'      pull_callable$set_args(list(cached = TRUE))
#'      relational_dataset_connector(\"ADSL\", pull_callable, get_cdisc_keys(\"ADSL\"))"
#'   ),
#'   con = file_example
#' )
#' x <- relational_dataset_connector_file(file_example)
#' get_code(x)
relational_dataset_connector_file <- function(path) { # nolint
  stopifnot(is_character_single(path))
  stopifnot(file.exists(path))

  lines <- paste0(readLines(path), collapse = "\n")
  object <- eval(parse(text = lines))

  if (is(object, "RelationalDatasetConnector")) {
    return(object)
  } else {
    stop("The object returned from the file is not RelationalDatasetConnector object.")
  }
}

#' `RCD` Dataset Connector
#'
#' `r lifecycle::badge("experimental")`
#' Create a `RelationalDatasetConnector` from any R function.
#' @md
#'
#' @param fun (`function`)\cr
#'   any R function which generates `data.frame`, especially functions from
#'   `random.cdisc.data` like \code{\link[random.cdisc.data]{radsl}}
#'
#' @param ... (`optional`)\cr
#'   Additional arguments applied to pull function.
#'   In case when this object code depends on the `raw_data` from the other
#'   `NamedDataset`, `NamedDatasetConnector` object(s) or other constant value,
#'   this/these object(s) should be included. Please note that `vars`
#'   are included to this object as local `vars` and they cannot be modified
#'   within another dataset.
#'
#' @inheritParams relational_dataset_connector
#'
#' @export
#'
#' @examples
#' library(random.cdisc.data)
#' x <- rcd_dataset_connector(
#'   dataname = "ADSL",
#'   fun = radsl,
#'   keys = get_cdisc_keys("ADSL"),
#'   cached = TRUE
#' )
#' x$get_code()
#' load_dataset(x)
#' get_dataset(x)
#' x$get_raw_data()
rcd_dataset_connector <- function(dataname,
                                  fun,
                                  keys,
                                  code = character(0),
                                  script = character(0),
                                  label = character(0),
                                  ...) {
  stopifnot(is.function(fun))

  dot_args <- list(...)
  stopifnot(is_fully_named_list(dot_args))

  x_fun <- callable_function(fun) # nolint

  adsl <- if ("ADSL" %in% names(dot_args)) {
    # ADSL argument to be included in radxxx
    x_fun$set_args(
      c(
        list(ADSL = as.name("ADSL")),
        dot_args[!names(dot_args) %in% "ADSL"]
      )
    )
    dot_args["ADSL"]
  } else {
    x_fun$set_args(dot_args)
    list()
  }

  x <- relational_dataset_connector(
    dataname = dataname,
    pull_callable = x_fun,
    keys = keys,
    code = code_from_script(code, script),
    label = label,
    vars = adsl
  )

  return(x)
}

#' `RDS` Dataset Connector
#'
#' `r lifecycle::badge("experimental")`
#' Create a `RelationalDatasetConnector` from `RDS` file.
#' @md
#'
#' @param file (`character`)\cr
#'   path to (`.rds` or `.R`) that contains `data.frame` object or
#'   code to `source`
#'
#' @param ... (`optional`)\cr
#'   additional arguments applied to \code{\link[base]{readRDS}} function
#'
#' @inheritParams relational_dataset_connector
#'
#' @export
#'
#' @examples
#' \dontrun{
#' x <- rds_dataset_connector(
#'   dataname = "ADSL",
#'   file = "path/to/file.RDS",
#'   keys = get_cdisc_keys("ADSL")
#' )
#' x$get_code()
#' }
rds_dataset_connector <- function(dataname,
                                  file,
                                  keys,
                                  code = character(0),
                                  script = character(0),
                                  label = character(0),
                                  ...) {
  dot_args <- list(...)
  stopifnot(is_fully_named_list(dot_args))

  stopifnot(is_character_single(file))
  if (!file.exists(file)) {
    stop("File ", file, " does not exist.", call. = FALSE)
  }

  x_fun <- callable_function(readRDS) # nolint
  args <- c(list(file = file), dot_args)
  x_fun$set_args(args)

  x <- relational_dataset_connector(
    dataname = dataname,
    pull_callable = x_fun,
    keys = keys,
    code = code_from_script(code, script),
    label = label
  )

  return(x)
}


#' Script Dataset Connector
#'
#' `r lifecycle::badge("experimental")`
#' Create a `RelationalDatasetConnector` from `.R` file.
#' @md
#'
#' @param file (`character`)\cr
#'   file location containing code to be evaluated in connector. Object obtained in the last
#'   call from file will be returned to the connector - same as `source(file = file)$value`
#'
#' @inheritParams relational_dataset_connector
#' @inheritParams rcd_dataset_connector
#'
#' @export
#'
#' @examples
#' \dontrun{
#' x <- script_dataset_connector(
#'   dataname = "ADSL",
#'   file = "path/to/script.R",
#'   keys = get_cdisc_keys("ADSL")
#' )
#' x$get_code()
#' }
script_dataset_connector <- function(dataname,
                                     file,
                                     keys,
                                     code = character(0),
                                     script = character(0),
                                     label = character(0),
                                     ...) {
  vars <- list(...)
  stopifnot(is_fully_named_list(vars))

  stopifnot(is_character_single(file))
  if (!file.exists(file)) {
    stop("File ", file, " does not exist.", call. = FALSE)
  }

  x_fun <- callable_function(source) # nolint
  x_fun$set_args(list(file = file, local = TRUE))

  x <- relational_dataset_connector(
    dataname = dataname,
    pull_callable = x_fun,
    keys = keys,
    code = code_from_script(code, script),
    label = label,
    vars = vars
  )

  return(x)
}
#' Code Dataset Connector
#'
#' `r lifecycle::badge("experimental")`
#' Create a `RelationalDatasetConnector`
#' from a string of code.
#' @md
#'
#' @inheritParams relational_dataset_connector
#' @inheritParams rcd_dataset_connector
#'
#' @param code (`character`)\cr
#'   String containing the code to produce the object.
#'   The code must end in a call to the object.
#' @param mutate_code (`character`)\cr
#'   String containing the code used to mutate the object
#'   after it is produced.
#' @param mutate_script (`character`)\cr
#'   Alternatively to `mutate_code` - location of the file containing modification code.
#'   Can't be used simultaneously with `mutate_script`.
#'
#' @export
#'
#' @examples
#' library(random.cdisc.data)
#' x <- code_dataset_connector(
#'   dataname = "ADSL",
#'   keys = get_cdisc_keys("ADSL"),
#'   code = "ADSL <- radsl(cached = TRUE); ADSL"
#' )
#'
#' x$get_code()
#'
#' mutate_dataset(x, code = "ADSL$new_variable <- 1")
#' x$get_code()
#'
#' file_example <- tempfile(fileext = ".R")
#' writeLines(
#'   text = c(
#'     "seed <- 1; ADSL <- radsl(cached = TRUE, seed = seed)\nADSL"
#'   ),
#'   con = file_example
#' )
#'
#' y <- code_dataset_connector(
#'   dataname = "ADSL",
#'   keys = get_cdisc_keys("ADSL"),
#'   code = paste0(readLines(file_example), collapse = "\n")
#' )
#'
code_dataset_connector <- function(dataname,
                                   code,
                                   keys,
                                   mutate_code = character(0),
                                   mutate_script = character(0),
                                   label = character(0),
                                   ...) {
  vars <- list(...)

  stopifnot(is_fully_named_list(vars))
  stopifnot(is_character_single(code))
  stopifnot(is_character_vector(code, min_length = 0L, max_length = 1L))

  call <- callable_code(code = code)

  x <- relational_dataset_connector(
    dataname = dataname,
    pull_callable = call,
    keys = keys,
    code = code_from_script(mutate_code, mutate_script),
    label = label,
    vars = vars
  )

  return(x)
}

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
#' @export
#'
#' @examples
#' \dontrun{
#' library(reticulate)
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
#' }
python_dataset_connector <- function(dataname,
                                     file,
                                     code,
                                     object,
                                     keys,
                                     mutate_code = character(0),
                                     mutate_script = character(0),
                                     label = character(0)) {
  stopifnot(is_character_single(object))

  if (!missing(file)) {
    stop_if_not(
      is_character_single(file),
      list(file.exists(file), paste("File", file, "does not exist")),
      endsWith(file, ".py")
    )

    x_fun <- callable_function("py_run_file") # nolint
    x_fun$set_args(list(file = file, local = TRUE))

  } else if (!missing(code)) {
    stopifnot(is_character_single(code))

    x_fun <- callable_function("py_run_string") # nolint
    x_fun$set_args(list(code = code, local = TRUE))
  }

  x_fun$set_object(object)

  x <- relational_dataset_connector(
    dataname = dataname,
    pull_callable = x_fun,
    keys = keys,
    code = code_from_script(mutate_code, mutate_script),
    label = label
  )

  return(x)
}

#' Rice Dataset Connector
#'
#' `r lifecycle::badge("experimental")`
#' Create a `RelationalDatasetConnector` from `RICE` dataset.
#' @md
#'
#' @param path (`character`)\cr
#'   path to the file
#'
#' @param ... (`optional`)\cr
#'   additional arguments applied to pull function
#'
#' @inheritParams relational_dataset_connector
#'
#' @export
#'
#' @examples
#' x <- rice_dataset_connector(
#'   dataname = "ADSL",
#'   path = "/path/to/file.sas7bdat",
#'   keys = get_cdisc_keys("ADSL")
#' )
#' x$get_code()
#' \dontrun{
#' load_dataset(x)
#' get_dataset(x)
#' x$get_raw_data()
#' }
rice_dataset_connector <- function(dataname,
                                   path,
                                   keys,
                                   code = character(0),
                                   script = character(0),
                                   label = character(0),
                                   ...) {
  dot_args <- list(...)
  stopifnot(is_fully_named_list(dot_args))
  stopifnot(is_character_single(path))

  check_pkg_quietly(
    "rice",
    paste0(
      "Connection to entimICE via rice was requested, but rice package is not available.",
      "Please install it from https://github.roche.com/Rpackages/rice.")
    )

  x_fun <- callable_function("rice::rice_read") # nolint
  args <- append(list(node = path, prolong = TRUE, quiet = TRUE), dot_args)
  x_fun$set_args(args)

  x <- relational_dataset_connector(
    dataname = dataname,
    pull_callable = x_fun,
    keys = keys,
    code = code_from_script(code, script),
    label = label
  )

  return(x)
}

#' `Teradata` Dataset Connector
#'
#' `r lifecycle::badge("experimental")`
#' Create a `RelationalDatasetConnector` from `Teradata` dataset
#' @md
#'
#' @inheritParams relational_dataset_connector
#' @inheritParams rcd_dataset_connector
#' @param table (`character`) table name
#'
#' @export
teradata_dataset_connector <- function(dataname,
                                       table,
                                       keys,
                                       code = character(0),
                                       script = character(0),
                                       label = character(0),
                                       ...) {
  dot_args <- list(...)
  stopifnot(is_fully_named_list(dot_args))

  check_pkg_quietly(
    "DBI",
    "Connection to Teradata tables was requested, but DBI package is not available."
  )

  x_fun <- callable_function("DBI::dbReadTable")
  args <- append(list(conn = as.name("conn"), name = table), dot_args)
  x_fun$set_args(args)

  x <- relational_dataset_connector(
    dataname = dataname,
    pull_callable = x_fun,
    keys = keys,
    code = code_from_script(code, script),
    label = label
  )

  return(x)
}

#' `csv` Dataset Connector
#'
#' `r lifecycle::badge("experimental")`
#' Create a `RelationalDatasetConnector` from `csv` (or general delimited file).
#' @md
#'
#' @param file (`character`)\cr
#'   path to (`.csv)` (or general delimited) file that contains `data.frame` object
#'
#' @param ... (`optional`)\cr
#'   additional arguments applied to pull function (`readr::read_delim`) by default
#'   `delim = ","`.
#'
#' @inheritParams relational_dataset_connector
#'
#' @export
#'
#' @examples
#' \dontrun{
#' x <- csv_dataset_connector(
#'   dataname = "ADSL",
#'   file = "path/to/file.csv",
#'   keys = get_cdisc_keys("ADSL"),
#'   delim = ",",
#'   col_types = quote(readr::cols(AGE = "i"))
#' )
#' x$get_code()
#' }
csv_dataset_connector <- function(dataname,
                                  file,
                                  keys,
                                  code = character(0),
                                  script = character(0),
                                  label = character(0),
                                  ...) {
  dot_args <- list(...)
  stopifnot(is_fully_named_list(dot_args))

  check_pkg_quietly(
    "readr",
    "library readr is required to use csv connectors please install it."
  )

  # add default delim as ","
  if (!"delim" %in% names(dot_args)) {
    dot_args$delim <- ","
  }

  stopifnot(is_character_single(file))
  if (!file.exists(file)) {
    stop("File ", file, " does not exist.", call. = FALSE)
  }

  x_fun <- callable_function(readr::read_delim) # using read_delim as preserves dates (read.csv does not)
  args <- c(list(file = file), dot_args)
  x_fun$set_args(args)

  x <- relational_dataset_connector(
    dataname = dataname,
    pull_callable = x_fun,
    keys = keys,
    code = code_from_script(code, script),
    label = label
  )

  return(x)
}



#' `RDS` `CDSIC` Dataset Connector
#'
#' `r lifecycle::badge("experimental")`
#' Create a `RelationalDatasetConnector` from `RDS` file with keys automatically
#' assigned by `dataname`
#' @md
#'
#' @inheritParams rds_dataset_connector
#'
#' @export
rds_cdisc_dataset_connector <- function(dataname,
                                        file,
                                        code = character(0),
                                        script = character(0),
                                        label = character(0),
                                        ...) {

  x <- rds_dataset_connector(
    dataname = dataname,
    file = file,
    keys = get_cdisc_keys(dataname),
    code = code_from_script(code, script),
    label = label,
    ...
  )

  return(x)
}


#' `RCD` `CDISC` Dataset Connector
#'
#' `r lifecycle::badge("experimental")`
#' Create a `DatasetConnector` from any R function with keys assigned automatically
#' by `dataname`.
#' @md
#'
#' @inheritParams rcd_dataset_connector
#'
#' @export
rcd_cdisc_dataset_connector <- function(dataname,
                                        fun,
                                        code = character(0),
                                        script = character(0),
                                        label = character(0),
                                        ...) {
  x <- rcd_dataset_connector(
    dataname = dataname,
    fun = fun,
    keys = get_cdisc_keys(dataname),
    code = code,
    script = script,
    label = label,
    ...
  )

  return(x)
}



#' Rice `CDISC` Dataset Connector
#'
#' `r lifecycle::badge("experimental")`
#' Create a `RelationalDatasetConnector` from `RICE` dataset with keys assigned
#' automatically by `dataname`.
#' @md
#'
#' @inheritParams rice_dataset_connector
#'
#' @export
rice_cdisc_dataset_connector <- function(dataname,
                                         path,
                                         code = character(0),
                                         script = character(0),
                                         label = character(0),
                                         ...) {
  x <- rice_dataset_connector(
    dataname = dataname,
    path = path,
    keys = get_cdisc_keys(dataname),
    code = code_from_script(code, script),
    label = label,
    ...
  )

  return(x)
}


#' Script `CDISC` Dataset Connector
#'
#' `r lifecycle::badge("experimental")`
#' Create a `RelationalDatasetConnector` from `script` file with keys assigned
#' automatically by `dataname`.
#' @md
#'
#' @inheritParams script_dataset_connector
#'
#' @export
script_cdisc_dataset_connector <- function(dataname,
                                           file,
                                           code = character(0),
                                           script = character(0),
                                           label = character(0),
                                           ...) {

  x <- script_dataset_connector(
    dataname = dataname,
    file = file,
    keys = get_cdisc_keys(dataname),
    code = code_from_script(code, script),
    label = label,
    ...
  )

  return(x)
}

#' Code `CDISC` Dataset Connector
#'
#' `r lifecycle::badge("experimental")`
#' Create a `RelationalDatasetConnector` from a string of code with keys
#' assigned automatically by `dataname`.
#' @md
#'
#' @inheritParams code_dataset_connector
#' @inheritParams rcd_dataset_connector
#'
#' @export
code_cdisc_dataset_connector <- function(dataname,
                                         code,
                                         mutate_code = character(0),
                                         label = character(0),
                                         ...) {

  x <- code_dataset_connector(
    dataname = dataname,
    code = code,
    keys = get_cdisc_keys(dataname),
    mutate_code = mutate_code,
    label = label,
    ...
  )

  return(x)
}

#' `Teradata` `CDISC` Dataset Connector
#'
#' `r lifecycle::badge("experimental")`
#' Create a `RelationalDatasetConnector` from `Teradata` dataset with keys assigned
#' automatically by `dataname`.
#' @md
#'
#' @inheritParams teradata_dataset_connector
#' @inheritParams rcd_dataset_connector
#'
#' @export
teradata_cdisc_dataset_connector <- function(dataname, # nolint
                                             table,
                                             code = character(0),
                                             script = character(0),
                                             label = character(0),
                                             ...) {

  x <- teradata_dataset_connector(
    dataname = dataname,
    table = table,
    code = code_from_script(code, script),
    keys = get_cdisc_keys(dataname),
    label = label,
    ...
  )

  return(x)
}

#' `csv` `CDISC` Dataset Connector
#'
#' `r lifecycle::badge("experimental")`
#' Create a `RelationalDatasetConnector` from `csv` (or general delimited) file
#' with keys assigned automatically by `dataname`.
#' @md
#'
#' @inheritParams csv_dataset_connector
#'
#' @export
csv_cdisc_dataset_connector <- function(dataname,
                                        file,
                                        code = character(0),
                                        script = character(0),
                                        label = character(0),
                                        ...) {

  x <- csv_dataset_connector(
    dataname = dataname,
    file = file,
    code = code_from_script(code, script),
    keys = get_cdisc_keys(dataname),
    label = label,
    ...
  )

  return(x)
}


#' Function Dataset Connector
#'
#' `r lifecycle::badge("experimental")`
#' Create a `RelationalDatasetConnector` from `function` and its arguments.
#' @md
#'
#' @inheritParams relational_dataset_connector
#' @inheritParams rcd_dataset_connector
#' @param func (`function`)\cr
#'   a custom function to obtain dataset.
#' @param func_args (`list`)\cr
#'   additional arguments for (`func`)\cr.
#'
#' @importFrom rlang set_env
#'
#' @export
#'
#' @examples
#' \dontrun{
#' my_data <- function(...) {
#'   # whatever code
#'   set.seed(1234)
#'   library(MASS)
#'   require(dplyr)
#'   x <- data.frame(
#'     STUDYID = 1,
#'     USUBJID = 1:40,
#'     z = stats::rnorm(40),
#'     zz = factor(sample(letters[1:3], 40, replace = TRUE)),
#'     NAs = rep(NA, 40)
#'   )
#'   x$w <- as.numeric(MASS::mvrnorm(40, 0, 1))
#'   x$ww <- as.numeric(MASS::mvrnorm(40, 0, 1))
#'   rtables::var_labels(x) <- c("STUDYID", "USUBJID", "z", "zz", "NAs", "w", "ww")
#'   x
#' }
#' y <- fun_cdisc_dataset_connector(
#'   dataname = "ADSL",
#'   func = my_data
#' )
#'
#' y$get_code()
#'
#' y$pull()
#'
#' get_raw_data(y)
#' }
#' # Error as global var is used in the function.
#' # Thus not reproducible.
#' \dontrun{
#' x <- 40
#' my_data <- function(global_var = x) {
#'   # whatever code
#'   set.seed(1234)
#'   library(MASS)
#'   x <- data.frame(
#'     STUDYID = 1,
#'     USUBJID = 1:global_var,
#'     z = stats::rnorm(40),
#'     zz = factor(sample(letters[1:3], 40, replace = TRUE)),
#'     NAs = rep(NA, 40)
#'   )
#'   x$w <- as.numeric(MASS::mvrnorm(40, 0, 1))
#'   x$ww <- as.numeric(MASS::mvrnorm(40, 0, 1))
#'   rtables::var_labels(x) <- c("STUDYID", "USUBJID", "z", "zz", "NAs", "w", "ww")
#'   x
#' }
#' y <- fun_cdisc_dataset_connector(
#'   dataname = "ADSL",
#'   func = my_data
#' )
#'
#' y$pull()
#' }
#' # Error - same as previous one
#' \dontrun{
#' global_var <- 40
#' my_data <- function() {
#'   # whatever code
#'   set.seed(1234)
#'   library(MASS)
#'   x <- data.frame(
#'     STUDYID = 1,
#'     USUBJID = 1:global_var,
#'     z = stats::rnorm(40),
#'     zz = factor(sample(letters[1:3], 40, replace = TRUE)),
#'     NAs = rep(NA, 40)
#'   )
#'   x$w <- as.numeric(MASS::mvrnorm(40, 0, 1))
#'   x$ww <- as.numeric(MASS::mvrnorm(40, 0, 1))
#'   rtables::var_labels(x) <- c("STUDYID", "USUBJID", "z", "zz", "NAs", "w", "ww")
#'   x
#' }
#' y <- fun_cdisc_dataset_connector(
#'   dataname = "ADSL",
#'   func = my_data
#' )
#'
#' y$pull()
#' }
#'
fun_dataset_connector <- function(dataname,
                                  func,
                                  func_args = NULL,
                                  label = character(0),
                                  code = character(0),
                                  script = character(0),
                                  keys,
                                  ...) {

  vars <- list(...)

  stopifnot(is_fully_named_list(vars))

  stopifnot(is.function(func))

  stopifnot(is.list(func_args) || is.null(func_args))

  fun_name <- if (length(sys.calls()) == 1) substitute(func) else substitute(func, env = parent.frame())

  cal <- if (!is.symbol(fun_name)) as.call(fun_name) else NULL

  is_pak <- FALSE
  is_locked <- TRUE
  if ((!is.null(cal)) && identical(cal[[1]], as.symbol("::"))) {
    pak <- cal[[2]]
    pak_char <- as.character(pak) #nolint
    library(pak_char, character.only = TRUE)
    fun_name <- cal[[3]]
    is_pak <- TRUE
    is_locked <- TRUE
  } else {
    is_locked <- environmentIsLocked(environment(func))
  }

  fun_char <- as.character(fun_name)

  ee <- new.env(parent = parent.env(globalenv()))

  ee$library <- function(...) {
    mc <- match.call()
    mc[[1]] <- quote(base::library)
    eval(mc, envir = globalenv())
    this_env <- parent.frame()
    if (!identical(this_env, globalenv())) {
      parent.env(this_env) <- parent.env(globalenv())
    }
  }


  if (!is_pak && !is_locked) {
    eval(bquote(.(fun_name) <- get(.(fun_char), .(environment(func)))), envir = ee)
    eval(bquote(.(fun_name) <- rlang::set_env(.(fun_name), .(ee))), envir = ee)
  }

  x_fun <- CallableFunction$new(fun_name, env = ee)
  x_fun$set_args(func_args)

  vars[[fun_char]] <- ee[[fun_char]]

  x <- relational_dataset_connector(
    dataname = dataname,
    pull_callable = x_fun,
    keys = keys,
    code = code_from_script(code, script),
    label = label,
    vars = vars
  )

  return(x)

}

#' Function `CDISC` Dataset Connector
#'
#' `r lifecycle::badge("experimental")`
#' Create a `RelationalDatasetConnector` from `function` and its arguments
#' with keys assigned automatically by `dataname`.
#' @md
#'
#' @inheritParams fun_dataset_connector
#' @inheritParams rcd_dataset_connector
#'
#' @export
fun_cdisc_dataset_connector <- function(dataname,
                                        func,
                                        func_args = NULL,
                                        label = character(0),
                                        code = character(0),
                                        script = character(0),
                                        ...) {

  x <- fun_dataset_connector(
    dataname,
    func,
    func_args = func_args,
    label = label,
    code = code,
    script = script,
    keys = get_cdisc_keys(dataname),
    ...
  )

  return(x)

}
