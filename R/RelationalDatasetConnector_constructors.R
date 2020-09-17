#' Create \code{RawDatasetConnector} object
#'
#' Create \link{RawDatasetConnector} object to execute specific call to fetch data
#' @param pull_fun (\code{CallableFunction})\cr
#'   function with necessary arguments set to fetch data from connection.
#' @examples
#' ds <- raw_dataset_connector(pull_fun = callable_function(data.frame))
#' set_args(ds, list(x = 1:5, y = letters[1:5], stringsAsFactors = FALSE))
#' ds$pull()
#' ds$get_raw_data()
#' ds$get_code()
#' @return \code{RawDatasetConnector} object
#' @export
raw_dataset_connector <- function(pull_fun) {
  stopifnot(is(pull_fun, "CallableFunction"))

  RawDatasetConnector$new(pull_fun = pull_fun)
}

#' Create a new \code{NamedDatasetConnector} object
#'
#' @description
#'  Create \code{NamedDatasetConnector} from \link{callable_function}.
#'
#' @inheritParams raw_dataset_connector
#' @param dataname (\code{character})\cr
#'  A given name for the dataset it may not contain spaces
#'
#' @param code (\code{character})\cr
#'  A character string defining code to modify \code{raw_data} from this dataset. To modify
#'  current dataset code should contain at least one assignment to object defined in \code{dataname}
#'  argument. For example if \code{dataname = ADSL} example code should contain
#'  \code{ADSL <- <some R code>}. Can't be used simultaneously with \code{script}
#'
#' @param script (\code{character})\cr
#'   Alternatively to \code{code} - location of the file containing modification code.
#'   Can't be used simultaneously with \code{script}.
#'
#' @param label (\code{character})\cr
#'  Label to describe the dataset.
#'
#' @param vars (list)\cr
#'   In case when this object code depends on the \code{raw_data} from the other
#'   \code{NamedDataset}, \code{NamedDatasetConnector} object(s) or other constant value,
#'   this/these object(s) should be included.
#'
#' @return new \code{NamedDatasetConnector} object
#'
#' @export
named_dataset_connector <- function(dataname,
                                    pull_fun,
                                    code = character(0),
                                    script = character(0),
                                    label = character(0),
                                    vars = list()) {
  stopifnot(is_character_single(dataname))
  stopifnot(is(pull_fun, "CallableFunction"))
  stopifnot(is_character_empty(code) || is_character_vector(code))
  stopifnot(is_character_empty(label) || is_character_vector(label))

  x <- NamedDatasetConnector$new(
    dataname = dataname,
    pull_fun = pull_fun,
    code = code_from_script(code, script),
    label = label,
    vars = vars
  )

  return(x)
}


#' Create a new \code{RelationalDatasetConnector} object
#'
#' @description
#'  Create \code{RelationalDatasetConnector} from \link{callable_function}.
#'
#' @inheritParams named_dataset_connector
#'
#' @param keys (\code{keys})\cr
#'  object of S3 class keys containing foreign, primary keys and parent information
#'
#' @return new \code{RelationalDatasetConnector} object
#'
#' @export
relational_dataset_connector <- function(dataname,
                                         pull_fun,
                                         keys,
                                         code = character(0),
                                         script = character(0),
                                         label = character(0),
                                         vars = list()) {
  stopifnot(is_character_single(dataname))
  stopifnot(is(pull_fun, "CallableFunction"))
  stopifnot(is(keys, "keys"))
  stopifnot(is_character_empty(code) || is_character_vector(code))
  stopifnot(is_character_empty(label) || is_character_vector(label))

  x <- RelationalDatasetConnector$new(
    dataname = dataname,
    pull_fun = pull_fun,
    keys = keys,
    code = code_from_script(code, script),
    label = label,
    vars = vars
  )

  return(x)
}

#' Load \code{RelationalDatasetConnector} object from a file
#'
#' Please note that the script has to end with a call creating desired object. The error will be raised otherwise.
#'
#' @inheritParams relational_dataset_file
#'
#' @return \code{RelationalDatasetConnector} object
#'
#' @importFrom methods is
#'
#' @rdname relational_dataset_connector
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
#'      pull_fun <- callable_function(radsl)
#'      pull_fun$set_args(list(cached = TRUE))
#'      relational_dataset_connector(\"ADSL\", pull_fun, get_cdisc_keys(\"ADSL\"))"
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

#' @description
#' \code{rcd_dataset_connector} - Create a \code{RelationalDatasetConnector} from any R function.
#'
#' @param fun (\code{function})\cr
#'   any R function which generates \code{data.frame}, especially functions from
#'   \code{random.cdisc.data} like \code{\link[random.cdisc.data]{radsl}}
#'
#' @param ... (\code{optional})\cr
#'   additional arguments applied to pull function
#'
#' @inheritParams relational_dataset_connector
#'
#' @rdname relational_dataset_connector
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
    pull_fun = x_fun,
    keys = keys,
    code = code_from_script(code, script),
    label = label,
    vars = adsl
  )

  return(x)
}


#' @description
#' \code{rds_dataset_connector} - Create a \code{RelationalDatasetConnector} from \code{RDS} file.
#'
#' @param file (\code{character})\cr
#'   path to (\code{.rds} or \code{.R}) that contains \code{data.frame} object or
#'   code to \code{source}
#'
#' @param ... (\code{optional})\cr
#'   additional arguments applied to pull function
#'
#' @inheritParams relational_dataset_connector
#'
#' @rdname relational_dataset_connector
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
    pull_fun = x_fun,
    keys = keys,
    code = code_from_script(code, script),
    label = label
  )

  return(x)
}


#' @description
#' \code{script_dataset_connector} - Create a \code{RelationalDatasetConnector} from \code{.R} file.
#'
#' @param file (\code{character})\cr
#'   file location containing code to be evaluated in connector. Object obtained in the last
#'   call from file will be returned to the connector - same as \code{source(file = file)$value}
#'
#' @param ... (\code{optional})\cr
#'   additional arguments applied to \code{source} function.
#'
#' @inheritParams relational_dataset_connector
#'
#' @rdname relational_dataset_connector
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
    pull_fun = x_fun,
    keys = keys,
    code = code_from_script(code, script),
    label = label,
    vars = vars
  )

  return(x)
}

#' @description
#' \code{code_dataset_connector} - Create a \code{RelationalDatasetConnector}
#'   from a string.
#'
#' @inheritParams relational_dataset_connector
#'
#' @param pull_code (\code{character})\cr
#'   function call with arguments to obtain dataset.
#'
#' @rdname relational_dataset_connector
#'
#' @note Do not include assignment in \code{code} argument. \code{code} string
#'   should be a single function call which returns an object. Use \code{mutate_code}
#'   to make additional transformations.
#'
#' @export
#'
#' @examples
#' library(random.cdisc.data)
#' x <- code_dataset_connector(
#'   dataname = "ADSL",
#'   keys = get_cdisc_keys("ADSL"),
#'   pull_code = "radsl(cached = TRUE)"
#' )
#'
#' x$get_code()
#'
#' mutate_dataset(x, code = "ADSL$new_variable <- 1")
#' x$get_code()
code_dataset_connector <- function(dataname,
                                   pull_code,
                                   keys,
                                   code = character(0),
                                   label = character(0),
                                   ...) {
  vars <- list(...)

  stopifnot(is_fully_named_list(vars))
  stopifnot(is_character_single(pull_code))
  stopifnot(is_character_vector(code, min_length = 0L, max_length = 1L))

  cl <- as.list(str2lang(pull_code))
  fn <- cl[[1]]

  x_fun <- callable_function(fn)
  x_fun$set_args(cl[-1])

  x <- relational_dataset_connector(
    dataname = dataname,
    pull_fun = x_fun,
    keys = keys,
    code = code,
    label = label,
    vars = vars
  )

  return(x)
}

#' @description
#' \code{rice_dataset_connector} -
#' Create a \code{RelationalDatasetConnector} from \code{RICE} dataset.
#'
#' @param path (\code{character})\cr
#'   path to the file
#'
#' @param ... (\code{optional})\cr
#'   additional arguments applied to pull function
#'
#' @inheritParams relational_dataset_connector
#'
#' @rdname relational_dataset_connector
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
    paste0("Connection to entimICE via rice was requested, but rice package is not available.",
           "Please install it from https://github.roche.com/Rpackages/rice."))

  x_fun <- callable_function("rice::rice_read") # nolint
  args <- append(list(node = path, prolong = TRUE, quiet = TRUE), dot_args)
  x_fun$set_args(args)

  x <- relational_dataset_connector(
    dataname = dataname,
    pull_fun = x_fun,
    keys = keys,
    code = code_from_script(code, script),
    label = label
  )

  return(x)
}

#' @description
#' \code{teradata_dataset_connector} -
#' Create a \code{RelationalDatasetConnector} from \code{TERADATA} dataset
#'
#' @inheritParams relational_dataset_connector
#' @param table (\code{character}) table name
#'
#' @rdname relational_dataset_connector
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
    pull_fun = x_fun,
    keys = keys,
    code = code_from_script(code, script),
    label = label
  )

  return(x)
}

#' @description
#' \code{csv_dataset_connector} - Create a \code{RelationalDatasetConnector} from \code{csv} (or
#' general delimited file).
#' @param file (\code{character})\cr
#'   path to (\code{.csv}) (or general delimited) file that contains \code{data.frame} object
#'
#' @param ... (\code{optional})\cr
#'   additional arguments applied to pull function (\code{readr::read_delim}) by default
#'   \code{delim = ","}.
#'
#' @inheritParams relational_dataset_connector
#'
#' @rdname relational_dataset_connector
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
    pull_fun = x_fun,
    keys = keys,
    code = code_from_script(code, script),
    label = label
  )

  return(x)
}



#' @description
#' \code{rds_cdisc_dataset_connector} -
#' Create a \code{RelationalDatasetConnector} from \code{RDS} file with keys automatically
#' assigned by \code{dataname}
#'
#' @inheritParams rds_dataset_connector
#'
#' @rdname relational_dataset_connector
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


#' @description
#' \code{rcd_cdisc_dataset_connector} -
#' Create a \code{DatasetConnector} from any R function with keys assigned automatically
#' by \code{dataname}.
#'
#' @inheritParams rcd_dataset_connector
#'
#' @rdname relational_dataset_connector
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



#' @description
#' \code{rice_cdisc_dataset_connector} -
#' Create a \code{RelationalDatasetConnector} from \code{RICE} dataset with keys assigned
#' automatically by \code{dataname}.
#'
#' @inheritParams rice_dataset_connector
#'
#' @rdname relational_dataset_connector
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


#' @description
#' \code{script_cdisc_dataset_connector} -
#' Create a \code{RelationalDatasetConnector} from \code{script} file with keys assigned
#' automatically by \code{dataname}.
#'
#' @inheritParams script_dataset_connector
#'
#' @rdname relational_dataset_connector
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

#' @description
#' \code{code_cdisc_dataset_connector} -
#' Create a \code{RelationalDatasetConnector} from \code{code} string with keys assigned
#' automatically by \code{dataname}.
#'
#' @inheritParams code_dataset_connector
#'
#' @rdname relational_dataset_connector
#'
#' @export
code_cdisc_dataset_connector <- function(dataname,
                                         pull_code = character(0),
                                         code = character(0),
                                         label = character(0),
                                         ...) {

  x <- code_dataset_connector(
    dataname = dataname,
    pull_code = pull_code,
    keys = get_cdisc_keys(dataname),
    mutate_code = code,
    label = label,
    ...
  )

  return(x)
}

#' @description
#' \code{teradata_cdisc_dataset_connector} -
#' Create a \code{RelationalDatasetConnector} from \code{TERADATA} dataset with keys assigned
#' automatically by \code{dataname}.
#'
#' @inheritParams teradata_dataset_connector
#'
#' @rdname relational_dataset_connector
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

#' @description
#' \code{csv_cdisc_dataset_connector} -
#' Create a \code{RelationalDatasetConnector} from \code{csv} (or general delimited) file
#' with keys assigned automatically by \code{dataname}.
#'
#' @inheritParams csv_dataset_connector
#'
#' @rdname relational_dataset_connector
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


#' @description
#' \code{fun_cdisc_dataset_connector} -
#' Create a \code{RelationalDatasetConnector} from \code{function} and its arguments
#' with keys assigned automatically by \code{dataname}.
#'
#' @inheritParams relational_dataset_connector
#' @param func (\code{function})\cr
#'   function to obtain dataset.
#' @param func_args (\code{list})\cr
#'   additional arguments for (\code{func})\cr.
#' @rdname relational_dataset_connector
#' @importFrom rlang set_env
#' @export
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
#'   x$w <- as.numeric(mvrnorm(40, 0, 1))
#'   x$ww <- as.numeric(mvrnorm(40, 0, 1))
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
#'   x$w <- as.numeric(mvrnorm(40, 0, 1))
#'   x$ww <- as.numeric(mvrnorm(40, 0, 1))
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
#'   x$w <- as.numeric(mvrnorm(40, 0, 1))
#'   x$ww <- as.numeric(mvrnorm(40, 0, 1))
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
fun_cdisc_dataset_connector <- function(dataname,
                                        func,
                                        func_args = NULL,
                                        label = character(0),
                                        code = character(0),
                                        script = character(0),
                                        keys = get_cdisc_keys(dataname),
                                        ...) {

  vars <- list(...)

  stopifnot(is_fully_named_list(vars))

  stopifnot(is.function(func))

  stopifnot(is.list(func_args) || is.null(func_args))

  fun_name <- substitute(func)

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

  eval(bquote(.(fun_name) <- rlang::set_env(.(fun_name), .(ee))), envir = parent.frame())

  x_fun <- CallableFunction$new(fun_name, env = ee)
  x_fun$set_args(func_args)

  vars[[fun_char]] <- get(fun_char, parent.frame())

  x <- relational_dataset_connector(
    dataname = dataname,
    pull_fun = x_fun,
    keys = keys,
    code = code_from_script(code, script),
    label = label,
    vars = vars
  )

  return(x)

}
