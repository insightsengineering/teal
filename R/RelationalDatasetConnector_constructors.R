#' Create \code{RawDatasetConnector} object
#'
#' @md
#' @description `r lifecycle::badge("experimental")`
#' Create \link{RawDatasetConnector} object to execute specific call to fetch data
#' @param pull_callable (\code{CallableFunction})\cr
#'   function with necessary arguments set to fetch data from connection.
#' @examples
#' ds <- raw_dataset_connector(pull_callable = callable_function(data.frame))
#' set_args(ds, list(x = 1:5, y = letters[1:5], stringsAsFactors = FALSE))
#' ds$pull()
#' ds$get_raw_data()
#' ds$get_code()
#' @return \code{RawDatasetConnector} object
#' @export
raw_dataset_connector <- function(pull_callable) {
  stopifnot(is(pull_callable, "Callable"))

  RawDatasetConnector$new(pull_callable = pull_callable)
}

#' Create a new \code{NamedDatasetConnector} object
#'
#' @md
#' @description `r lifecycle::badge("experimental")`
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


#' Create a new \code{RelationalDatasetConnector} object
#'
#' @md
#' @description  `r lifecycle::badge("experimental")`
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
    pull_callable = x_fun,
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
    pull_callable = x_fun,
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
    pull_callable = x_fun,
    keys = keys,
    code = code_from_script(code, script),
    label = label,
    vars = vars
  )

  return(x)
}
#' @description
#' \code{code_dataset_connector} - Create a \code{RelationalDatasetConnector}
#'   from a string of code.
#'
#' @inheritParams relational_dataset_connector
#'
#' @param code (\code{character})\cr
#'   String containing the code to produce the object.
#'   The code must end in a call to the object.
#' @param mutate_code (\code{character})\cr
#'   String containing the code used to mutate the object
#'   after it is produced.
#'
#' @rdname relational_dataset_connector
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
    code = mutate_code,
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
    pull_callable = x_fun,
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
    pull_callable = x_fun,
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
#' \code{code_cdisc_dataset_connector} - Create a \code{RelationalDatasetConnector}
#'   from a string of code with keys assigned automatically by \code{dataname}.
#'
#' @inheritParams code_dataset_connector
#'
#' @rdname relational_dataset_connector
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
#' \code{fun_dataset_connector} -
#' Create a \code{RelationalDatasetConnector} from \code{function} and its arguments.
#'
#' @inheritParams relational_dataset_connector
#' @param func (\code{function})\cr
#'   a custom function to obtain dataset.
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

#' @description
#' \code{fun_cdisc_dataset_connector} -
#' Create a \code{RelationalDatasetConnector} from \code{function} and its arguments
#' with keys assigned automatically by \code{dataname}.
#'
#' @inheritParams relational_dataset_connector
#' @param func (\code{function})\cr
#'   a custom function to obtain dataset.
#' @param func_args (\code{list})\cr
#'   additional arguments for (\code{func})\cr.
#' @rdname relational_dataset_connector
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
