#' Create a new \code{RelationalDatasetConnector} object
#'
#' @description
#'  Create \code{RelationalDatasetConnector} from \link{callable_function}.
#'
#' @inheritParams named_dataset_connector
#' @inheritParams code_from_script
#'
#' @param keys (\code{keys})\cr
#'  object of S3 class keys containing foreign, primary keys and parent information
#'
#' @return new \code{RelationalDatasetConnector} object
#'
#' @export
relational_dataset_connector <- function(pull_fun,
                                         dataname,
                                         keys,
                                         code = character(0),
                                         label = character(0),
                                         vars = list()) {
  stopifnot(is(pull_fun, "CallableFunction"))
  stopifnot(is_character_single(dataname))
  stopifnot(is(keys, "keys"))
  stopifnot(is_character_empty(code) || is_character_vector(code))
  stopifnot(is_character_empty(label) || is_character_vector(label))

  x <- RelationalDatasetConnector$new(
    pull_fun = pull_fun,
    dataname = dataname,
    keys = keys,
    code = code,
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
#'      relational_dataset_connector(pull_fun, \"ADSL\", get_cdisc_keys(\"ADSL\"))"
#'   ),
#'   con = file_example
#' )
#' x <- relational_dataset_connector_file(file_example)
#' get_code(x)
relational_dataset_connector_file <- function(x) { # nolint
  stopifnot(is_character_single(x))
  stopifnot(file.exists(x))

  lines <- paste0(readLines(x), collapse = "\n")
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
    pull_fun = x_fun,
    dataname = dataname,
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
#'   path to code for \code{source}
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
#' @param mutate_code (\code{character})\cr
#'   Vector with additional code that can be supplied to mutate the dataset.
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
#' x <- code_dataset_connector(
#'   dataname = "ADSL",
#'   keys = get_cdisc_keys("ADSL"),
#'   code = "radsl(cached = TRUE)"
#' )
#'
#' x$get_code()
#'
#' mutate_dataset(x, code = "ADSL$new_variable <- 1")
#' x$get_code()
code_dataset_connector <- function(dataname,
                                   code,
                                   keys,
                                   mutate_code = character(0),
                                   label = character(0),
                                   ...) {
  vars <- list(...)

  stopifnot(is_fully_named_list(vars))
  stopifnot(is_character_single(code))
  stopifnot(is_character_vector(mutate_code, min_length = 0L, max_length = 1L))

  cl <- as.list(str2lang(code))
  fn <- cl[[1]]

  x_fun <- callable_function(fn)
  x_fun$set_args(cl[-1])

  x <- relational_dataset_connector(
    dataname = dataname,
    pull_fun = x_fun,
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
                                         code = character(0),
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
