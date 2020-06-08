#' Create a new \code{RelationalDatasetConnector} object
#'
#' @description
#'  Create \code{RelationalDatasetConnector} from \link{callable_function}.
#'
#' @param pull_fun (\code{CallableFunction})\cr
#'  Set the pulling function \link{CallableFunction} to load \code{data.frame}.
#'  \code{dataname} will be used as name of object to be assigned.
#'
#' @param dataname (\code{character})\cr
#'  A given name for the dataset it may not contain spaces
#'
#' @param keys (\code{keys})\cr
#'  object of S3 class keys containing foreign, primary keys and parent information
#'
#' @param code (\code{character})\cr
#'  A character string defining the code needed to produce the data set in \code{x}
#'
#' @param label (\code{character})\cr
#'  Label to describe the dataset
#'
#' @param ... (optional)\cr
#'   additional arguments passed to function loading data.
#'
#' @return new \code{RawDatasetConnector} object
#' @rdname relational_dataset_connector
#' @export
relational_dataset_connector <- function(pull_fun,
                                         dataname,
                                         keys,
                                         code = character(0),
                                         label = character(0),
                                         ...) {
  stopifnot(is(pull_fun, "CallableFunction"))
  stopifnot(is_character_single(dataname))
  stopifnot(is(keys, "keys"))
  stopifnot(is_character_empty(code) || is_character_vector(code))
  stopifnot(is_character_empty(label) || is_character_vector(label))

  pull_fun$set_args(list(...))

  x <- RelationalDatasetConnector$new(
    pull_fun = pull_fun,
    dataname = dataname,
    keys = keys,
    code = code,
    label = label
  )

  return(x)
}

#' @description
#' \code{rcd_dataset_connector} - Create a \code{RelationalDatasetConnector} from any R function.
#'
#' @param fun (\code{function})\cr
#'   any R function which generates \code{data.frame}, especially functions from
#'   \code{random.cdisc.data} like \code{\link[random.cdisc.data]{radsl}}
#'
#' @inheritParams as_relational
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
#'
#' @rdname relational_dataset_connector
#' @export
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
  x_fun$set_args(dot_args)

  x <- relational_dataset_connector(
    pull_fun = x_fun,
    dataname = dataname,
    keys = keys,
    code = code_from_script(code, script),
    label = label
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
#' @inheritParams as_relational
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
#'
#' @rdname relational_dataset_connector
#' @export
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
  args <- append(list(file = file), dot_args)
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
#' \code{rds_dataset_connector} - Create a \code{RelationalDatasetConnector} from \code{RDS} file.
#'
#' @inheritParams as_relational
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
#'
#' @rdname relational_dataset_connector
#' @export
script_dataset_connector <- function(dataname,
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

  x_fun <- callable_function(source) # nolint
  args <- append(list(file = file), list(...))
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
#' \code{rice_dataset_connector} -
#' Create a \code{RelationalDatasetConnector} from \code{RICE} dataset.
#'
#' @param path (\code{character})\cr
#'   path to the file
#'
#' @inheritParams as_relational
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
#'
#' @rdname relational_dataset_connector
#' @export
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

  x_fun <- callable_function(rice::rice_read) # nolint
  args <- append(list(node = path, prolong = TRUE, quiet = TRUE), list(...))
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
#' @inheritParams script_cdisc_dataset_connector
#'
#' @rdname relational_dataset_connector
#' @export
script_cdisc_dataset_connector <- function(dataname, #nolint
                                           file,
                                           keys,
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
