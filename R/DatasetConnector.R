#' A \code{DatasetConnector} class of objects
#'
#' Objects of this class stores connection function to single dataset. Note that for some specific connection type
#' (e.g. \code{RAICE} or \code{SAICE}), pre-requisite object of class \code{DataConnection} is required.
#' Data can be pulled via \code{pull} method and returned via \code{get_data} method.
#'
#' @name DatasetConnector
DatasetConnector <- R6::R6Class( #nolint
  # DatasetConnector public ----
  "DatasetConnector",
  public = list(
    #' @description
    #' Create a new \code{DatasetConnector} object
    #'
    #' @return new \code{DatasetConnector} object
    initialize = function() {
    },
    #' @description
    #' Get executed call
    #'
    #' @param deparse (\code{logical}) whether return deparsed form of a call
    #' @param args (\code{NULL} or named \code{list}) additional dynamic arguments for pull function
    #' @param silent (\code{logical}) whether convert all "missing function" errors to messages
    #'
    #' @return optionally deparsed \code{call} object
    get_call = function(deparse = TRUE, args = NULL, silent = FALSE) {
      stopifnot(is_logical_single(deparse))
      if_cond(private$check_pull_fun(silent = silent), return(), isFALSE)
      res <- call("<-", as.name(private$dataname), private$pull_fun$get_call(deparse = FALSE, args = args))
      if (deparse) {
        return(paste0(deparse(res, width.cutoff = 80L), collapse = "\n"))
      } else {
        return(res)
      }
    },
    #' @description
    #' Get the data from connection
    #'
    #' Pull (if it is not already pulled) and return data
    #'
    #' @param args (\code{NULL} or named \code{list}) additional dynamic arguments for pull function
    #' @param silent (\code{logical}) whether convert all "missing function" errors to messages
    #'
    #' @return object returned from connection function
    get_data = function(args = NULL, silent = FALSE) {
      if (!private$is_pulled) {
        self$pull(args = args, silent = silent)
      }

      # preserve old labels if given
      current_l <- self$get_labels()
      default_l <- get_labels(private$data)
      new_l <- current_l

      if (is.null(current_l$dataset_label)) {
        new_l["dataset_label"] <- list(default_l$dataset_label)
      }

      if (is.null(current_l$column_labels)) {
        new_l["column_labels"] <- list(default_l$column_labels)
      } else {
        # preserve old labels if given
        new_l$column_labels <- default_l$column_labels
        idx <- names(current_l$column_labels)[!is.na(current_l$column_labels) & !is.null(current_l$column_labels)]
        new_l$column_labels[idx] <- current_l$column_labels[idx]
      }

      self$set_labels(new_l)

      return(private$data)
    },
    #' @description
    #' Get connection dataname
    #'
    #' @return (\code{character}) connection data dataname
    get_dataname = function() {
      return(private$dataname)
    },
    #' @description
    #' Get keys of the dataset
    #'
    #' @return (\code{list}) of keys
    get_keys = function() {
      return(private$keys)
    },
    #' @description
    #' Get labels of the dataset
    #'
    #' @return (\code{list}) of labels
    get_labels = function() {
      return(private$labels)
    },
    #' @description
    #' Get path of the dataset
    #'
    #' @return (\code{character})
    get_path = function() {
      return(private$path)
    },
    #' @description
    #' Pull the data
    #'
    #' Force execution of connection function
    #'
    #' @param args (\code{NULL} or named \code{list}) additional dynamic arguments for pull function
    #' @param silent (\code{logical}) whether convert all "missing function" errors to messages
    #'
    #' @return nothing, in order to get the data please use \code{get_data} method
    pull = function(args = NULL, silent = FALSE) {
      if_cond(private$check_pull_fun(silent = silent), return(), isFALSE)
      private$data <- private$pull_fun$run(args = args)
      private$is_pulled <- TRUE
      return(invisible(NULL))
    },
    #' @description
    #' Set dataname
    #'
    #' @param dataname (\code{character}) dataname
    #'
    #' @return nothing
    set_dataname = function(dataname) {
      private$dataname <- dataname
      return(invisible(NULL))
    },
    #' @description
    #' Set dataset keys
    #'
    #' @param keys (\code{list}) of keys
    #'
    #' @return nothing
    set_keys = function(keys) {
      stopifnot(is.list(keys))
      stopifnot(all_true(keys, function(x) is.null(x) || is_character_vector(x)))
      stopifnot(all(c("primary", "foreign", "parent") %in% names(keys)))

      if (!is.null(keys$foreign) && is.null(keys$parent) || (is.null(keys$foreign) && !is.null(keys$parent))) {
        stop(dataname, ": Please specify both foreign keys and a parent!")
      }

      private$keys <- keys
      return(invisible(NULL))
    },
    #' @description
    #' Set dataset labels
    #'
    #' @param labels
    #'
    #' @return nothing
    set_labels = function(labels) {
      stopifnot(is.list(labels))
      stopifnot(all_true(labels, function(x) is.null(x) || (is.character(x) || is_character_vector(x))))
      stopifnot(all(c("dataset_label", "column_labels") %in% names(labels)))
      private$labels <- labels
      return(invisible(NULL))
    },
    #' @description
    #' Set connection path
    #'
    #' @param path (\code{character}) path
    #'
    #' @return nothing
    set_path = function(path) {
      stopifnot(is_character_vector(path))
      private$path <- path
      return(invisible(NULL))
    },
    #' @description
    #' Set connection function single argument with value
    #'
    #' @param name (\code{character}) function argument name
    #' @param value value to pass as a \code{name}
    #' @param silent (\code{logical}) whether convert all "missing function" errors to messages
    #'
    #' @return nothing
    set_pull_arg_value = function(name, value, silent = FALSE) {
      if_cond(private$check_pull_fun(silent = silent), return(), isFALSE)
      private$pull_fun$set_arg_value(name, value)
    },
    #' @description
    #' Set connection function arguments with values
    #'
    #' @param args (\code{NULL} or named \code{list}) with values where list names are argument names
    #' @param silent (\code{logical}) whether convert all "missing function" errors to messages
    #'
    #' @return nothing
    set_pull_args = function(args, silent = FALSE) {
      if_cond(private$check_pull_fun(silent = silent), return(), isFALSE)
      private$pull_fun$set_args(args)
    },
    #' @description
    #' Set the pulling function, which itself defines the connection through which the data gets pulled.
    #'
    #' @param fun (\code{CallableFunction}) function to pull the data
    #'
    #'  @return nothing
    set_pull_fun = function(fun) {
      stopifnot(is(fun, "CallableFunction"))
      stop_if_not(list(!is_character_empty(private$dataname), "Please set up dataname before function"))
      private$pull_fun <- fun
      return(invisible(NULL))
    }
  ),
  # DatasetConnector private -----
  private = list(
    path = character(0),
    keys = NULL,
    labels = NULL,
    data = NULL,
    dataname = character(0),
    is_pulled = FALSE,
    pull_fun = NULL, # CallableFunction
    silent = FALSE,
    check_pull_fun = function(silent = FALSE) {
      stopifnot(is_logical_single(silent))
      if (is.null(private$pull_fun)) {
        msg <- "Pull function not set"
        if (silent) {
          message(msg)
          return(FALSE)
        } else {
          stop(msg)
        }
      } else {
        return(TRUE)
      }
    }
  )
)

# DatasetConnector wrappers ----

#' Set up connection to \code{random.cdisc.data}
#'
#' @export
#'
#' @param fun (\code{function}) connection function
#' @param cached (\code{logical}) whether to use cached data
#' @param seed (\code{numeric}) random seed if used non-cached data
#' @inheritParams cdisc_dataset
#'
#' @return (\code{DatasetConnector}) type of object
#'
#' @examples
#' library(random.cdisc.data)
#' x <- rcd_dataset("ADSL", radsl)
#' x$get_call()
#' x$get_data()
rcd_dataset <- function(dataname, fun, cached = TRUE, seed = 1) {
  stopifnot(is_character_single(dataname))
  stopifnot(is.function(fun))
  stopifnot(is_logical_single(cached))
  stopifnot(is_numeric_single(seed))

  x_fun <- CallableFunction$new(fun) # nolint
  x_fun$set_args(list(cached = cached, seed = seed))

  x <- DatasetConnector$new() # nolint
  x$set_dataname(dataname)
  x$set_keys(get_cdisc_keys(dataname))
  x$set_pull_fun(x_fun)

  return(x)
}

#' Set up connection to local \code{rds} file
#'
#' @export
#'
#' @param file (\code{character}) file path
#' @inheritParams cdisc_dataset
#'
#' @return (\code{DatasetConnector}) type of object
#'
#' @examples
#' \dontrun{
#' x <- rds_dataset("ADSL", "/path/to/file.rds")
#' x$get_call()
#' x$get_data()
#' }
rds_dataset <- function(dataname, file) {
  stopifnot(is_character_single(dataname))
  stopifnot(is_character_single(file))

  x_fun <- CallableFunction$new(readRDS) # nolint
  x <- DatasetConnector$new() # nolint

  x$set_dataname(dataname)
  x$set_pull_fun(x_fun)
  x$set_pull_args(list(file = file))
  return(x)
}

#' Set up connection to local \code{RICE} dataset
#'
#' @export
#'
#' @param path (\code{character}) file path
#' @inheritParams cdisc_dataset
#'
#' @return (\code{DatasetConnector}) type of object
#'
#' @examples
#' x <- rice_dataset("ADSL", "/path/to/ADSL")
#' x$get_call()
#' \dontrun{
#' x$get_data()
#' }
rice_dataset <- function(dataname,
                         path,
                         keys = get_cdisc_keys(dataname),
                         labels = list(dataset_label = NULL, column_labels = NULL)) {
  stopifnot(is_character_single(dataname))
  stopifnot(is_character_single(path))
  stopifnot(is.list(keys))
  stopifnot(all_true(keys, function(x) is.null(x) || is_character_vector(x)))
  stopifnot(all(c("primary", "foreign", "parent") %in% names(keys)))
  stopifnot(is.list(labels))
  stopifnot(all_true(labels, function(x) is.null(x) || (is.character(x) || is_character_vector(x))))
  stopifnot(all(c("dataset_label", "column_labels") %in% names(labels)))

  check_pckg_quietly("rice",
                     paste0("Connection to entimICE via rice was requested, but rice package is not available.",
                            "Please install it from https://github.roche.com/Rpackages/rice."))

  x <- DatasetConnector$new() # nolint

  x$set_dataname(dataname)
  x$set_path(path)
  x$set_keys(keys)
  x$set_labels(labels)

  pull_fun <- CallableFunction$new(rice::rice_read) # nolint
  x$set_pull_fun(pull_fun)

  x$set_pull_args(list(node = path, prolong = TRUE, quiet = TRUE))

  return(x)
}
