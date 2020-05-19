#' A \code{RelationalDatasetConnector} class of objects
#'
#' Objects of this class store connection function to single dataset. Note that for some specific
#' connection type (e.g. \code{RAICE} or \code{SAICE}), pre-requisite object of class
#' \code{DataConnection} is required. Data can be pulled via \code{pull} method and accessed directly
#' by \code{dataset} active binding. Pulled data is of class \link{RelationalDataset} and it can be
#' accessed
#'
#' @name RelationalDatasetConnector
RelationalDatasetConnector <- R6::R6Class( #nolint
  # RelationalDatasetConnector public ----
  classname = "RelationalDatasetConnector",
  inherit = RawDatasetConnector,
  public = list(

    #' @description
    #' Create a new \code{RelationalDatasetConnector} object. Set the pulling function
    #' \link{CallableFunction} to load \code{data.frame}. \code{dataname} will be used as name
    #' of object to be assigned.
    #'
    #' @param pull_fun (\code{CallableFunction})\cr
    #'  function to load the data.
    #' @param dataname (\code{character})\cr
    #'  A given name for the dataset it may not contain spaces
    #' @param keys (\code{keys})\cr
    #'  object of S3 class keys containing foreign, primary keys and parent information
    #' @param code (\code{character})\cr
    #'  A character string defining the code needed to produce the data set in \code{x}
    #' @param label (\code{character})\cr
    #'  Label to describe the dataset
    #'
    #' @return new \code{RawDatasetConnector} object
    initialize = function(pull_fun, dataname, keys, code = character(0), label = character(0)) {
      super$initialize(pull_fun = pull_fun)
      # instead of following do private$.dataset %>% as_relational
      private$set_dataname(dataname)
      private$set_keys(keys)
      private$set_code(code)
      self$set_label(label)

      return(self)
    },

    #' @description
    #' Get dataname of dataset
    #'
    #' @return dataname of the dataset
    get_dataname = function() {
      return(private$.dataname)
    },

    #' @description
    #' Get keys of dataset
    #'
    #' @return \code{keys} object
    get_keys = function() {
      return(private$.keys)
    },

    #' @description
    #' Get label of dataset
    #'
    #' @return \code{character}
    get_label = function() {
      return(private$.label)
    },

    #' @description
    #' Get code to get data
    #'
    #' @param deparse (\code{logical}) whether return deparsed form of a call
    #'
    #' @return optionally deparsed \code{call} object
    get_code = function(deparse = TRUE) {
      stopifnot(is_logical_single(deparse))

      pull_code <- private$get_pull_code(deparse)
      mutate_code <- private$get_mutate_code(deparse)

      code <- if (deparse) {
        if (length(mutate_code) == 0) {
          pull_code
        } else {
          sprintf(
            "%s\n%s",
            pull_code,
            mutate_code
          )
        }

      } else {
        append(
          pull_code,
          mutate_code
        )
      }

      return(code)
    },
    #' @description
    #' Mutate dataset by code
    #'
    #' @param code (\code{character}) Code to mutate the dataset. Must contain the
    #'  \code{dataset$dataname}
    #' @param script (\code{character}) file that contains R Code that can
    #'   be read using \link{read_script}. Preferred before \code{code} argument
    mutate_dataset = function(code = character(0), script = character(0)) {
      code <- code_from_script(code, script)
      if (!is.null(private$.dataset)) {
        private$.dataset <- mutate_dataset(private$.dataset, code = code)
      }
      private$set_code(code)

      return(invisible(self))
    },


    #' @description
    #' Pull the data
    #'
    #' Read or create data using \code{pull_fun} specified in the constructor.
    #'
    #' @param args (\code{NULL} or named \code{list})\cr
    #' additional dynamic arguments for pull function. \code{args} can be omitted if \code{pull_fun}
    #' from constructor already contains all necessary arguments to pull data. One can try
    #' to execute \code{pull_fun} directly by \code{x$pull_fun$run()} or to get code using
    #' \code{x$pull_fun$get_code()}. \code{args} specified in pull are used temporary to get data but
    #' not saved in code.
    #' @param try (\code{logical}) whether perform function evaluation inside \code{try} clause
    #'
    #' @return nothing, in order to get the data please use \code{get_data} method
    pull = function(args = NULL, try = FALSE) {
      data <- private$.pull_fun$run(args = args, try = try)

      private$.dataset <- RelationalDataset$new(
        x = data,
        dataname = self$get_dataname(),
        code = private$get_pull_code(deparse = TRUE),
        keys = self$get_keys(),
        label = self$get_label()
      )

      if (length(private$get_mutate_code()) > 0) {
        private$.dataset <- mutate_dataset(
          private$.dataset,
          code = private$get_mutate_code()
        )
      }

      return(invisible(NULL))
    },
    #' @description
    #' Set label of the \code{dataset} object
    #'
    #' @param label (\code{character})\cr
    #'  Label to describe the dataset
    set_label = function(label) {
      stopifnot(utils.nest::is_character_vector(label, 0, 1))
      private$.label <- label
      return(invisible(TRUE))
    }
  ),

  # RelationalDatasetConnector private ----
  private = list(
    .dataname = character(0),
    .keys = NULL,
    .label = character(0),
    .mutate_code = NULL,

    get_pull_code = function(deparse = TRUE) {
      code <- if (deparse) {
        sprintf("%s <- %s",
                private$.dataname,
                super$get_pull_code(deparse))
      } else {
        substitute(
          a <- b,
          list(a = as.name(private$.dataname),
               b = super$get_pull_code(deparse))
        )
      }

      return(code)
    },

    get_mutate_code = function(deparse = TRUE) {

      code <- if (deparse) {
        if (length(private$.mutate_code) > 0) {
          paste0(
            vapply(
              private$.mutate_code,
              FUN = deparse,
              FUN.VALUE = character(1),
              width.cutoff = 80L
            ),
            collapse = "\n"
          )
        } else {
          character(0)
        }

      } else {
        private$.mutate_code
      }

      return(code)
    },

    set_code = function(code) {
      stopifnot(utils.nest::is_character_vector(code, 0, 1))
      if (length(code) > 0) {
        private$.mutate_code <- as.list(as.call(parse(text = code)))
      }

      return(invisible(TRUE))
    },

    set_dataname = function(dataname) {
      stopifnot(utils.nest::is_character_single(dataname))
      private$.dataname <- dataname
      return(invisible(TRUE))
    },

    set_keys = function(keys) {
      stopifnot(is(keys, "keys"))
      private$.keys <- keys
      return(invisible(TRUE))
    }
  )
)

# DatasetConnector wrappers ----

#' Create a new \code{RelationalDatasetConnector} object. Set the pulling function
#' \link{CallableFunction} to load \code{data.frame}. \code{dataname} will be used as name
#' of object to be assigned.
#'
#' @param pull_fun (\code{CallableFunction})\cr
#'  function to load the data.
#' @param dataname (\code{character})\cr
#'  A given name for the dataset it may not contain spaces
#' @param keys (\code{keys})\cr
#'  object of S3 class keys containing foreign, primary keys and parent information
#' @param code (\code{character})\cr
#'  A character string defining the code needed to produce the data set in \code{x}
#' @param label (\code{character})\cr
#'  Label to describe the dataset
#'
#' @return new \code{RawDatasetConnector} object
#' @export
relational_dataset_connector <- function(pull_fun,
                                         dataname,
                                         keys,
                                         code = character(0),
                                         label = character(0)) {
  RelationalDatasetConnector$new(pull_fun = pull_fun,
                                 dataname = dataname,
                                 keys = keys,
                                 code = code,
                                 label = label)
}


#' Set up connection to \code{random.cdisc.data}
#'
#' @export
#'
#' @param fun (\code{function}) connection function
#' @param ... additional arguments passed to fun
#' @inheritParams cdisc_dataset
#'
#' @return (\code{DatasetConnector}) type of object
#'
#' @examples
#' library(random.cdisc.data)
#' x <- rcd_dataset_connector("ADSL", radsl, cached = TRUE)
#' x$get_call()
#' x$get_dataset()
rcd_dataset_connector <- function(dataname, fun, ...) {
  stopifnot(is_character_single(dataname))
  stopifnot(is.function(fun))

  dot_args <- list(...)
  stopifnot(is_fully_named_list(dot_args))

  x_fun <- callable_function(fun) # nolint
  x_fun$set_args(dot_args)

  x <- DatasetConnector$new() # nolint
  x$set_dataname(dataname)
  x$set_pull_fun(x_fun)
  x$set_keys(get_cdisc_keys(dataname))

  return(x)
}

#' Set up connection to local \code{rds} file
#'
#' @export
#'
#' @param file (\code{character}) path to \code{.rds} that contains
#'   a single data.frame stored by \code{saveRDS}
#' @inheritParams cdisc_dataset
#'
#' @return (\code{DatasetConnector}) type of object
#'
#' @importFrom tools file_ext
#' @examples
#' \dontrun{
#' x <- rds_dataset_connector("ADSL", "/path/to/file.rds")
#' x$get_call()
#' x$get_dataset()
#' }
rds_dataset_connector <- function(dataname, file, keys = get_cdisc_keys(dataname)) {
  stopifnot(is_character_single(dataname))
  stopifnot(is_character_single(file))
  stopifnot(file.exists(file))
  stopifnot(tolower(tools::file_ext(file)) == "rds")

  x_fun <- callable_function(readRDS) # nolint
  x <- DatasetConnector$new() # nolint

  x$set_dataname(dataname)
  x$set_keys(keys)
  x$set_pull_fun(x_fun)
  x$set_pull_args(list(file = file))
  x$set_path(file)
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
#' x <- rice_dataset_connector("ADSL", "/path/to/ADSL")
#' x$get_call()
#' \dontrun{
#' x$get_dataset()
#' }
rice_dataset_connector <- function(dataname,
                                   path,
                                   keys = get_cdisc_keys(dataname)) {
  stopifnot(is_character_single(dataname))
  stopifnot(is_character_single(path))
  stopifnot(is(keys, "keys"))
  stopifnot(all_true(keys, function(x) is.null(x) || is_character_vector(x)))
  stopifnot(all(c("primary", "foreign", "parent") %in% names(keys)))

  check_pckg_quietly("rice",
                     paste0("Connection to entimICE via rice was requested, but rice package is not available.",
                            "Please install it from https://github.roche.com/Rpackages/rice."))

  x <- DatasetConnector$new() # nolint

  x$set_dataname(dataname)
  x$set_path(path)
  x$set_keys(keys)

  pull_fun <- callable_function(rice::rice_read) # nolint
  x$set_pull_fun(pull_fun)

  x$set_pull_args(list(node = path, prolong = TRUE, quiet = TRUE))

  return(x)
}
