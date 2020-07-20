## RelationalData ====
#' @title \code{RelationalData} class
#' @description
#' Class combines multiple \code{RelationalDataset} objects.
#'
#' @importFrom R6 R6Class
#'
#' @examples
#' x <- relational_dataset(
#'   x = data.frame(x = c(1, 2), y = c("a", "b"), stringsAsFactors = FALSE),
#'   keys = keys(primary = "y", foreign = NULL, parent = NULL),
#'   dataname = "XY",
#'   code = "XY <- data.frame(x = c(1, 2), y = c('a', 'b'),
#'                            stringsAsFactors = FALSE)",
#'   label = character(0)
#' )
#'
#' x2 <- relational_dataset(
#'   x = data.frame(x = c(1, 2), y = c("a", "b"), stringsAsFactors = FALSE),
#'   keys = keys(primary = "y", foreign = NULL, parent = NULL),
#'   dataname = "XYZ",
#'   code = "XYZ <- data.frame(x = c(1, 2), y = c('a', 'b'),
#'                            stringsAsFactors = FALSE)",
#'   label = character(0)
#' )
#'
#' rd <- teal:::RelationalData$new(x, x2)
RelationalData <- R6::R6Class( #nolint
  classname = "RelationalData",
  ## __Public Methods ====
  public = list(
    #' @description
    #' Create a new \code{RelationalData} object from multiple
    #' \code{RelationalDataset} objects.
    #'
    #' @param ... (\code{RelationalDataset})\cr
    #'  at least one object.
    #'
    #' @return new \code{RelationalData} object
    initialize = function(...) {
      datasets <- list(...)

      is_teal_data <- is_any_class_list(datasets, "RelationalDataset")
      if (!all(is_teal_data)) {
        stop("All data elements should be RelationalDataset")
      }

      dataset_names <- vapply(datasets, get_dataname, character(1))
      if (any(duplicated(dataset_names))) {
        stop("Dataset names should be unique")
      }

      names(datasets) <- dataset_names
      private$datasets <- datasets

      return(invisible(self))
    },
    #' @description
    #'   Check if the object raw data is reproducible from the \code{get_code()} code.
    #' @return
    #'   \code{TRUE} if all the datasets generated from evaluating the
    #'   \code{get_code()} code are identical to the raw data, else \code{FALSE}.
    check = function() {
      if (!self$is_pulled()) {
        stop("Cannot check the raw data until it is pulled.")
      }

      if (is.null(private$code)) {
        all(vapply(private$datasets, function(x) x$check(), logical(1)))
      } else {
        all(vapply(private$datasets, function(x) private$check_dataset_all_code(x), logical(1)))
      }
    },
    #' @description
    #' Get all datasets and all dataset connectors
    #'
    #'   name of dataset connector to be returned. If \code{NULL}, all connectors are returned.
    #' @param dataname (\code{character} value)\cr
    #'
    #' @return \code{list} with all datasets and all connectors
    get_all_datasets = function(dataname = NULL) {
      stopifnot(is.null(dataname) || is_character_single(dataname))
      if (is.null(dataname)) {
        private$datasets
      } else {
        private$datasets[[dataname]]
      }
    },
    #' @description
    #' Get names of the datasets.
    #'
    #' @return \code{character} vector with names of all datasets.
    get_datanames = function() {
      vapply(private$datasets, get_dataname, character(1))
    },
    #' @description
    #' Get code for all datasets.
    #' @param dataname (\code{character}) dataname or \code{NULL} for all datasets
    #' @param deparse (\code{logical}) whether to return the deparsed form of a call
    #' @return (\code{character}) vector of code to generate datasets.
    get_code = function(dataname = NULL, deparse = TRUE) {
      stopifnot(is_logical_single(deparse))

      datasets_code <- private$get_code_datasets(dataname = dataname, deparse = deparse)
      mutate_code <- private$get_mutate_code(deparse = deparse)

      all_code <- c(datasets_code, mutate_code)

      if (isTRUE(deparse)) {
        all_code <- paste0(all_code, collapse = "\n")
      }

      return(all_code)
    },
    #' @description
    #' Set reproducible code
    #' @param code (\code{character}) reproducible code
    #' @return self invisibly for chaining
    set_code = function(code) {
      stopifnot(is_character_vector(code, min_length = 0, max_length = 1))

      if (length(code) > 0 && !is_empty_string(code)) {
        private$code <- c(private$code,
                          `if`(is_empty(parse(text = code)), code, as.list(as.call(parse(text = code)))))
      }

      return(invisible(self))
    },
    #' @description
    #' Get \code{RelationalDataset} object.
    #'
    #' @param dataname (\code{character} value)\cr
    #'   name of dataset to be returned. If \code{NULL}, all datasets are returned.
    #'
    #' @return \code{RelationalDataset}.
    get_dataset = function(dataname = NULL) {
      stopifnot(is.null(dataname) || is_character_single(dataname))
      self$get_datasets()[[dataname]]
    },
    #' @description
    #' Get \code{list} of \code{RelationalDataset} objects.
    #'
    #' @return \code{list} of \code{RelationalDataset}.
    get_datasets = function() {

      if (is.null(private$code)) {
        private$datasets
      } else {
        # have to evaluate post-processing code (i.e. private$code) before returning dataset
        new_env <- new.env()
        for (dataset in private$datasets) {
          assign(dataset$get_dataname(), get_raw_data(dataset), envir = new_env)
        }
        for (code_chunk in private$code) {
          eval(code_chunk, envir = new_env)
        }
        lapply(
          private$datasets,
          function(x) {
            x_name <- x$get_dataname()
            relational_dataset(
              dataname = x_name,
              x = get(x_name, new_env),
              keys = x$get_keys(),
              code = x$get_code(),
              label = x$get_dataset_label()
            )
          }
        )
      }
    },
    #' @description
    #' Check if dataset has already been pulled.
    #'
    #' @return \code{TRUE} if dataset has been already pulled, else \code{FALSE}
    is_pulled = function() {
      all(vapply(private$datasets, is_pulled, logical(1)))
    }
  ),

  ## __Private Fields ====
  private = list(
    datasets = NULL,
    code = NULL, # list of calls

    ## __Private Methods ====
    get_mutate_code = function(deparse = TRUE) {
      if (is.null(private$code)) {
        if (isTRUE(deparse)) {
          character(0)
        } else {
          NULL
        }
      } else if (isTRUE(deparse)) {
        paste(
          vapply(
            private$code,
            function(x) {
              if (is.character(x)) {
                x
              } else {
                paste(
                  deparse(x, width.cutoff = 500L),
                  collapse = "\n"
                )
              }
            },
            FUN.VALUE = character(1),
            USE.NAMES = FALSE
          ),
          collapse = "\n"
        )
      } else {
        private$code
      }
    },
    get_code_datasets = function(dataname = NULL, deparse = TRUE) {
      if (is.null(private$datasets)) {
        if (isTRUE(deparse)) {
          character(0)
        } else {
          NULL
        }
      } else if (!is.null(dataname) && !(dataname %in% self$get_datanames())) {
        if (isTRUE(deparse)) {
          character(0)
        } else {
          NULL
        }
      } else if (is.null(private$code) && !is.null(dataname)) {
        if_cond(get_code(private$datasets[[dataname]], deparse = deparse), character(0), is_empty_string)
      } else {
        if (isTRUE(deparse)) {
          Filter(
            Negate(is_empty_string),
            vapply(private$datasets, get_code, character(1), deparse = TRUE, USE.NAMES = FALSE)
          )
        } else {
          unname(unlist(lapply(private$datasets, get_code, deparse = FALSE)))
        }
      }
    },
    check_dataset_all_code = function(dataset) {
      code <- self$get_code()

      new_env <- new.env(parent = parent.env(.GlobalEnv))
      tryCatch({
        eval(parse(text = code), new_env)
      }, error = function(e) {
        error_dialog(e)
      })

      res_check <- tryCatch({
        identical(get_raw_data(dataset), get(get_dataname(dataset), envir = new_env))
      }, error = function(e) {
        FALSE
      })

      return(res_check)
    }
  )
)
