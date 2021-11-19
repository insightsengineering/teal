## JoinKeys ====
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @title R6 Class to store relationships for joining datasets
#'
#' @description
#' This class stores symmetric links between pairs of key-values
#' (e.g. column A of dataset X can be joined with column B of dataset Y). This relationship
#' is more general than the SQL foreign key relationship which also imposes constraints on the values
#' of these columns.
#' @param dataset_1 (`character`) one dataset name
#' @param dataset_2 (`character`) other dataset name
#'
#' @examples
#' x <- teal:::JoinKeys$new()
#' x$set(
#'   list(
#'     join_key("dataset_A", "dataset_B", c("col_1" = "col_a")),
#'     join_key("dataset_A", "dataset_C", c("col_2" = "col_x", "col_3" = "col_y"))
#'   )
#' )
#' x$get()
#' x$mutate("dataset_A", "dataset_B", c("col1" = "col10"))
#' x$get("dataset_A", "dataset_B")
JoinKeys <- R6::R6Class( # nolint
  classname = "JoinKeys",
  ## __Public Methods ====
  public = list(
    #' @description
    #' Create a new object of `JoinKeys`
    #' @return empty (`JoinKeys`)
    initialize = function() {
      logger::log_trace("JoinKeys initialized.")
      return(invisible(self))
    },
    #' @description
    #' Split the current `JoinKeys` object into a named list of join keys objects with an element for each dataset
    #' @return (`list`) a list of `JoinKeys` object
    split = function() {
      list_of_list_of_join_key_set <- lapply(
        names(self$get()),
        function(dataset_1) lapply(
          names(self$get()[[dataset_1]]),
          function(dataset_2) join_key(dataset_1, dataset_2, self$get()[[dataset_1]][[dataset_2]])
        )
      )
      res <- lapply(
        list_of_list_of_join_key_set,
        function(x) {
          y <- JoinKeys$new()
          y$set(x)
        }
      )
      names(res) <- names(self$get())

      logger::log_trace("JoinKeys$split keys split.")
      return(res)
    },
    #' @description
    #' Merging a list (or one) of `JoinKeys` objects into the current `JoinKeys` object
    #' @param x  `list` of `JoinKeys` objects or single `JoinKeys` object
    #' @return (`self`) invisibly for chaining
    merge = function(x) {
      stopifnot(is_class_list("JoinKeys")(x) || is(x, "JoinKeys"))
      if (!is_class_list("JoinKeys")(x)) {
        x <- list(x)
      }
      for (jk in x) {
        for (dataset_1 in names(jk$get())) {
          for (dataset_2 in names(jk$get()[[dataset_1]]))
            self$mutate(dataset_1, dataset_2, jk$get()[[dataset_1]][[dataset_2]])
        }
      }
      logger::log_trace("JoinKeys$merge keys merged.")
      return(invisible(self))
    },
    #' @description
    #' Get join keys between two datasets.
    #' @return (`character`) named character vector x with names(x) the
    #' columns of `dataset_1` and the values of `(x)` the corresponding join
    #' keys in `dataset_2` or `character(0)` if no relationship
    #' @details if one or both of `dataset_1` and `dataset_2` are missing then
    #' underlying keys structure is returned for further processing
    get = function(dataset_1, dataset_2) {
      if (missing(dataset_1) && missing(dataset_2)) {
        return(private$.keys)
      }
      if (missing(dataset_2)) {
        return(private$.keys[[dataset_1]])
      }
      if (missing(dataset_1)) {
        return(private$.keys[[dataset_2]])
      }
      if (is.null(private$.keys[[dataset_1]][[dataset_2]])) {
        return(character(0))
      }
      return(private$.keys[[dataset_1]][[dataset_2]])
    },
    #' @description
    #' Change join_keys for a given pair of dataset names (or
    #' add join_keys for given pair if it does not exist)
    #' @param val (named `character`) column names used to join
    #' @return (`self`) invisibly for chaining
    mutate = function(dataset_1, dataset_2, val) {
      stopifnot(is_character_single(dataset_1))
      stopifnot(is_character_single(dataset_2))
      stopifnot(is_character_vector(val, min_length = 0L))

      private$join_pair(join_key(dataset_1, dataset_2, val))

      logger::log_trace("JoinKeys$mutate changed the keys between { dataset_1 } and { dataset_2 }.")
      return(invisible(self))
    },
    #' @description
    #' Set up join keys basing on list of `JoinKeySet` objects.
    #' @param x  `list` of `JoinKeySet` objects (which are created using the `join_key` function)
    #' or single `JoinKeySet` objects
    #' @details Note that join keys are symmetric although the relationship only needs
    #' to be specified once
    #' @return (`self`) invisibly for chaining
    set = function(x) {
      if (!is_empty(private$.keys)) {
        stop("Keys already set, please use JoinKeys$mutate() to change them")
      }
      if (!is_class_list("JoinKeySet")(x)) {
        x <- list(x)
      }

      # check if any JoinKeySets share the same datasets but different values
      for (idx_1 in seq_along(x)) {
        for (idx_2 in seq_len(idx_1)) {
          private$check_compatible_keys(x[[idx_1]], x[[idx_2]])
        }
      }

      stopifnot(is_class_list("JoinKeySet")(x))
      lapply(x, private$join_pair)

      logger::log_trace("JoinKeys$set keys are set.")
      return(invisible(self))
    }
  ),
  ## __Private Fields ====
  private = list(
    .keys = list(),

    join_pair = function(join_key) {

      dataset_1 <- join_key$dataset_1
      dataset_2 <- join_key$dataset_2
      keys <- join_key$keys

      if (is.null(private$.keys[[dataset_1]])) {
        private$.keys[[dataset_1]] <- list()
      }
      private$.keys[[dataset_1]][[dataset_2]] <- keys

      if (dataset_2 != dataset_1) {
        if (is.null(private$.keys[[dataset_2]])) {
          private$.keys[[dataset_2]] <- list()
        }

        if (length(keys) > 0){
          keys <- setNames(names(keys), keys)
        }
        private$.keys[[dataset_2]][[dataset_1]] <- keys
      }

    },
    # helper function to deterimine if two key sets contain incompatible keys
    # return TRUE if compatible, throw error otherwise
    check_compatible_keys = function(join_key_1, join_key_2){

      error_message <- function(dataset_1, dataset_2){
        stop(
          paste("cannot specify multiple different join keys between datasets:", dataset_1, "and", dataset_2)
        )
      }


      # if first datasets and the second datasets match and keys
      # must contain the same named elements
      if (join_key_1$dataset_1 == join_key_2$dataset_1 && join_key_1$dataset_2 == join_key_2$dataset_2) {
        if (!identical(sort(join_key_1$keys), sort(join_key_2$keys))) {
          error_message(join_key_1$dataset_1, join_key_1$dataset_2)
        }
      }

      # if first dataset of join_key_1 matches second dataset of join_key_2
      # and the first dataset of join_key_2 must match second dataset of join_key_1
      # and keys must contain the same elements but with names and values swapped
      if (join_key_1$dataset_1 == join_key_2$dataset_2 && join_key_1$dataset_2 == join_key_2$dataset_1) {

        # have to handle empty case differently as names(character(0)) is NULL
        if (is_empty(join_key_1$keys) && is_empty(join_key_2$keys)) {
          return(TRUE)
        }

        if (xor(is_empty(join_key_1$keys), is_empty(join_key_2$keys)) ||
            !identical(sort(join_key_1$keys), sort(setNames(names(join_key_2$keys), join_key_2$keys)))) {
          error_message(join_key_1$dataset_1, join_key_1$dataset_2)
        }
      }

      # otherwise they are compatible
      return(TRUE)
    }
  )
)

# constructors ====

#' Create a `JoinKeys` out of a list of `JoinKeySet` objects
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param ... optional, a `JoinKeySet` objects created using the `join_key` function.
#' @details Note that join keys are symmetric although the relationship only needs
#' to be specified once.
#'
#' @return `JoinKeys`
#'
#' @export
#'
#' @examples
#' join_keys()
#' join_keys(
#'   join_key("dataset_A", "dataset_B", c("col_1" = "col_a")),
#'   join_key("dataset_A", "dataset_C", c("col_2" = "col_x", "col_3" = "col_y"))
#' )
#' join_keys(
#'   join_key("dataset_A", "dataset_B", c("col_1" = "col_a"))
#' )
join_keys <- function(...) {
  x <- list(...)
  res <- JoinKeys$new()
  if (!is_empty(x))
    res$set(x)
  res
}


# wrappers ====
#' Mutate `JoinKeys` with a new values
#'
#' @description `r lifecycle::badge("experimental")`
#' Mutate `JoinKeys` with a new values
#'
#' @param x (`JoinKeys`) object to be modified
#' @param dataset_1 (`character`) one dataset name
#' @param dataset_2 (`character`) other dataset name
#' @param val (named `character`) column names used to join
#'
#' @return modified `JoinKeys` object
#'
#' @export
mutate_join_keys <- function(x, dataset_1, dataset_2, val) {
  UseMethod("mutate_join_keys")
}

#' @rdname mutate_join_keys
#' @export
#' @examples
#' # JoinKeys ----
#'
#' x <- join_keys(
#'   join_key("dataset_A", "dataset_B", c("col_1" = "col_a")),
#'   join_key("dataset_A", "dataset_C", c("col_2" = "col_x", "col_3" = "col_y"))
#' )
#' x$get("dataset_A", "dataset_B")
#'
#' mutate_join_keys(x, "dataset_A", "dataset_B", c("col_1" = "col_10"))
#' x$get("dataset_A", "dataset_B")
mutate_join_keys.JoinKeys <- function(x, dataset_1, dataset_2, val) {
  x$mutate(dataset_1, dataset_2, val)
}

#' @rdname mutate_join_keys
#' @export
#' @examples
#' # RelationalData ----
#'
#' library(scda)
#'
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#' ADRS <- synthetic_cdisc_data("latest")$adrs
#'
#' x <- cdisc_data(
#'   cdisc_dataset("ADSL", ADSL),
#'   cdisc_dataset("ADRS", ADRS)
#' )
#' x$get_join_keys()$get("ADSL", "ADRS")
#'
#' mutate_join_keys(x, "ADSL", "ADRS", c("COLUMN1" = "COLUMN2"))
#' x$get_join_keys()$get("ADSL", "ADRS")
#'
mutate_join_keys.RelationalData <- function(x, dataset_1, dataset_2, val) { # nolint
  x$mutate_join_keys(dataset_1, dataset_2, val)
}


#' Create a relationship between a pair of datasets
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @inheritParams mutate_join_keys
#' @param keys (optionally named `character`) where `names(keys)` are columns in `dataset_1`
#' with relationship to columns of `dataset_2` given by the elements in `keys`.
#' If `names(keys)` is `NULL` then the same column names are used for both `dataset_1`
#' and `dataset_2`.
#'
#' @return object of class `JoinKeySet` to be passed into `join_keys` function.
#'
#' @seealso [join_keys()]
#'
#' @export
join_key <- function(dataset_1, dataset_2, keys) {
  stopifnot(is_character_single(dataset_1))
  stopifnot(is_character_single(dataset_2))
  stopifnot(is_character_vector(keys, min_length = 0))

  if (!is_empty(keys)) {
    if (is.null(names(keys))) {
      names(keys) <- keys
    }

    if (any(names(keys) == "")) {
      names(keys)[names(keys) == "" & keys != ""] <- keys[names(keys) == "" & keys != ""]
    }

    stopifnot(!is.null(names(keys)))
    stopifnot(!anyDuplicated(keys))
    stopifnot(!anyDuplicated(names(keys)))
  }

  if (dataset_1 == dataset_2 && any(names(keys) != keys)) {
    stop("Keys within a dataset must match exactly: keys = c('A' = 'B') are not allowed")
  }

  structure(
    list(
      dataset_1 = dataset_1,
      dataset_2 = dataset_2,
      keys = keys
    ),
    class = "JoinKeySet"
  )
}
