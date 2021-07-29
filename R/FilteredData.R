#' @name FilteredData
#' @docType class
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @title Class to encapsulate filtered datasets
#'
#' @details
#' The main purpose of this class is to provide a collection of reactive datasets,
#' each dataset having a filter state that determines how it is filtered.
#'
#' For each dataset, `get_filter_expr` returns the call to filter the dataset according
#' to the filter state. The data itself can be obtained through `get_data`.
#' Other classes take care of actually merging together all the datasets.
#'
#' The datasets are filtered lazily, i.e. only when requested / needed in the Shiny app.
#'
#' By the design of the class (and `reactiveValues`), any dataname set through `set_data`
#' cannot be removed because other code may already depend on it. As a workaround, the
#' underlying data can be set to `NULL`.
#'
#' The class currently supports variables of the following types within datasets:
#' - `choices`: variable of type `factor`, e.g. `ADSL$COUNTRY`, `iris$Species`
#'      zero or more options can be selected, when the variable is a factor
#' - `logical`: variable of type `logical`, e.g. `ADSL$TRT_FLAG`
#'      exactly one option must be selected, `TRUE` or `FALSE`
#' - `ranges`: variable of type `numeric`, e.g. `ADSL$AGE`, `iris$Sepal.Length`
#'      numerical range, a range within this range can be selected
#' Other variables cannot be filtered for.
#'
#' Common arguments are:
#' 1. `filtered`: filtered dataset or not
#' 2. `dataname`: one of the datasets
#' 3. `varname`: one of the columns in a dataset
#'
#'
#' @examples
#' library(shiny)
#' datasets <- teal:::FilteredData$new()
#'
#' # setting the data
#' isolate({
#'   datasets$set_dataset(dataset("iris", iris))
#'   datasets$set_dataset(dataset("mtcars", mtcars))
#'  })
#'
#'
#' isolate({
#'   datasets$datanames()
#'   datasets$get_data_info("iris", filtered = FALSE)
#'
#'   # filters dataset to obtain information
#'   datasets$get_data_info("mtcars", filtered = TRUE)
#'
#'   print(datasets$get_call("iris"))
#'   print(datasets$get_call("mtcars"))
#'
#'   df <- datasets$get_data("iris", filtered = FALSE)
#'   print(df)
#'  })
#'
#'
#' filter_state_iris <- teal:::init_filter_state(
#'   iris$Species,
#'   varname = "Species",
#'   varlabel = "Species name",
#'   input_dataname = as.name("iris"),
#'   use_dataname = TRUE
#' )
#' filter_state_iris$set_selected("virginica")
#'
#' queue <- datasets$get_filtered_datasets("iris")$get_filter_states(1)
#' queue$queue_push(filter_state_iris, queue_index = 1L, element_id = "virginica")
#'
#' isolate(datasets$get_call("iris"))
#'
#'
#' filter_state_mtcars <- teal:::init_filter_state(
#'   mtcars$mpg,
#'   varname = "mpg",
#'   varlabel = "Miles per galon",
#'   input_dataname = as.name("mpg"),
#'   use_dataname = TRUE
#' )
#' filter_state_mtcars$set_selected(c(15, 20))
#'
#' queue <- datasets$get_filtered_datasets("mtcars")$get_filter_states("filter")
#' queue$queue_push(filter_state_mtcars, queue_index = 1L, element_id = "mpg")
#'
#' isolate(datasets$get_call("iris"))
#' isolate(datasets$get_call("mtcars"))
FilteredData <- R6::R6Class( # nolint
  "FilteredData",
  ## __Public Methods ====
  public = list(
    #' @description
    #' Initialize a `FilteredData` object
    initialize = function() {
      return(invisible(self))
    },
    #' @description
    #' Get datanames
    #'
    #' The datanames are returned in the order in which they must be
    #' evaluated (in case of dependencies).
    #' @return (`character` vector) of datanames
    datanames = function() {
      return(names(private$filtered_datasets))
    },


    #' Get data label for the dataset
    #'
    #' Useful to display in `Show R Code`.
    #'
    #' @param dataname (`character`) name of the dataset
    #' @return (`character`) keys of dataset
    get_datalabel = function(dataname) {
      return(self$get_data_attr(dataname, "data_label"))
    },

    #' @description
    #' Get dataset names of a given dataname for the filtering.
    #'
    #' @param dataname (`character` vector) names of the dataset
    #' @return (`character` vector) of dataset names
    get_filterable_datanames = function(dataname) {
      dataname
    },
    #' @description
    #' Get variable names of a given dataname for the filtering.
    #'
    #' @param dataname (`character`) name of the dataset
    #' @return (`character` vector) of variable names
    get_filterable_varnames = function(dataname) {
      self$get_varnames(dataname)
    },

    # datasets methods ----
    #' @description
    #' Get a `call` to filter the dataset according to the filter state
    #'
    #' It returns a `call` to filter the dataset only, assuming the
    #' other (filtered) datasets it depends on are available.
    #'
    #' Together with `self$datanames()` which returns the datasets in the correct
    #' evaluation order, this generates the whole filter code, see the function
    #' `FilteredData$get_filter_code`.
    #'
    #' For the return type, note that `rlang::is_expression` returns `TRUE` on the
    #' return type, both for base R expressions and calls (single expression,
    #' capturing a function call).
    #'
    #' The filtered dataset has the name given by `self$filtered_dataname(dataname)`
    #'
    #' This can be used for the `Show R Code` generation.
    #'
    #' @param dataname (`character`) name of the dataset
    #' @return (`call` or `list` of calls) to filter dataset
    #'  calls
    get_call = function(dataname) {
      private$check_data_varname_exists(dataname)
      self$get_filtered_datasets(dataname)$get_call()
    },

    #' @description
    #' Get the R preprocessing code string that generates the unfiltered datasets
    #' @param dataname (`character`) name(s) of dataset(s)
    #' @return (`character`) deparsed code
    get_code = function(dataname = self$datanames()) {
      return(paste0(private$code$get_code(dataname), collapse = "\n"))
    },

    #' @description
    #' Get `FilteredDataset` object which contains all informations
    #' related to specific dataset.
    #' @param dataname (`character(1)`)\cr
    #'  name of the dataset.
    #' @return `FilteredDataset` object or list of `FilteredDataset`
    get_filtered_datasets = function(dataname = character(0)) {
      if (is_empty(dataname)) {
        private$filtered_datasets
      } else {
        private$filtered_datasets[[dataname]]
      }
    },

    #' @description
    #' Get filtered or unfiltered dataset
    #'
    #' For `filtered = FALSE`, the original data set with
    #' `set_data` is returned including all attributes.
    #'
    #' @param dataname (`character`) name of the dataset
    #' @param filtered (`logical`) whether to return filtered or unfiltered dataset
    get_data = function(dataname, filtered = TRUE) {
      private$check_data_varname_exists(dataname)
      stopifnot(is_logical_single(filtered))

      self$get_filtered_datasets(dataname)$get_data(filtered = filtered)
    },

    #' @description
    #' Get data attribute for the dataset
    #'
    #' sets and gets the data attribute on unfiltered data as it is never modified
    #' as attributes
    #'
    #' @param dataname (`character`) name of the dataset
    #' @param attr (`character`) attribute to get from the data attributes of the dataset
    #' @return value of attribute, may be `NULL` if it does not exist
    get_data_attr = function(dataname, attr) {
      private$check_data_varname_exists(dataname)
      stopifnot(is_character_single(attr))
      return(
        get_attrs(
          self$get_filtered_datasets(dataname)$get_dataset()
        )[[attr]]
      )
    },

    #' @description
    #' Get info about dataname, i.e. number of rows
    #'
    #' @param dataname (`character`) name of the dataset
    #' @param filtered `logical` whether to obtain this info for the
    #'   filtered dataset
    #' @return a named vector
    get_data_info = function(dataname, filtered) {
      private$check_data_varname_exists(dataname)
      stopifnot(is_logical_single(filtered))
      list(
        Rows = self$get_filtered_datasets(dataname)$get_data_info(filtered = filtered)
      )
    },

    #' @description
    #' Get join keys between two datasets.
    #' @param dataset_1 (`character`) one dataset name
    #' @param dataset_2 (`character`) other dataset name
    #' @return (`named character`) vector with column names
    get_join_keys = function(dataset_1, dataset_2) {
      if (is.null(private$join_keys))
        return(character(0))
      private$join_keys$get(dataset_1, dataset_2)
    },


    #' @description
    #' Get filter overview table in form of X (filtered) / Y (non-filtered)
    #'
    #' This is intended to be presented in the application.
    #' The content for each of the data names is defined in `get_data_info` method.
    #'
    #' @param datanames (`character` vector) names of the dataset
    #' @return (`data.frame`)
    get_filter_overview_tbl = function(datanames) {
      if (identical(datanames, "all")) {
        datanames <- self$datanames()
      }

      check_in_subset(datanames, self$datanames(), "Some datasets are not available: ")

      data_info_filtered <- sapply(
        datanames,
        function(dataname) {
          self$get_data_info(dataname, filtered = TRUE)
        },
        USE.NAMES = TRUE,
        simplify = FALSE
      )
      data_info_nfiltered <- sapply(
        datanames,
        function(dataname) {
          self$get_data_info(dataname, filtered = FALSE)
        },
        USE.NAMES = TRUE,
        simplify = FALSE
      )

      res_list <- Map(
        x = data_info_filtered,
        y = data_info_nfiltered,
        function(x, y) {
          setNames(paste0(x, "/", y), names(x))
          sapply(
            names(x),
            function(xname) {
              paste0(x[[xname]], "/", y[[xname]])
            },
            USE.NAMES = TRUE
          )
        }
      )

      res_df <- as.data.frame(do.call(rbind, res_list))
      res_df[, "Dataset"] <- rownames(res_df)
      rownames(res_df) <- NULL
      res_df <- res_df[, c("Dataset", setdiff(names(res_df), "Dataset"))]

      return(res_df)
    },



    #' Get keys for the dataset
    #' @param dataname (`character`) name of the dataset
    #' @return (`character`) keys of dataset
    get_keys = function(dataname) {
      self$get_filtered_datasets(dataname)$get_keys()
    },
    #' @description
    #' Get labels of variables in the data
    #'
    #' Variables are the column names of the data.
    #' Either, all labels must have been provided for all variables
    #' in `set_data` or `NULL`.
    #'
    #' @param dataname (`character`) name of the dataset
    #' @param variables (`character` vector) variables to get labels for;
    #'   if `NULL`, for all variables in data
    #' @return (`character` or `NULL`) variable labels, `NULL` if `column_labels`
    #'   attribute does not exist for the data
    get_varlabels = function(dataname, variables = NULL) {
      self$get_filtered_datasets(dataname)$get_varlabels(variables = variables)
    },

    #' @description
    #' Get variable names
    #'
    #' @param dataname (`character`) name of the dataset
    #' @return (`character` vector) of variable names
    get_varnames = function(dataname) {
      self$get_filtered_datasets(dataname)$get_varnames()
    },


    #' @description
    #' Add dataset
    #'
    #' Add dataset and preserve all attributes attached to this object.
    #' Technically `set_dataset` created `FilteredDataset` which keeps
    #' `dataset` for filtering purpose.
    #'
    #' @param dataset (`Dataset`)\cr
    #'   object containing data and attributes.
    #' @param join_key_set (`JoinKeySet`)\cr
    #'   keys to merge this `dataset` to the other datasets
    #' @return (`self`) object of this class
    set_dataset = function(dataset, join_key_set = NULL) {
      stopifnot(is(dataset, "Dataset") || is(dataset, "DatasetConnector"))
      dataname <- get_dataname(dataset)
      # to include it nicely in the Show R Code; the UI also uses datanames in ids, so no whitespaces allowed
      check_simple_name(dataname)
      private$filtered_datasets[[dataname]] <- init_filtered_dataset(
        get_dataset(dataset),
        join_keys = join_key_set
      )

      return(invisible(self))
    },

    #' @description
    #' Set the R preprocessing code for single dataset
    #'
    #' @param code `CodeClass` preprocessing code that can be parsed to generate the
    #'   unfiltered datasets
    #' @return (`self`)
    set_code = function(code) {
      stopifnot(inherits(code, "CodeClass"))
      private$code <- code
      return(invisible(self))
    },

    #' @description
    #' Set join keys object
    #' @param x (`JoinKeys`)
    #' @return (`self`) invisibly for chaining
    set_join_keys = function(x) {
      stopifnot(is(x, "JoinKeys"))
      private$join_keys <- x
      return(invisible(self))
    },

    # Functions useful for restoring from another dataset ----

    #' @description
    #' Returns the state to be bookmarked
    #'
    #' hash sums of `datasets`, `FilterState` selections and `preproc_code`
    #'  are bookmarked.
    #'
    #' @return named list
    get_bookmark_state = function() {
     stop("Not implemented")
    },

    #' @description
    #' Sets bookmark state
    #' @param state (`named list`)\cr
    #'  nested list of filter selections applied to datasets.
    set_bookmark_state = function(state) {
      stop("Not implemented")
    },

    #' @description
    #' Set this object from a bookmarked state
    #'
    #' Only sets the filter state, does not set the data
    #' and the preprocessing code. The data should already have been set.
    #' Also checks the preprocessing code is identical if provided in the `state`.
    #'
    #' Since this function is used from the end-user part, its error messages
    #' are more verbose. We don't call the Shiny modals from here because this
    #' class may be used outside of a Shiny app.
    #'
    #' @param state (`named list`)\cr
    #'  containing fields `data_hash`, `filter_states`
    #'   and `preproc_code`.
    #' @param check_data_hash (`logical`) whether to check that `md5sums` agree
    #'   for the data; may not make sense with randomly generated data per session
    restore_state_from_bookmark = function(state, check_data_hash = TRUE) {
      stop("Not implemented")
    }
  ),

  ## __Private Methods ====
  private = list(

    # private attributes ----
    filtered_datasets = list(),

    # preprocessing code used to generate the unfiltered datasets as a string
    code = CodeClass$new(),

    join_keys = NULL,

    # we implement these functions as checks rather than returning logicals so they can
    # give informative error messages immediately

    # @details
    # Validate object to inspect if something is wrong
    # The call to this function should be isolated to avoid a reactive dependency.
    # Getting the names of a reactivevalues also needs a reactive context.
    validate = function() {
      .log("## validating FilteredData object consistency")

      # Note: Here, we directly refer to the private attributes because the goal of this
      # function is to check the underlying attributes and the get / set functions might be corrupted

      has_same_names <- function(x, y) setequal(names(x), names(y))
      # check `filter_states` are all valid
      lapply(
        names(private$filter_states),
        function(dataname) {
          stopifnot(is.list(private$filter_states)) # non-NULL, possibly empty list
          lapply(
            names(private$filter_states[[dataname]]),
            function(varname) {
              var_state <- private$filter_states[[dataname]][[varname]]
              stopifnot(!is.null(var_state)) # should not be NULL, see doc of this attribute
              check_valid_filter_state(
                filter_state = var_state,
                dataname = dataname,
                varname = varname
              )
            }
          )
        }
      )

      return(invisible(NULL))
    },

    # @description
    # Checks if the dataname exists and
    # (if provided) that varname is a valid column in the dataset
    #
    # Stops when this is not the case.
    #
    # @param dataname (`character`) name of the dataset
    # @param varname (`character`) column within the dataset;
    #   if `NULL`, this check is not performed
    check_data_varname_exists = function(dataname, varname = NULL) {
      stopifnot(is_character_single(dataname))
      stopifnot(is.null(varname) || is_character_single(varname))

      isolate({
        # we isolate everything because we don't want to trigger again when datanames
        # change (which also triggers when any of the data changes)
        if (!(dataname %in% names(self$get_filtered_datasets()))) {
          # data must be set already
          stop(paste("data", dataname, "is not available"))
        }
        if (!is.null(varname) && !(varname %in% self$get_varnames(dataname = dataname))) {
          stop(paste("variable", varname, "is not in data", dataname))
        }
      })

      return(invisible(NULL))
    },

    filtered_dataname = function(dataname) {
      stopifnot(is_character_single(dataname))
      sprintf("%s_FILTERED", dataname)
    }
  )
)

# Wrapper functions for `FilteredData` class ----

#' Refer to the default filter state
#'
#' @description `r lifecycle::badge("maturing")`
#' You can use it to refer to the variable's default filter state,
#' which will be set when `FilteredData$set_data` is called.
#' It can be used together with `teal::init`.
#'
#' @details
#' This is a simple wrapper around an S3 class.
#'
#' @export
#' @examples
#' default_filter() # test printing
default_filter <- function() {
  structure(list(), class = "default_filter")
}

#' @export
print.default_filter <- function(x, ...) {
  cat("This will pick the default filter state for the variable.\n")
}


#' Get filter expression for multiple datanames taking into account its order.
#'
#' To be used in show R code button.
#'
#' @param datasets (`FilteredData`)
#' @param datanames (`character`) vector of dataset names
#'
#' @export
#'
#' @return (`expression`)
get_filter_expr <- function(datasets, datanames = datasets$datanames()) {
  stopifnot(
    is(datasets, "FilteredData"),
    is_character_vector(datanames),
    all(datanames %in% datasets$datanames())
  )

  paste(
    utils.nest::ulapply(
      datanames,
      function(dataname) {
        datasets$get_call(dataname)
      }
    ),
    collapse = "\n"
  )
}
