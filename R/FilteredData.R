# Roxygen inheritParams is not working within R6 classes, so we cannot inherit from
# this dummy method.
#' This method exists solely to be able to inherit parameter documentation from it
#' @md
#' @param dataname `character` dataname to get data for, must be in the list
#'   of allowed datanames, see `initialize` (and dataset must have
#'   been set except for in `set_data`)
#' @param varname `character` if non-NULL, a column in the dataset specified
#'   by `dataname`
#' @param filtered `logical` filtered or unfiltered dataset
filtered_data_doc_helper <- function(dataname, varname, filtered) {
  NULL
}

#' @name FilteredData
#' @docType class
#'
#' @title Class to encapsulate filtered datasets
#'
#' @details
#' The main purpose of this class is to provide a collection of reactive datasets,
#' each of which can be filtered through the right panel of teal apps.
#' For each dataset, `get_filter_call` returns the call to filter the dataset according
#' to the UI user selection.
#' Other classes then take care of actually merging together the datasets.
#'
#' In the constructor, all available datanames are specified. These can then be set
#' using the `set_data` method. A method to load from a file is also available.
#'
#' This class is `ADSL`-centric in the sense that `ADSL` is required to apply the filters.
#' Every dataset, in addition to its own filter, is filtered to make sure that it only
#' contains keys present in `ADSL` (defaulting to `(USUBJID, STUDYID)`).
#' Once the `ADSL` dataset is set, the filters are applied.
#'
#' In order to set several datasets or change several filters at once without intermediate
#' refiltering, the functions `hold_filtering` and `continue_filtering` can be used.
#'
#' By the design of the class (and `reactiveValues`), any dataname set through `set_data`
#' cannot be removed because other code may already depend on it.
#'
#' General arguments are:
#' 1. `filtered`: filtered dataset or not
#' 2. `dataname`: one of the datasets
#' 3. `varname`: one of the columns in a dataset
#'
#' @md
#' @importFrom digest digest
#' @importFrom dplyr n_groups group_by_
#' @importFrom haven read_sas
#' @importFrom R6 R6Class
#' @importFrom readr read_csv
#' @importFrom tools file_ext file_path_sans_ext
#'
#' @examples
#' library(random.cdisc.data)
#'
#' ADSL <- radsl(cached = TRUE)
#' x <- teal:::FilteredData$new()
#'
#' # to evaluate without isolate(), i.e. provide a default isolate context
#' options(shiny.suppressMissingContextError = TRUE)
#' on.exit(options(shiny.suppressMissingContextError = FALSE), add = TRUE)
#'
#' x$set_data("ADSL", ADSL)
#'
#' x$datanames()
#' x$print_data_info("ADSL")
#' x$get_filter_info("ADSL")
#' df <- x$get_data("ADSL")
#' # df
#'
#' x$get_filter_type("ADSL", "SEX")
#' x$set_filter_state("ADSL", varname = NULL, state = list(
#'   AGE = list(selection = c(3, 5), keep_na = FALSE),
#'   SEX = list(selection = c("M", "F"), keep_na = FALSE)
#' ))
#' x$get_filter_type("ADSL", "SEX")
#' x$get_filter_info("ADSL")[["SEX"]]$type
#'
#' x$hold_filtering()
#' x$set_filter_state("ADSL", varname = NULL, list(
#'   AGE = list(selection = c(3, 7), keep_na = FALSE)
#' ))
#' x$set_filter_state(
#'   "ADSL",
#'   varname = "SEX",
#'   state = list(selection = c("M", "F"), keep_na = FALSE)
#' )
#' x$continue_filtering()
#' x$get_filter_type("ADSL", "SEX")
#'
#' x$get_filter_state("ADSL")
FilteredData <- R6::R6Class( # nolint
  "FilteredData",
  ## FilteredData ====
  ## __Public Methods ====
  public = list(

    #' @details
    #' Initialized with the allowed datanames. The actual datasets can afterwards
    #' be added using the set_data function.
    #'
    #' @md
    initialize = function() {
      # create reactiveValues for unfiltered and filtered dataset and filter state
      # each reactiveValues is a list with one entry per dataset name
      private$datasets <- reactiveValues()
      private$filtered_datasets <- reactiveValues()
      private$filter_state <- reactiveValues()
      private$filter_infos <- reactiveValues()

      # we explicitly don't make it reactive
      private$previous_filter_state <- list()

      return(invisible(self))
    },

    #' @details
    #' Get datanames, ADSL appears first
    datanames = function() {
      return(make_adsl_first(names(private$datasets)))
    },

    # getters and setters for attributes ----

    #' @details
    #' Get filtered or unfiltered dataset
    #'
    #' @inheritParams filtered_data_doc_helper
    get_data = function(dataname, filtered = FALSE) {
      private$check_data_varname(dataname)
      stopifnot(is_logical_single(filtered))

      if (filtered) {
        # we must be careful not to create a new reactive here
        # by creating it outside and storing it in the list, we benefit from
        # the reactive caching mechanism
        private$filtered_datasets[[dataname]]()
      } else {
        private$datasets[[dataname]]
      }
    },

    #' @details
    #' Add data
    #'
    #' Will also add the `md5` sum of the data.
    #'
    #' Note: due to the nature of reactiveValues(), once a dataname
    #' is added, it cannot be removed anymore because some code may depend
    #' on it. You can try to achieve this by setting the data to NULL
    #' and the observers must then know that NULL means that the dataset
    #' was removed.
    #'
    #' Any attributes attached to data are kept in the unfiltered data.
    #'
    #' @md
    #' @param dataname `character` name  of the data
    set_data = function(dataname, data) {
      stopifnot(is_character_single(dataname))
      if (grepl("[[:space:]]", dataname)) {
        stop(paste0("dataname '", dataname, "' must not contain spaces.", collapse = "\n"))
      }
      stopifnot(is.data.frame(data))
      # column_labels and data_label may be NULL, so attributes will not be present
      stopifnot("keys" %in% names(attributes(data)))

      if (dataname %in% self$datanames()) {
        # already exists, so we only modify the data
      } else {
        private$filtered_datasets[[dataname]] <- private$reactive_filtered_dataset(dataname)
        private$filter_infos[[dataname]] <- private$reactive_filter_infos(dataname)
      }

      # due to the data update, the old filter may no longer be valid, so we unset it
      private$filter_state[[dataname]] <- list()
      private$previous_filter_state[[dataname]] <- list()

      # save md5sum for reproducibility
      attr(data, "md5sum") <- digest(data, algo = "md5")
      private$datasets[[dataname]] <- data

      return(invisible(self))
    },

    # no corresponding set_ function
    #' @details
    #' Get attribute in the `attrs` field
    #'
    #' @md
    #' @param attr attribute to get in the `attrs` field
    get_attr = function(attr) {
      # todo2: remove and instead call get_code in teal.devel
      stopifnot(attr == "code")
      warning("Please replace get_attr by a call to get_code", immediate. = TRUE)
      return(private$code)
    },

    #' @details
    #' Get the R code string that generates the unfiltered datasets
    get_code = function() {
      return(private$code)
    },

    #' @details
    #' Set the R code to generate the unfiltered datasets
    #'
    #' @param code `character` code that can be parsed to generate the
    #'   unfiltered datasets
    set_code = function(code) {
      stopifnot(is_character_single(code))
      # todo2: check here that the code faithfully reproduces the datasets
      private$code <- code
      return(invisible(NULL))
    },

    # todo2: remove these two functions as they are trivial?
    #' @details
    #' Get data attribute for the dataset
    #'
    #' sets and gets the data attribute on unfiltered data as it is never modified
    #' as attributes
    #'
    #' @param attr attribute to get from the data attributes of the dataset
    get_data_attr = function(dataname, attr) {
      private$check_data_varname(dataname)
      stopifnot(is_character_single(attr))
      return(attr(private$datasets[[dataname]], attr))
    },

    #' @details
    #' Set data attribute for the dataset
    #'
    #' @param attr attribute to get from the data attributes of the dataset
    #' @param value value to set attribute to
    set_data_attr = function(dataname, attr, value) {
      private$check_data_varname(dataname)
      stopifnot(is_character_single(attr))

      return(attr(private$datasets[[dataname]], attr) <- value)
    },

    # no corresponding set_ function
    #' @details
    #' Get non-NA labels of columns in the data
    #'
    #' @md
    #' @param columns (`character` vector) columns to get labels for;
    #'   if `NULL`, for all columns
    get_column_labels = function(dataname, columns = NULL) {
      private$check_data_varname(dataname)
      stopifnot(is.null(columns) || is_character_empty(columns) || is_character_vector(columns))

      labels <- self$get_data_attr(dataname, "column_labels")
      if (!is.null(columns)) {
        labels <- labels[columns]
      }
      labels <- labels[!is.na(labels)]

      return(labels)
    },

    # filtering state and characteristics, remove and continue filtering ----

    #' @details
    #' Return filter characteristics for a variable in a dataset
    #' e.g. numeric range for a numerical variable, all levels for a
    #' factor variable.
    #' This can be used to see how the variable must be filtered
    #'
    #' @md
    #' @param all_vars `logical` (only applies if `varname` is NULL)
    #'   whether to include non-filtered variables
    get_filter_info = function(dataname, varname = NULL, all_vars = FALSE) {
      private$check_data_varname(dataname, varname)
      stopifnot(is_logical_single(all_vars))

      if (is.null(varname)) {
        # filter out variables that are not filtered
        if (all_vars) {
          private$filter_infos[[dataname]]()
        } else {
          private$filter_infos[[dataname]]()[names(private$filter_state[[dataname]])]
        }
      } else {
        private$filter_infos[[dataname]]()[[varname]]
      }
    },

    #' @details
    #' See `get_filter_info`, only returns filter type (numerical, categorical).
    #'
    #' @md
    get_filter_type = function(dataname, varname) {
      stopifnot(is_character_single(varname)) # check that varname is non-NULL
      return(self$get_filter_info(dataname, varname)[["type"]])
    },

    #' @details
    #' Filter state for a dataset (or only a variable within it).
    #'
    get_filter_state = function(dataname, varname = NULL) {
      private$check_data_varname(dataname, varname)

      if (is.null(varname)) {
        private$filter_state[[dataname]]
      } else {
        return(private$filter_state[[dataname]][[varname]])
      }
    },

    #' @details
    #' Set filter state
    #'
    #' # todo2: is the interface of this function good?
    #' The state is only updated and triggers reactive behavior when it actually
    #' changes.
    #' When `varname` is non-NULL, only the state for that variable name is changed.
    #' Otherwise, the state is entirely set to the new state.
    #'
    #' To remove all filters for a dataset, you can pass in `varname = NULL, state = list()`.
    #' To remove a filter variable for a dataset, pass in `varname, state = NULL`.
    #'
    #' @param state new state to set; when varname is NULL, state must be a named list
    #' with the new filter state for each variable (states of omitted variables are
    #' set to NULL, i.e. no filtering for these variables)
    #'
    #' @return TRUE if the state was changed
    set_filter_state = function(dataname, varname, state) {
      private$check_data_varname(dataname, varname)

      # checking and adapting arguments
      if (!is.null(varname)) {
        state <- setNames(list(state), varname)
        # we don't set varname to NULL so we can detect it below and only set the state of varname
      }
      stopifnot(is.list(state))

      # each item in the state list indicates what filter to apply to that column
      state_names <- names(state)

      # check if all names of state are columns of the dataname
      inexistent_columns <- which(!(state_names %in% names(self$get_data(dataname))))
      if (length(inexistent_columns) > 0) {
        stop(paste(
          "variables", toString(state_names[inexistent_columns]),
          "are not available in data", dataname
        ))
      }

      for (name in state_names) {
        private$check_valid_filter_state(dataname, name, var_state = state[[name]])
      }

      # get new full state
      if (is.null(varname)) {
        new_state <- state
      } else {
        # reset state for that varname only
        stopifnot(length(state) == 1)
        new_state <- self$get_filter_state(dataname)
        new_state[[varname]] <- state[[1]]
      }

      # for this to work reliably, the previous state must really capture all info
      # i.e. NA filtering or not
      # all.equal returns TRUE if all equal, otherwise character vector of differences
      if (isTRUE(all.equal(private$filter_state[[dataname]], new_state))) {
        return(FALSE)
      }

      private$previous_filter_state[[dataname]] <- private$filter_state[[dataname]]
      private$filter_state[[dataname]] <- new_state

      return(TRUE)
    },

    #' @details
    #' Get the default filter state (useful for initial UI state)
    #' This is different to NULL state which means no filter applied.
    #' Instead, this filter is a sensible default, e.g. filter out NAs.
    #' for numerics, full range; for factors, all choices etc.
    #'
    get_default_filter_state = function(dataname, varname) {
      stopifnot(is_character_single(varname))
      private$check_data_varname(dataname, varname)

      filter_info <- self$get_filter_info(dataname, varname)
      selection_state <- switch(
        filter_info$type,
        choices = filter_info$choices,
        range = filter_info$range,
        logical = "TRUE",
        stop("unknown type")
      )
      return(list(selection = selection_state, keep_na = FALSE))
    },

    #' @details
    #' Restores previous filter state (if the filter was removed in between)
    #' if there is no previous filter, it takes the filter's default state
    #'
    #' if a previous filter state exists and the state is `s`` before calling
    #' this function twice, then the state will be s again
    #'
    #' @md
    #' @return TRUE if state was changed
    restore_filter = function(dataname, varname) {
      stopifnot(is_character_single(varname))
      private$check_data_varname(dataname, varname)

      state <- private$previous_filter_state[[dataname]][[varname]]
      if (is.null(state)) {
        state <- self$get_default_filter_state(dataname, varname)
      }

      return(self$set_filter_state(dataname, varname, state))
    },

    #' @details
    #' Check whether the variable can be filtered, i.e. not of unknown type
    #'
    #' @return whether the variable can be filtered (type not unknown)
    can_be_filtered = function(dataname, varname) {
      stopifnot(is_character_single(dataname))
      stopifnot(is_character_single(varname))

      # todo2: do we want to implement some additional logic that a categorical variable cannot be filtered
      # if it has more than e.g. 1000 levels as the UI will otherwise get stuck
      return(self$get_filter_type(dataname, varname) != "unknown")
    },

    # this function needs to be public because it is used to generate R code
    #' @details
    #' Get the call to filter the dataset according to the filter state
    #'
    #' When the dataname is ADSL, it is simply the filtered ADSL.
    #' Otherwise, it is the filtered dataset that only contains subjects which
    #' are also in the filtered ADSL.
    #'
    #' Note: `merge()` function in returned call will fail if corresponding dataset is NULL.
    #'
    #' @return call to filter dataset (taking out patients not in ADSL)
    get_filter_call = function(dataname) {
      private$check_data_varname(dataname)

      filtered_alone <- if (dataname == "ADSL") {
        # for ADSL, there is no merging step involved, so no `FILTERED_ALONE`
        paste0(dataname, "_FILTERED")
      } else {
        paste0(dataname, "_FILTERED_ALONE")
      }
      filter_call <- as.call(list(
        as.name("<-"), as.name(filtered_alone),
        private$get_pure_filter_call(dataname)
      ))

      if (dataname == "ADSL") {
        return(list(filter_call))
      } else {
        # ADSL has a special status in the sense that filtering in ADSL impacts filtering of the other datasets
        # example: ADLB_FILTERED_ALONE is ADLB with filter applied
        # ADLB_FILTERED is ADLB_FILTERED_ALONE with only the (USUBJID, STUDYID) combinations appearing in ADSL_FILTERED
        # todo2: instead of a merge call, it would be simpler to just check %in% USUBJID assuming a single studyid
        filtered_joined_adsl <- paste0(dataname, "_FILTERED") # filtered_alone with

        keys <- self$get_data_attr("ADSL", "keys")$primary
        stopifnot(!is.null(keys))
        # filter additionally to only have combinations of keys that are in ADSL_FILTERED
        merge_call <- call(
          "<-", as.name(filtered_joined_adsl),
          call(
            "merge",
            x = call("[", as.name("ADSL_FILTERED"), quote(expr = ), keys), # nolint
            y = as.name(filtered_alone),
            by = keys,
            all.x = FALSE, all.y = FALSE
          )
        )

        return(list(filter_call, merge_call))
      }
    },

    # info functions for end user ----

    #' @details
    #' Prints filter characteristics of each variable for a given dataset to the
    #' console.
    #'
    #' @md
    #' @param filtered_vars_only `logical` whether to only consider filtered
    #'   vars or unfiltered as well
    #' @param variables (`character` vector) variables to consider; if NULL, takes all
    print_data_info = function(dataname, filtered_vars_only = FALSE, variables = NULL) {
      private$check_data_varname(dataname)
      stopifnot(is_logical_single(filtered_vars_only))
      stopifnot(is.null(variables) || is_character_vector(variables))

      # log with newline
      log2 <- function(...) {
        cat(paste(..., collapse = " "))
        cat("\n")
      }

      log2("====", dataname, "=======================")

      df <- self$get_data(dataname)
      if (is.null(df)) {
        log2("Data", dataname, "data is NULL") # not yet set with set_data
      } else {
        varnames <- if (filtered_vars_only) {
          names(self$get_filter_state(dataname))
        } else {
          names(df)
        }
        if (!is.null(variables)) {
          varnames <- intersect(varnames, variables)
        }

        filter_infos <- self$get_filter_info(dataname, all_vars = TRUE)
        var_maxlength <- max(nchar(varnames))
        for (name in varnames) {
          var_chars <- filter_infos[[name]]

          info <- switch(
            var_chars$type,
            range = paste(var_chars$range, collapse = " - "),
            choices = {
              if (length(var_chars$choices) > 5) {
                paste0(toString(var_chars$choices[1:5]), ", ...")
              } else {
                toString(var_chars$choices)
              }
            },
            logical = toString(var_chars$choices),
            "" # no special info when filter type is unknown
          )
          log2(sprintf(paste0("%-", var_maxlength, "s has filter type %-10s: %s"), name, var_chars$type, info))
        }
      }
      log2("===========================")
    },

    #' @details
    #' Get info about dataname, e.g. number of patients
    #' returned in a list
    #'
    get_data_info = function(dataname, filtered = TRUE) {
      stopifnot(is_character_single(dataname))
      stopifnot(is_logical_single(filtered))

      data <- self$get_data(dataname, filtered = filtered)

      keys <- self$get_data_attr(dataname, "keys")
      unique_patient_colnames <- if_null(keys$foreign, keys$primary)
      # foreign is NULL if ADSL, otherwise foreign refers to primary keys of ADSL
      stopifnot(!is.null(unique_patient_colnames))

      list(
        name = dataname,
        dim = dim(data),
        patients = nrow(dplyr::distinct(data[unique_patient_colnames]))
      )
    },

    # functions related to class attributes ----

    # todo1: remove eventually
    #' @details
    #' Temporary access to private validation function
    validate_temp = function() {
      return(private$validate())
    }
  ), # end of public functions

  ## __Private Methods---------------------
  private = list(

    # private attributes ----

    # the following attributes are (possibly reactive lists) per dataname
    datasets = NULL, # unfiltered datasets, attributes per dataname are possible like `keys`, `data_label` etc.
    filtered_datasets = NULL, # stores reactive which return filtered dataset after applying filter to unfiltered dataset
    # filter to apply to obtain filtered dataset from unfiltered one, NULL (for a dataname) means
    # no filter applied: it does not mean that it does not show up as a filtering element,
    # NULL just means that filtering has no effect
    # therefore, the following lists may not be complete and simply not contain all varnames
    # for a given dataname, which means that the state is NULL
    filter_state = NULL,
    # previous filter state when you want to revert, e.g. when filter previously removed is added again
    # explicitly not reactive, but kept in this class because it may need to be synced with filter_state,
    # e.g. discarded when it becomes obsolete with hierarchical filtering
    previous_filter_state = NULL,
    # filter characteristics to create filter that has no effect (i.e. full range of variable),
    # useful for UI to show sensible ranges
    filter_infos = NULL,

    code = NULL, # code used to generate the unfiltered datasets as a string

    # check functions ----
    # we implement these functions as checks rather than returning logicals so they can
    # give informative error messages immediately

    #' @details
    #' Validate object to inspect if something is wrong
    #'
    validate = function() {
      stopifnot(
        # check classes
        is.reactivevalues(private$datasets),
        is.list(private$filtered_datasets),
        is.reactivevalues(private$filter_state),
        is.list(private$previous_filter_state),
        is.reactivevalues(private$filter_infos),

        # check names are the same
        setequal(names(private$datasets), names(private$filtered_datasets)),
        setequal(names(private$datasets), names(private$filter_infos)),
        # iterating over reactive values: should use reactiveValuesToList first
        all_true(self$datanames(), function(dataname) !is.null(attr(private$datasets[[dataname]], "md5sum"))),
        setequal(names(private$datasets), names(private$filter_state)),
        setequal(names(private$datasets), names(private$previous_filter_state))
      )

      # check filter_state
      lapply(names(private$filter_state), function(dataname) {
        lapply(names(private$filter_state[[dataname]]), function(varname) {
          private$check_valid_filter_state(
            dataname,
            varname = varname,
            var_state = private$filter_state[[dataname]][[varname]]
          )
        })
      })

      # check previous_filter_state
      lapply(names(private$previous_filter_state), function(dataname) {
        lapply(names(private$previous_filter_state[[dataname]]), function(varname) {
          private$check_valid_filter_state(
            dataname,
            varname = varname,
            var_state = private$previous_filter_state[[dataname]][[varname]]
          )
        })
      })

      return(invisible(NULL))
    },

    #' @details
    #' Checks if dataname is in datanames (of this class) and
    #' (if provided) that varname is a valid column in data
    #'
    #' If data for dataname was not set yet, the check will also fail.
    check_data_varname = function(dataname, varname = NULL) {
      stopifnot(is_character_single(dataname))
      stopifnot(is.null(varname) || is_character_single(varname))

      isolate({
        # we isolate everything because we don't want to trigger again when datanames change
        if (!(dataname %in% self$datanames())) { # data must be set already
          stop(paste("data", dataname, "is not available"))
        }
        if (!is.null(varname) && !(varname %in% names(self$get_data(dataname)))) {
          stop(paste("variable", varname, "is not in data", dataname))
        }
      })

      return(invisible(NULL))
    },

    #' @details
    #' Checks that the given filter state is valid for the specified
    #' variable in the specified dataset.
    #'
    check_valid_filter_state = function(dataname, varname, var_state) {
      stopifnot(is_character_single(varname))
      private$check_data_varname(dataname, varname)

      var_char <- self$get_filter_info(dataname, varname) # filter characteristics for var, e.g. range

      if (is.null(var_state)) {
        # when state is NULL, this means no filtering is applied
        return()
      }

      if (!is_logical_single(var_state$keep_na)) {
        stop(
          "data", dataname, "variable", varname, ":",
          "NA selection must be one of TRUE, FALSE, but is ", var_state$keep_na
        )
      }

      selection_state <- var_state$selection
      switch(
        var_char$type,
        choices = {
          if (any(!(selection_state %in% var_char$choices))) {
            stop(paste(
              "data", dataname, "variable", varname, ":", "choices (", toString(selection_state),
              ") not all in valid choices (", toString(var_char$choices), ")"
            ))
          }
        },
        range = {
          if ((length(selection_state) != 2) ||
            (selection_state[1] > selection_state[2]) ||
            ((selection_state[1] < var_char$range[1]) && (selection_state[2] > var_char$range[2]))
          ) {
            stop(paste(
              "data", dataname, "variable", varname, "range (", toString(selection_state),
              ") not valid for full range (", toString(var_char$range), ")"
            ))
          }
        },
        logical = {
          # the conceptual difference to type 'choices' is that it allows exactly one value rather than a subset
          stopifnot(length(selection_state) == 1)

          if (!(selection_state %in% var_char$choices)) {
            stop(paste(
              "data", dataname, "variable", varname, "choices (", toString(selection_state),
              ") not in valid choices (", toString(var_char$choices), ")"
            ))
          }
        },
        stop(paste("Unknown filter type", var_char$type, "for data", dataname, "and variable", varname))
      )
      return(invisible(NULL))
    },

    #' @details
    #' Creates a call that filters the dataset to obtain the filtered dataset.
    #' `get_filter_call` calls this function to filter out patients that are not in
    #' the filtered ADSL.
    #'
    #' @md
    get_pure_filter_call = function(dataname) {
      private$check_data_varname(dataname)

      data_filter_call_items <- Map(
        function(filter_info, filter_state, varname) {
          stopifnot(!is.null(filter_state)) # filter_state only contains non-NULL values

          selection_state <- filter_state$selection
          type <- filter_info$type
          if (is.null(type)) {
            # such a filter should never have been set
            stop(paste("filter type for variable", varname, "in", dataname, "not known"))
          }

          filter_call <- switch(
            type,
            choices = {
              if (length(selection_state) == 1) {
                call("==", as.name(varname), selection_state)
              } else {
                call("%in%", as.name(varname), selection_state)
              }
            },
            range = call(
              "&",
              call(">=", as.name(varname), selection_state[1]),
              call("<=", as.name(varname), selection_state[2])
            ),
            logical = {
              stopifnot(length(selection_state) == 1)
              switch(
                selection_state,
                "TRUE" = as.name(varname),
                "FALSE" = call("!", as.name(varname)),
                stop(
                  "Unknown filter state", toString(selection_state),
                  " for logical var ", varname, " in data", dataname
                )
              )
            },
            stop(paste("filter type for variable", varname, "in", dataname, "not known"))
          )
          # allow NA as well, i.e. do not filter it out
          if (filter_state$keep_na && (
            !is.null(filter_info$na_count) && (filter_info$na_count > 0)
          )) {
            # only add is.na call if the var really has NA values
            filter_call <- call("|", filter_call, call("is.na", as.name(varname)))
          }
          return(filter_call)
        },
        self$get_filter_info(dataname),
        self$get_filter_state(dataname),
        names(self$get_filter_info(dataname))
      )
      data_filter_call_items <- Filter(function(x) !is.null(x), data_filter_call_items)

      if (length(data_filter_call_items) == 0) {
        # no filtering applied to the dataset
        return(as.name(dataname))
      } else {
        # concatenate with "&" when several filters need to be applied to this dataset
        # when the list has a single element, reduce simply returns the element
        # extra parentheses () must be put around x and y if more than one elem
        if (length(data_filter_call_items) > 1) {
          data_filter_call_items <- lapply(data_filter_call_items, function(item) call("(", item))
        }
        combined_filters <- Reduce(function(x, y) call("&", x, y), data_filter_call_items)

        # do not use subset (this is a convenience function intended for use interactively, stated in the doc)
        # as a result of subset and filter, NA is filtered out; this can be circumvented by explicitly
        # adapting the filtering condition above to watch out for NAs and keep them
        return(call("subset", as.name(dataname), combined_filters))
      }
    },

    # generate reactive expressions for reactive attributes that depend on data ----

    #' @details
    #' this returns a reactive that returns the filtered dataset
    #' it refilters whenever the source datasets
    #' change or the filter state
    reactive_filtered_dataset = function(dataname) {
      stopifnot(is_character_single(dataname))
      reactive({
        if (!dataname %in% isolate(self$datanames())) { # todo2: isolate to avoid triggering due to set_data
          stop("Cannot filter data ", dataname, " as it needs to be set first")
        }

        .log("################################################# Refiltering dataset ", dataname)

        # filter data directly in an empty environment to make sure no global variables or
        # other variables from this class are used
        # todo2: why not use a chunks object for this from teal.devel (and put chunks code into teal?)
        # packages bind right before globalenv(), so we don't accidentally pick up other packages
        env <- new.env(parent.env(globalenv()))

        # put dependencies of filter call into environment
        if (dataname != "ADSL") {
          # need to add ADSL_filtered as filter call for dataset depends on it
          env[["ADSL_FILTERED"]] <- self$get_data("ADSL", filtered = TRUE)
        }
        env[[dataname]] <- self$get_data(dataname, filtered = FALSE)

        lapply(
          self$get_filter_call(dataname),
          eval,
          envir = env
        )

        return(env[[paste0(dataname, "_FILTERED")]])
      })
    },

    #' @details
    #' Sets filter chars
    #'
    #' Detect variable type (categorical, numeric range)
    #' and accordingly set the filter characteristics
    reactive_filter_infos = function(dataname) {
      reactive({
        df <- self$get_data(dataname, filtered = FALSE)

        filter_info <- Map(function(var, varname) {
          # add counts per variable as names for UI
          add_counts <- function(choices) {
            setNames(choices, paste0(choices, " (", as.vector(table(var)[choices]), ")"))
          }

          res <- if (all(is.na(var))) {
            .log("all elements in", varname, "are NA")
            list(
              type = "unknown",
              label = if_null(attr(var, "label"), ""),
              class = class(var)
            )
          } else if (is.factor(var) || is.character(var)) {
            list(
              type = "choices",
              label = if_null(attr(var, "label"), ""),
              choices = add_counts(if (is.factor(var)) {
                levels(var)
              } else {
                sort(unique(as.character(var)))
              })
            )
          } else if (is.numeric(var)) {
            list(
              type = "range",
              label = if_null(attr(var, "label"), ""),
              range = range(var, na.rm = TRUE)
            )
          } else if (is.logical(var)) {
            list(
              type = "logical",
              label = if_null(attr(var, "label"), ""),
              choices = add_counts(c("TRUE", "FALSE"))
            )
          } else {
            .log(
              "variable '", varname, "' is of class '",
              class(var), "' which has currently no filter UI element",
              sep = ""
            )
            list(
              type = "unknown",
              label = if_null(attr(var, "label"), ""),
              class = class(var)
            )
          }
          res$na_count <- sum(is.na(var))
          return(res)
        }, df, names(df))

        return(setNames(filter_info, names(df)))
      })
    }
  )
)
