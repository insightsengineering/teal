# todo1:  rename to FilteredDatasets

# todo1: not working
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
#' By the design of the class, datanames must be provided in the beginning. They cannot
#' be changed.
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
#' \dontrun{
#' # if on BEE
#' path <- "/opt/BIOSTAT/qa/cdt7876a/libraries/asl.sas7bdat"
#' x <- FilteredData$new()
#' x$load_data(path, dataname = "ADSL")
#'
#' # todo1: should be able to remove dontrun starting from here, but strict complains about
#' # attempt to call non-function
#' library(random.cdisc.data)
#'
#' ADSL <- radsl(cached = TRUE)
#' x <- teal:::FilteredData$new(datanames = c("ADSL", "ADAE"))
#'
#' isolate({
#'   x$set_data("ADSL", ADSL)
#' })
#'
#' isolate({
#'   x$datanames()
#'   x$list_data_info("ADSL")
#'   x$get_filter_chars("ADSL")
#'   df <- x$get_data("ADSL")
#'   # df
#'
#'   x$get_filter_type("ADSL", "SEX")
#'   x$set_filter_state("ADSL", varname = NULL, state = list(
#'     AGE = list(selection = c(3, 5), keep_na = FALSE),
#'     SEX = list(selection = c("M", "F"), keep_na = FALSE)
#'   ))
#'   x$get_filter_type("ADSL", "SEX")
#'   x$get_filter_chars("ADSL")[["SEX"]]$type
#'
#'   x$hold_filtering()
#'   x$set_filter_state("ADSL", varname = NULL, list(
#'     AGE = list(selection = c(3, 7), keep_na = FALSE)
#'   ))
#'   x$set_filter_state(
#'     "ADSL",
#'     varname = "SEX",
#'     state = list(selection = c("M", "F"), keep_na = FALSE)
#'   )
#'   x$continue_filtering()
#'   x$get_filter_type("ADSL", "SEX")
#'
#'   x$get_filter_state("ADSL")
#' })
#' }
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
    #' @param datanames (vector of `character`) names of datasets that can
    #'   be added to this class with `set_data`, cannot be changed
    #'
    initialize = function(datanames = c("ADSL")) {
      stopifnot(is_character_vector(datanames))
      # this is assumed in many functions below which only filter once ADSL dataset has been set with set_data
      stopifnot("ADSL" %in% datanames)
      datanames_with_spaces <- grep("[[:space:]]", datanames)
      if (length(datanames_with_spaces) > 0) {
        stop(paste0("dataname '", datanames, "' must not contain spaces.", collapse = "\n"))
      }

      create_rv <- function() {
        do.call(reactiveValues, setNames(lapply(datanames, function(x) NULL), datanames))
      }
      # create reactiveValues for unfiltered and filtered dataset and filter state
      # each reactiveValues is a list with one entry per dataset name
      private$datasets <- create_rv() # make it reactive so it triggers when dataset changed with set_data
      private$filtered_datasets <- setNames(lapply(datanames, private$reactive_filtered_dataset), datanames)
      private$filter_state <- create_rv()
      private$filter_chars <- create_rv()

      private$previous_filter_state <- setNames(lapply(datanames, function(x) NULL), datanames)

      # set default values, i.e. empty lists or NULL
      # using isolate is safe here because the class is just getting initialized
      # and attributes cannot yet be accessed
      isolate(self$reset_class_vars())

      return(invisible(self))
    },

    #' @details
    #' Get (active) datanames
    #'
    #' If `include_unset` is set, it will return all possible datanames (as specified
    #' in `\link{initialize}`). Otherwise, it will just return the datanames of already
    #' set datasets using the method `\link{set_data}`.
    #'
    #' The former can never change and is therefore isolated. The latter case can change
    #' as new datasets are added and is therefore reactive and needs to be isolated
    #' if necessary.
    #'
    #' @md
    #' @param include_unset `logical` whether to also return datasets that were
    #'   not set yet, i.e. still NULL
    datanames = function(include_unset = FALSE) {
      stopifnot(is_logical_single(include_unset))
      if (include_unset) {
        # it is safe to use isolate here because by the design of the class,
        # the datanames cannot change
        return(isolate(names(private$datasets)))
      } else {
        # unset datasets are NULL
        return(Filter(
          function(dataname) !is.null(private$datasets[[dataname]]),
          names(private$datasets)
        ))
      }
    },

    # load data ----

    #' @details
    #' Load data from path into dataname
    #'
    #' @md
    #' @param path `character` path to load dataset from
    #' @param dataname dataname, if `NULL`, it is inferred from the basename of the path
    #' @param ... additional parameters passed to `read_` functions
    #'
    load_data = function(path, dataname = NULL, ...) {
      if (!file.exists(path)) {
        stop(paste("invalid path:", path))
      }
      dataname <- if (is.null(dataname)) {
        file_path_sans_ext(basename(path))
      } else {
        dataname
      }
      # currently only allow datanames that were defined in initialize
      private$check_data_varname(dataname)

      logger_in()
      .log("load data:", path)

      path <- normalizePath(path, mustWork = TRUE)
      df <- switch(tolower(file_ext(path)),
        sas7bdat = read_sas(path, ...),
        csv      = read_csv(path, ...),
        rds      = readRDS(path, ...),
        stop(paste("The format of", path, "is currently not supported."))
      )

      attr(df, "path") <- path
      attr(df, "last_modified") <- file.info(path)$mtime[1]

      .log("load data", dataname)
      logger_out()

      return(self$set_data(dataname, df))
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
        if (private$filter_on_hold) {
          stop("You have to resume filtering first to get the filtered data.")
        }
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
    #' Will also add the `md5` sum of the data
    #'
    #' @md
    #' @param dataname `character` name  of the data
    set_data = function(dataname, data) {
      stopifnot(is_character_single(dataname), dataname %in% self$datanames(include_unset = TRUE))
      stopifnot(is.data.frame(data))

      self$reset_class_vars(dataname)

      private$datasets[[dataname]] <- data
      self$set_data_attr(dataname, "md5sum", digest(data, algo = "md5"))
      private$update_filter_chars(dataname)

      return(invisible(self))
    },

    #' @details
    #' Get attributes of `self` (in dedicated field)
    #' (not equivalent to `attributes(self)`)
    #'
    #' @md
    get_attrs = function() {
      return(private$attrs)
    },

    #' @details
    #' Set attributes field of `self`
    #' (not equivalent to `attributes(self)`)
    #'
    #' @md
    set_attrs = function(data) {
      private$attrs <- attributes(data)
      return(invisible(self))
    },

    # no corresponding set_ function
    #' @details
    #' Get attribute in the `attrs` field
    #'
    #' @md
    #' @param attr attribute to get in the `attrs` field
    get_attr = function(attr) {
      stopifnot(is_character_single(attr))
      return(private$attrs[[attr]])
    },

    # no corresponding set_ function
    #' @details
    #' Get name of dataset (to display in the UI)
    get_data_label = function(dataname) {
      stopifnot(is_character_single(dataname))
      return(self$get_data_attr(dataname, "data_label"))
    },

    #' @details
    #' Get data attribute for the dataset
    #'
    #' @param attr attribute to get from the data attributes of the dataset
    get_data_attr = function(dataname, attr) {
      stopifnot(is_character_single(dataname))
      stopifnot(is_character_single(attr))
      return(private$data_attrs[[dataname]][[attr]])
    },

    #' @details
    #' Set data attribute for the dataset
    #'
    #' @param attr attribute to get from the data attributes of the dataset
    #' @param value value to set attribute to
    set_data_attr = function(dataname, attr, value) {
      stopifnot(is_character_single(dataname))
      stopifnot(is_character_single(attr))

      private$data_attrs[[dataname]][[attr]] <- value
      return(private$data_attrs[[dataname]][[attr]])
    },

    # no corresponding set_ function
    #' @details
    #' Get data attributes for the dataset
    get_data_attrs = function(dataname) {
      stopifnot(is_character_single(dataname))
      return(private$data_attrs[[dataname]])
    },

    # no corresponding set_ function
    #' @details
    #' Get non-NA labels of columns in the data
    #'
    #' @md
    #' @param columns (`character` vector) columns to get labels for;
    #'   if `NULL`, for all columns
    get_column_labels = function(dataname, columns = NULL) {
      stopifnot(is_character_single(dataname))
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
    get_filter_chars = function(dataname, varname = NULL, all_vars = FALSE) {
      private$check_data_varname(dataname, varname)
      stopifnot(is_logical_single(all_vars))

      if (is.null(varname)) {
        # filter out variables that are not filtered
        if (all_vars) {
          private$filter_chars[[dataname]]
        } else {
          private$filter_chars[[dataname]][names(private$filter_state[[dataname]])]
        }
      } else {
        private$filter_chars[[dataname]][[varname]]
      }
    },

    #' @details
    #' See `get_filter_chars`, only returns filter type (numerical, categorical).
    #'
    #' @md
    get_filter_type = function(dataname, varname) {
      stopifnot(is_character_single(varname))
      return(self$get_filter_chars(dataname, varname)[["type"]])
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
    #' The state is only updated and triggers reactive behavior when it actually
    #' changes.
    #'
    #' @param state new state to set; when varname is NULL, state must be a named list
    #' with the new filter state for each variable that should be affected.
    #'
    #' @return TRUE if the state was changed
    set_filter_state = function(dataname, varname, state) {
      private$check_data_varname(dataname, varname)

      # checking and adapting arguments
      # todo1: do not allow varname = NULL
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

      filter_char <- self$get_filter_chars(dataname, varname)
      selection_state <- switch(
        filter_char$type,
        choices = filter_char$choices,
        range = filter_char$range,
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
    #' Removes the filter for dataname (and varname)
    #' If varname is not provided, resets it for the whole dataset.
    #'
    remove_filter = function(dataname, varname) {
      # The following optimization *cannot* be implemented, but erroneously was in the past.
      # When a filter is removed, instead of trashing
      # the filtered dataset, it is kept in memory. When the filter is added back, it still
      # has the same filtering applied as when it was removed. The filtered dataset still kept
      # in memory can be used again. This is not possible because ADSL filtering may have changed in the
      # meantime, thus affecting the filtering of the dataset.
      return(self$set_filter_state(dataname, varname, NULL))
    },

    #' @details
    #' Check whether the variable can be filtered, i.e. not of unknown type
    #'
    #' @return whether the variable can be filtered (type not unknown)
    can_be_filtered = function(dataname, varname) {
      stopifnot(is_character_single(dataname))
      stopifnot(is_character_single(varname))

      return(self$get_filter_type(dataname, varname) != "unknown")
    },

    # this function needs to be public because it is used to generate R code
    #' @details
    #' Get the call to filter the dataset according to the filter state
    #'
    #' Note: `merge()` function in returned call will fail if corresponding dataset is NULL.
    #'
    #' @param merge whether to include merge call that removes any keys from filtered data that
    #'   are not also in filtered ADSL afterwards
    #' @param adsl whether to include `ADSL` filtering call before
    #'
    #' @return call to filter dataset (taking out patients not in ADSL)
    get_filter_call = function(dataname, merge = TRUE, adsl = TRUE) {
      private$check_data_varname(dataname)
      stopifnot(is_logical_single(merge))
      stopifnot(is_logical_single(adsl))
      if (!merge) {
        # todo1: remove once these warnings no longer appear
        warning("Set merge to TRUE or add doc to this function why it is needed")
      }
      if (adsl) {
        # todo1: remove once these warnings no longer appear
        warning("Set adsl to FALSE or add doc to this function why it is needed")
      }

      adsl_filter_call <- private$get_filter_call_no_adsl("ADSL", "ADSL_FILTERED")

      if (dataname == "ADSL") {
        list(adsl_filter_call)
      } else {
        # ADSL has a special status in the sense that filtering in ADSL impacts filtering in the other datasets
        # example: ADLB_FILTERED_ALONE is ADLB with filter applied
        # ADLB_FILTERED is ADLB_FILTERED_ALONE with only the (USUBJID, STUDYID) combinations appearing in ADSL_FILTERED
        filtered_alone <- paste0(dataname, "_FILTERED_ALONE")
        filtered_joined_adsl <- paste0(dataname, "_FILTERED") # filtered_alone with

        filter_call <- private$get_filter_call_no_adsl(dataname, filtered_alone)
        keys <- self$get_data_attrs("ADSL")$keys$primary
        if (is.null(keys)) {
          # todo1: should this ever happen because of backwards compatibility?
          keys <- c("USUBJID", "STUDYID")
        }
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

        calls <- list(adsl_filter_call, filter_call, merge_call)

        return(if (merge && adsl) {
          calls
        } else if (merge && !adsl) {
          calls[2:3]
        } else if (!merge && adsl) {
          calls[1:2]
        } else {
          calls[[2]]
        })
      }
    },

    #' @details
    #' Pause the filtering until reactivated with `continue_filtering` to make several
    #' changes to the filter state and avoid refilterings in between that are of no interest.
    #'
    hold_filtering = function() {
      private$filter_on_hold <- TRUE
      return(invisible(NULL))
    },

    #' @details
    #' Continue filtering after it was paused with `hold_filtering`
    #' Can result in several datasets being filtered
    #'
    continue_filtering = function() {
      private$filter_on_hold <- FALSE
      return(invisible(NULL))
    },

    # info functions for end user ----

    #' @details
    #' Shows filter characteristics of each variable for a given dataset
    #' will intersect variables with actual variables in dataname if non-NULL
    #'
    #' @md
    #' @param filtered_vars_only `logical` whether to only consider filtered
    #'   vars or unfiltered as well
    #' @param variables (`character` vector) variables to consider; if NULL, takes all
    list_data_info = function(dataname, filtered_vars_only = FALSE, variables = NULL) {
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

        filter_chars <- self$get_filter_chars(dataname, all_vars = TRUE)
        var_maxlength <- max(nchar(varnames))
        for (name in varnames) {
          var_chars <- filter_chars[[name]]

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

    #' @details
    #' Reset class attributes
    #'
    #' Set class variables to default values, i.e. empty lists or `NULL` per dataname
    #' The following attributes are not set: `attr`, `filter_on_hold`
    #' The contents of the fields with `reactiveValues` are set to `NULL`, but the
    #' `reactiveValues` itself it not set to a new `reactiveValues` to keep reactive
    #' dependencies.
    #'
    #' If dataname is provided, only do it for this dataset. In  case of ADSL, will
    #' reset all datasets.
    reset_class_vars = function(dataname = NULL) {
      if (!is.null(dataname)) {
        # cannot use private$check_data_varname because data may not have been set
        stopifnot(is_character_single(dataname))
      }

      # if dataset is ADSL, we need to reset filtered datasets
      # to simplify, here we simply reset the other datasets as well then
      datanames <- if (is.null(dataname) || (dataname == "ADSL")) {
        self$datanames(include_unset = TRUE) # delete all datasets, include_unset to be safe
      } else {
        dataname
      }

      # do not recreate reactiveValues here because reactivity will be lost otherwise
      # just set to NULL so the observers can see that the dataset was removed or
      # will be replaced.
      for (name in datanames) {
        private$datasets[[name]] <- NULL
        # for each varname, there is a state and filter info
        # therefore, a list makes sure that the $ operator works
        private$filter_chars[[name]] <- list()
        private$filter_state[[name]] <- list()
        private$previous_filter_state[[name]] <- list()
        private$data_attrs[[name]] <- list()
      }

      return(invisible(self))
    },

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
    datasets = NULL, # unfiltered datasets
    filtered_datasets = NULL, # stores reactive which return filtered dataset after applying filter to unfiltered dataset
    data_attrs = NULL, # attributes per dataname
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
    filter_chars = NULL,

    # whether filtering is currently performed or it is postponed, useful when several fields
    # are changed at once and it should not run for each of the steps, but just once at the end
    filter_on_hold = FALSE,
    attrs = list(), # attributes of this class retrievable with get_attrs and set_attrs

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
        is.list(private$data_attrs),
        is.reactivevalues(private$filter_state),
        is.list(private$previous_filter_state),
        is.list(private$filter_chars),
        is_logical_single(private$filter_on_hold),
        is.list(private$attrs),

        # check names are the same
        all(names(private$datasets) == names(private$data_attrs)),
        all(names(private$datasets) == names(private$filtered_datasets)),
        all(names(private$datasets) == names(private$filter_chars)),
        all(names(private$datasets) == names(private$data_attrs)),
        all_true(private$data_attrs, function(attrs) !is.null(attrs[["md5sum"]])),
        all(names(private$datasets) == names(private$filter_state)),
        all(names(private$datasets) == names(private$previous_filter_state))
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

      if (!(dataname %in% self$datanames())) { # data must be set already
        stop(paste("data", dataname, "is not available"))
      }
      if (!is.null(varname) && !(varname %in% names(self$get_data(dataname)))) {
        stop(paste("variable", varname, "is not in data", dataname))
      }

      return(invisible(NULL))
    },

    #' @details
    #' Checks that the given filter state is valid for the specified
    #' variable in the specified dataset.
    #'
    check_valid_filter_state = function(dataname, varname, var_state) {
      stopifnot(is_character_single(varname))
      private$check_data_varname(dataname, varname)

      var_char <- self$get_filter_chars(dataname, varname) # filter characteristics for var, e.g. range

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

    # is private because it should not be user set
    #' @details
    #' Detect variable type (categorical, numeric range)
    #' and accordingly set the filter characteristics
    #'
    update_filter_chars = function(dataname) {
      df <- self$get_data(dataname)

      filter_char <- Map(function(var, varname) {
        # add counts per variable as names for UI
        add_counts <- function(choices) {
          setNames(choices, paste0(choices, " (", as.vector(table(var)[choices]), ")"))
        }

        res <- if (all(is.na(var))) {
          .log("all elements in", varname, "are NA")
          list(
            type = "unknown",
            label = "", # todo1: really empty here?
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
            label = "", # todo1: really empty here?
            class = class(var)
          )
        }
        res$na_count <- sum(is.na(var))
        return(res)
      }, df, names(df))

      private$filter_chars[[dataname]] <- setNames(filter_char, names(df))
      return(private$filter_chars[[dataname]])
    },

    # todo1: maybe remove out_dataname or default to NULL (no assignment)
    #' @details
    #' Creates a call that filters the dataset to obtain the filtered dataset and
    #' assigns it to a new variable.
    #' As opposed to `get_filter_call`, this function does not filter for patient for
    #' patients in ADSL.
    #'
    #' @md
    #' @param dataname_out `character` name of dataset to assign filtered dataset to
    get_filter_call_no_adsl = function(dataname, dataname_out) {
      private$check_data_varname(dataname)
      stopifnot(is_character_single(dataname_out))

      data_filter_call_items <- Map(
        function(filter_char, filter_state, varname) {
          stopifnot(!is.null(filter_state)) # filter_state only contains non-NULL values

          selection_state <- filter_state$selection
          type <- filter_char$type
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
            !is.null(filter_char$na_count) && (filter_char$na_count > 0)
          )) {
            # only add is.na call if the var really has NA values
            filter_call <- call("|", filter_call, call("is.na", as.name(varname)))
          }
          return(filter_call)
        },
        self$get_filter_chars(dataname),
        self$get_filter_state(dataname),
        names(self$get_filter_chars(dataname))
      )
      data_filter_call_items <- Filter(function(x) !is.null(x), data_filter_call_items)

      if (length(data_filter_call_items) == 0) {
        # no filtering applied to the dataset
        return(call("<-", as.name(dataname_out), as.name(dataname)))
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
        return(as.call(list(as.name("<-"), as.name(dataname_out), call("subset", as.name(dataname), combined_filters))))
      }
    },

    #' @details
    #' this returns a reactive that returns the filtered dataset
    #' it refilters whenever the source datasets
    #' change or the filter state
    reactive_filtered_dataset = function(dataname) {
      stopifnot(is_character_single(dataname))
      reactive({
        if (private$filter_on_hold) {
          # todo1: should make on_hold reactive, we don't make on_hold reactive so that the filtered datasets are not invalidated
          # todo1: is on_hold still needed, should only be called at beginning of the app when no reactive listeners are there yet?
          # todo1: these functions currently don't work: hold_filtering, continue_filtering
          stop("You have to resume filtering first")
        }
        if (!dataname %in% isolate(self$datanames())) { # todo: isolate to avoid triggering due to set_data
          stop("Cannot filter data ", dataname, " as it needs to be set first")
        }

        .log("Refiltering dataset ", dataname)

        # filter data directly in an empty environment to make sure no global variables or
        # other variables from this class are used
        # todo1: why not use a chunks object for this from teal.devel (and put chunks code into teal?)
        # packages bind right before globalenv(), so we don't accidentally pick up other packages
        env <- new.env(parent.env(globalenv()))

        # put dependencies of filter call into environment
        if (dataname != "ADSL") {
          # need to add ADSL_filtered as filter call for dataset depends on it
          env[["ADSL_filtered"]] <- self$get_data("ADSL", filtered = TRUE)
        }
        env[[dataname]] <- self$get_data(dataname, filtered = FALSE)

        lapply(
          self$get_filter_call(dataname, merge = TRUE, adsl = FALSE),
          eval,
          envir = env
        )

        return(env[[paste0(dataname, "_FILTERED")]])
      })
    }
  )
)
