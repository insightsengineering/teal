# todo1:  rename to FilteredDatasets

#' @name FilteredData
#' @docType class
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
#' }
#'
#' ADSL <- radsl(cached = TRUE)
#' x <- FilteredData$new(datanames = c("ADSL", "ADAE"))
#'
#' isolate({
#'   x$set_data("ADSL", ADSL)
#' })
#'
#' x$datanames()
#' isolate({
#'   x$list_data_info("ADSL")
#'   x$get_filter_chars("ADSL")
#'   df <- x$get_data("ADSL")
#'   # df
#'
#'   x$get_filter_type("ADSL", "SEX")
#'   x$set_filter_state("ADSL", varname = NULL, state = list(AGE = c(3, 5), SEX = c("M", "F")))
#'   x$get_filter_type("ADSL", "SEX")
#'   x$get_filter_chars("ADSL")[["SEX"]]$type
#'
#'   x$hold_filtering()
#'   x$set_filter_state("ADSL", varname = NULL, list(AGE = c(3, 7)))
#'   x$set_filter_state("ADSL", varname = "SEX", state = c("M", "F"))
#'   x$continue_filtering()
#'   x$get_filter_type("ADSL", "SEX")
#'
#'   x$get_filter_state("ADSL")
#' })
FilteredData <- R6::R6Class( # nolint
  "FilteredData",
  ## FilteredData ====
  ## __Public Methods ====
  public = list(

    # It is initialized with the allowed datanames. The actual datasets can afterwards
    # be added using the set_data function.
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
      private$datasets <- create_rv()
      private$filtered_datasets <- create_rv()
      private$filter_state <- create_rv()
      private$previous_filter_state <- create_rv()
      private$filter_chars <- create_rv()

      # set default values, i.e. empty lists or NULL
      # using isolate is safe here because the class is just getting initialized
      # and attributes cannot yet be accessed
      isolate(self$reset_class_vars())

      return(invisible(self))
    },

    datanames = function() {
      # it is safe to use isolate here because by the design of the class,
      # the datanames cannot change
      return(isolate(names(private$datasets)))
    },

    # load data ----

    # load data as "dataname" from specified path and set
    # attributes for path and mtime
    # dataname is inferred from basename of path if not provided
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

    # filtered: return filtered or full dataset
    get_data = function(dataname, filtered = FALSE) {
      private$check_data_varname(dataname)
      stopifnot(is_logical_single(filtered))

      if (filtered) {
        if (private$filter_on_hold) {
          stop("You have to resume filtering first to get the filtered data.")
        }
        private$filtered_datasets[[dataname]]
      } else {
        private$datasets[[dataname]]
      }
    },

    # add data into dataname slot together with md5 sum
    set_data = function(dataname, data) {
      stopifnot(is_character_single(dataname))
      stopifnot(is.data.frame(data))

      self$reset_class_vars(dataname)

      private$datasets[[dataname]] <- data
      self$set_data_attr(dataname, "md5sum", digest(data, algo = "md5"))
      private$update_filter_chars(dataname)
      private$apply_filter(dataname)

      return(invisible(self))
    },

    # get info in the attributes field of self (not equivalent to attributes(self))
    get_attrs = function() {
      return(private$attrs)
    },

    # set info in the attributes field of self (not equivalent to attributes(self))
    # from the attributes in data
    set_attrs = function(data) {
      private$attrs <- attributes(data)
      return(invisible(self))
    },

    # no corresponding set_ function
    get_attr = function(attr) {
      stopifnot(is_character_single(attr))
      return(private$attrs[[attr]])
    },

    # name of dataset (to display in the UI)
    # no corresponding set_ function
    get_data_label = function(dataname) {
      stopifnot(is_character_single(dataname))
      return(self$get_data_attr(dataname, "data_label"))
    },

    get_data_attr = function(dataname, attr) {
      stopifnot(is_character_single(dataname))
      stopifnot(is_character_single(attr))
      return(private$data_attrs[[dataname]][[attr]])
    },

    set_data_attr = function(dataname, attr, value) {
      stopifnot(is_character_single(dataname))
      stopifnot(is_character_single(attr))

      private$data_attrs[[dataname]][[attr]] <- value
      return(private$data_attrs[[dataname]][[attr]])
    },

    # no corresponding set_ function
    get_data_attrs = function(dataname) {
      stopifnot(is_character_single(dataname))
      return(private$data_attrs[[dataname]])
    },

    # get non-NA labels of columns in data
    # no corresponding set_ function
    get_column_labels = function(dataname, subset = NULL) {
      stopifnot(is_character_single(dataname))
      stopifnot(is.null(subset) || is_character_empty(subset) || is_character_vector(subset))

      labels <- self$get_data_attr(dataname, "column_labels")
      if (!is.null(subset)) {
        labels <- labels[subset]
      }
      labels <- labels[!is.na(labels)]

      return(labels)
    },

    # filtering state and characteristics, remove and continue filtering ----

    # return variable information (categorical, numeric range etc) for dataname
    # if varname  is specified, only returns it for the column varname in dataname
    # this can be used to see how the variable must be filtered
    # when varname is NULL, only returns characteristics for vars that are filtered (unless all_vars is TRUE)
    # update_filter_chars is private because it should not be user set
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

    # similar to get_filter_chars, only return data type (numerical, categorical)
    get_filter_type = function(dataname, varname) {
      stopifnot(is_character_single(varname))
      return(self$get_filter_chars(dataname, varname)[["type"]])
    },

    get_filter_state = function(dataname, varname = NULL) {
      private$check_data_varname(dataname, varname)

      if (is.null(varname)) {
        private$filter_state[[dataname]]
      } else {
        return(private$filter_state[[dataname]][[varname]])
      }
    },

    # if varname is non-NULL, only the state of this varname is set,
    # otherwise the state is assumed to be a named list (by varname)
    # for the new states to set for each var
    # returns TRUE if state was reset or not (if it had same value as before)
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
        stop(paste("variables", toString(state_names[inexistent_columns]),
                   "are not available in data", dataname))
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

      private$previous_filter_state[[dataname]] <- isolate(private$filter_state[[dataname]])
      private$filter_state[[dataname]] <- new_state
      private$apply_filter(dataname)

      return(TRUE)
    },

    # restores previous filter state (if filter was removed in between)
    # if not present, takes the filter's default state (i.e. filter characteristics which in
    # most cases do not filter, but may filter out NAs)
    # if not isolated, will depend on previous_filter_state reactive value
    restore_filter = function(dataname, varname) {
      stopifnot(is_character_single(varname))
      private$check_data_varname(dataname, varname)

      selection_state <- private$previous_filter_state[[dataname]][[varname]]
      if (is.null(selection_state)) {
        filter_char <- self$get_filter_chars(dataname, varname)
        selection_state <- switch(
          filter_char$type,
          choices = filter_char$choices,
          range = filter_char$range,
          logical = "TRUE",
          stop("unknown type")
        )
      }
      state <- list(selection = selection_state, keep_na = FALSE)

      return(self$set_filter_state(dataname, varname, state))
    },

    # removes the filter for varname and resets the initial state (set during set_filter_state)
    remove_filter = function(dataname, varname) {
      # The following optimization *cannot* be implemented, but erroneously was in the past.
      # When a filter is removed, instead of trashing
      # the filtered dataset, it is kept in memory. When the filter is added back, it still
      # has the same filtering applied as when it was removed. The filtered dataset still kept
      # in memory can be used again. This is not possible because ADSL filtering may have changed in the
      # meantime, thus affecting the filtering of the dataset.
      return(self$set_filter_state(dataname, varname, NULL))
    },

    # whether the variable can be filtered (type not unknown)
    can_be_filtered = function(dataname, varname) {
      stopifnot(is_character_single(dataname))
      stopifnot(is_character_single(varname))

      return(self$get_filter_type(dataname, varname) != "unknown")
    },

    # this function needs to be public because it is used to generate R code
    # returns call to filter dataset (taking out patients not in ADSL)
    # merge: whether to include merge call that removes any keys from filtered data that are not also in filtered ADSL afterwards
    # adsl: whether to include adsl filtering call before
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
          call("merge",
               x = call("[", as.name("ADSL_FILTERED"), quote(expr =), keys), # nolint
               y = as.name(filtered_alone),
               by = keys,
               all.x = FALSE, all.y = FALSE)
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

    # stop filtering until reactivated to make several changes and only filter in the end
    hold_filtering = function() {
      private$filter_on_hold <- TRUE
      return(invisible(NULL))
    },

    # reactivate filtering which can result in several datasets being filtered
    continue_filtering = function() {
      private$filter_on_hold <- FALSE
      private$apply_filter() # rerun all filtering
      return(invisible(NULL))
    },

    # info functions for end user ----

    # shows filter characteristics of each variable for a given dataset
    # will intersect variables with actual variables in dataname if non-NULL
    # filtered_vars_only: whether to only include filtered_vars
    list_data_info = function(dataname, filtered_vars_only = FALSE, variables = NULL) {
      private$check_data_varname(dataname)
      stopifnot(is_logical_single(filtered_vars_only))
      stopifnot(is.null(variables) || is_character_vector(variables))

      # log with newline
      log2 <- function(...) {
        cat(paste(..., collapse = " ")); cat("\n")
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

    # get info about dataname, e.g. number of patients
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

    # set class variables to default values, i.e. empty lists or NULL per dataname
    # following attributes are not set: `attr`, `filter_on_hold`
    # reactiveValues contents are set to NULL, but not reset entirely so the reactive
    # dependencies are kept, in particular, the datanames are kept
    reset_class_vars = function(dataname = NULL) {
      if (!is.null(dataname)) {
        private$check_data_varname(dataname)
      }

      # if dataset is ADSL, we need to reset filtered datasets
      # to simplify, here we simply reset the other datasets as well then
      datanames <- if (is.null(dataname) || (dataname == "ADSL")) {
        self$datanames() # delete all datasets
      } else {
        dataname
      }

      # do not recreate reactiveValues here because reactivity will be lost otherwise
      # just set to NULL so the observers can see that the dataset was removed or
      # will be replaced.
      for (name in datanames) {
        private$datasets[[name]] <- NULL
        private$filtered_datasets[[name]] <- NULL
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
    validate_temp = function() {
      return(private$validate())
    }

  ), # end of public functions

  ## __Private Methods---------------------
  private = list(

    # private attributes ----

    # the following attributes are (possibly reactive lists) per dataname
    datasets = NULL, # unfiltered datasets
    filtered_datasets = NULL, # filtered dataset after applying filter to unfiltered dataset
    data_attrs = NULL,  # attributes per dataname
    # filter to apply to obtain filtered dataset from unfiltered one, NULL (for a dataname) means
    # no filter applied: it does not mean that it does not show up as a filtering element,
    # NULL just means that filtering has no effect
    # therefore, the following lists may not be complete and simply not contain all varnames
    # for a given dataname, which means that the state is NULL
    filter_state = NULL,
    # previous filter state when you want to revert, e.g. when filter previously removed is added again
    previous_filter_state = NULL,
    # filter characteristics to create filter that has no effect (i.e. full range of variable),
    # useful for UI to show sensible ranges
    filter_chars = NULL,

    # whether filtering is currently performed or it is postponed, useful when several fields
    # are changed at once and it should not run for each of the steps, but just once at the end
    filter_on_hold = FALSE,
    attrs = list(), # attributes of this class retrievable with get_attrs and set_attrs

    # check functions ----
    # we implement these functions as checks rather than returning logicals so they can give informative error messages immediately

    # validate object to inspect if something is wrong
    validate = function() {
      stopifnot(
        # check classes
        is.reactivevalues(private$datasets),
        is.reactivevalues(private$filtered_datasets),
        is.list(private$data_attrs),
        is.reactivevalues(private$filter_state),
        is.reactivevalues(private$previous_filter_state),
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
            dataname, varname = varname,
            var_state = private$filter_state[[dataname]][[varname]]
          )
        })
      })

      # check previous_filter_state
      lapply(names(private$previous_filter_state), function(dataname) {
        lapply(names(private$previous_filter_state[[dataname]]), function(varname) {
          private$check_valid_filter_state(
            dataname, varname = varname,
            var_state = private$previous_filter_state[[dataname]][[varname]]
          )
        })
      })

      return(invisible(NULL))
    },

    # checks if dataname is in datanames (of this class) and
    # (if provided) that varname is a valid column in data
    check_data_varname = function(dataname, varname = NULL) {
      stopifnot(is_character_single(dataname))
      stopifnot(is.null(varname) || is_character_single(varname))

      if (!(dataname %in% self$datanames())) {
        stop(paste("data", dataname, "is not available"))
      }
      if (!is.null(varname) && !(varname %in% names(self$get_data(dataname)))) {
        stop(paste("variable", varname, "is not in data", dataname))
      }

      return(invisible(NULL))
    },

    check_valid_filter_state = function(dataname, varname, var_state) {
      var_char <- self$get_filter_chars(dataname, varname) # filter characteristics for var, e.g. range

      if (is.null(var_state)) {
        # when state is NULL, this means no filtering is applied
        return()
      }

      if (!is_logical_single(var_state$keep_na)) {
        browser()
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
            stop(paste("data", dataname, "variable", varname, "range (", toString(selection_state),
                       ") not valid for full range (", toString(var_char$range), ")"))
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

    # helper function to detect variable type (categorical, numeric range)
    # and accordingly set the filter characteristics
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
            class(var), "' which has currently no filter UI element", sep = ""
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

    # maybe remove out_dataname or default to NULL (no assignment)
    # creates a call that assigns the operates on dataname to obtain the filtered dataset and
    # assigns it to the variable named dataname_out
    # as opposed to get_filter_call, this function does not filter for patient for patients in ADSL
    get_filter_call_no_adsl = function(dataname, dataname_out) {
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
                stop("Unknown filter state", toString(selection_state), " for logical var ", varname, " in data", dataname)
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

    # apply filter (error if filter_on_hold is TRUE)
    # i.e. set filtered data from unfiltered data
    # when the data is set in the beginning and ADSL is not first, the filtered data is not set
    # once ADSL is set, one must explicitly call this function again so that the filtered_datasets
    # for all datasets will be set
    # returns if filter was actually applied or delayed (if ADSL not yet provided)
    # dataname = NULL means refilter all datasets (always happens for ADSL), otherwise means the dataset with name dataname changed, so optimized logic can be implemented
    apply_filter = function(dataname = NULL) {
      if (private$filter_on_hold) {
        stop("Cannot apply filter because filtering is on hold.")
      }

      .log("apply filter for", dataname) # todo1: write something similar to withr: with_logging(code)

      if (is.null(self$get_data("ADSL", filtered = FALSE))) {
        # ADSL was not yet provided with set_data, so we remove also filters for all datasets
        # todo1: why is this here? an error should be thrown that the filter cannot be applied
        stopifnot(all(private$filtered_datasets, is.null)) # nothing should have been set by now or reset after ADSL was reset
        stop("You must first set ADSL before setting any other datasets")
        return(FALSE)
      } else {

        datanames <- if (is.null(dataname) || identical(dataname, "ADSL")) {
          # re-run all filters
          # make ADSL first because the others depend on filtered ADSL
          c("ADSL", setdiff(self$datanames(), "ADSL"))
        }

        # filter data directly in an empty environment to make sure no global variables or
        # other variables from this class are used
        # todo1: why not use a chunks object for this from teal.devel (and put chunks code into teal?)
        # packages bind right before globalenv(), so we don't accidentally pick up other packages
        env <- new.env(parent.env(globalenv()))

        for (name in datanames) {
          # assign unfiltered datasets to env, evaluate filtered datasets
          env[[name]] <- self$get_data(name, filtered = FALSE)
          lapply(
            self$get_filter_call(name, merge = TRUE, adsl = FALSE),
            eval,
            envir = env
          )
        }

        # assign from environment back to class
        for (name in datanames) {
          private$filtered_datasets[[name]] <- env[[paste0(name, "_FILTERED")]]
        }

        return(TRUE)
      }
    }
  )
)
