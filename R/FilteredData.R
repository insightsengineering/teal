#' @name FilteredData
#' @docType class
#'
#' @title Class to encapsulate filtered datasets
#'
#' @details
#' The main purpose of this class is to provide a collection of reactive datasets,
#' each of which can be filtered through the right filter panel of teal apps.
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
#' Common arguments are:
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
#' # for `on.exit` to work with examples which are executed line-by-line
#' # note: you can still execute every line in the function line-by-line
#' # without calling the function as a whole, beware that the exit handler
#' # is not called in this case
#' execute_example <- function() {
#'   ADSL <- radsl(cached = TRUE)
#'   attr(ADSL, "keys") <- get_cdisc_keys("ADSL")
#'   datasets <- teal:::FilteredData$new()
#'
#'   # to evaluate without isolate(), i.e. provide a default isolate context
#'   options(shiny.suppressMissingContextError = TRUE)
#'   on.exit(options(shiny.suppressMissingContextError = FALSE), add = TRUE)
#'
#'   datasets$set_data("ADSL", ADSL)
#'
#'   datasets$datanames()
#'   datasets$get_data_info("ADSL", filtered = FALSE)
#'   # filters dataset to obtain information
#'   datasets$get_data_info("ADSL", filtered = TRUE)
#'   datasets$get_filter_info("ADSL")
#'   df <- datasets$get_data("ADSL", filtered = FALSE)
#'   # df
#'
#'   datasets$get_filter_type("ADSL", "SEX")
#'   datasets$set_filter_state("ADSL", varname = NULL, state = list(
#'     AGE = list(range = c(33, 44), keep_na = FALSE),
#'     SEX = list(choices = c("M", "F"), keep_na = FALSE)
#'   ))
#'   datasets$get_filter_type("ADSL", "SEX")
#'   datasets$get_filter_info("ADSL")[["SEX"]]$type
#'
#'   # will fail because of invalid range
#'   # datasets$set_filter_state("ADSL", varname = NULL, list(
#'   #   AGE = list(range = c(3, 7), keep_na = FALSE)
#'   # ))
#'   datasets$set_filter_state("ADSL", varname = NULL, list(
#'     AGE = list(range = c(33, 44), keep_na = FALSE)
#'   ))
#'   datasets$set_filter_state(
#'     "ADSL",
#'     varname = "SEX",
#'     state = list(choices = c("M", "F"), keep_na = FALSE)
#'   )
#'   datasets$get_filter_type("ADSL", "SEX")
#'
#'   datasets$get_filter_state("ADSL")
#' }
#' execute_example()
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
      # create `reactiveValues` for unfiltered and filtered dataset and filter state
      # each `reactiveValues` is a list with one entry per dataset name
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
    #' @md
    #' @return `character` vector of datanames
    datanames = function() {
      return(make_adsl_first(names(private$datasets)))
    },

    # getters and setters for attributes ----

    #' @details
    #' Get filtered or unfiltered dataset
    #'
    #' @param dataname `character` name of the dataset
    #' @param filtered `logical` whether to return filtered or unfiltered dataset
    # sodo3: remove default argument for filtered, already fixed in teal, but maybe not downstream
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
    #' Note: due to the nature of `reactiveValues()`, once a dataname
    #' is added, it cannot be removed anymore because some code may depend
    #' on it. You can try to achieve this by setting the data to NULL
    #' and the observers must then know that NULL means that the dataset
    #' was removed.
    #'
    #' Any attributes attached to data are kept in the unfiltered data.
    #'
    #' @md
    #' @param dataname `character` name of the dataset, without spaces
    #' @param data `data.frame` data that corresponds to `dataname`
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

      # save `md5sum` for reproducibility
      attr(data, "md5sum") <- digest(data, algo = "md5")
      private$datasets[[dataname]] <- data

      return(invisible(NULL))
    },

    #' @details
    #' Get the R preprocessing code string that generates the unfiltered datasets
    #' @md
    #' @return `character` deparsed code
    get_preproc_code = function() {
      return(private$preproc_code)
    },

    #' @details
    #' Set the R preprocessing code to generate the unfiltered datasets
    #'
    #' @md
    #' @param preproc_code `character` preprocessing code that can be parsed to generate the
    #'   unfiltered datasets
    set_preproc_code = function(preproc_code) {
      stopifnot(is_character_single(preproc_code))
      # sodo3: check here that the preproc_code faithfully reproduces the datasets
      private$preproc_code <- preproc_code
      return(invisible(NULL))
    },

    # sodo3: keep `get_data_attr` and `set_data_attr`
    #' @details
    #' Get data attribute for the dataset
    #'
    #' sets and gets the data attribute on unfiltered data as it is never modified
    #' as attributes
    #'
    #' @md
    #' @param dataname `character` name of the dataset
    #' @param attr attribute to get from the data attributes of the dataset
    get_data_attr = function(dataname, attr) {
      private$check_data_varname(dataname)
      stopifnot(is_character_single(attr))
      return(attr(private$datasets[[dataname]], attr))
    },

    #' @details
    #' Set data attribute for the dataset
    #'
    #' @md
    #' @param dataname `character` name of the dataset
    #' @param attr attribute to get from the data attributes of the dataset
    #' @param value value to set attribute to
    set_data_attr = function(dataname, attr, value) {
      private$check_data_varname(dataname)
      stopifnot(is_character_single(attr))

      attr(private$datasets[[dataname]], attr) <- value
      return(invisible(NULL))
    },

    # no corresponding set_ function
    #' @details
    #' Get non-NA labels of columns in the data
    #'
    #' @md
    #' @param dataname `character` name of the dataset
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
    #' @param dataname `character` name of the dataset
    #' @param varname `character` column within the dataset,
    #'   if `NULL`, return all (filtered) variables
    #' @param all_vars `logical` (only applies if `varname` is NULL)
    #'   whether to include non-filtered variables
    get_filter_info = function(dataname, varname = NULL, all_vars = FALSE) {
      private$check_data_varname(dataname, varname)
      stopifnot(
        is_logical_single(all_vars),
        is.null(varname) || !all_vars
      )

      if (is.null(varname)) {
        # filter out variables that are not filtered
        if (all_vars) {
          # sodo3: only ever return filterable variables
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
    #' @param dataname `character` name of the dataset
    #' @param varname `character` column within the dataset,
    #'   must be provided
    #' @return `character` filter type
    get_filter_type = function(dataname, varname) {
      stopifnot(is_character_single(varname)) # check that varname is non-NULL
      return(self$get_filter_info(dataname, varname)[["type"]])
    },

    #' @details
    #' Filter state for a dataset (or only a variable within it).
    #'
    #' @md
    #' @param dataname `character` name of the dataset
    #' @param varname `character` column within the dataset;
    #'   if `NULL`, return all variables
    #' @return `filter state or list of these`
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
    #' When `varname` is non-`NULL`, only the state for that variable name is changed.
    #' Otherwise, the state is entirely set to the new state.
    #'
    #' To remove all filters for a dataset, you can pass in `varname = NULL, state = list()`.
    #' To remove a filter variable for a dataset, pass in `varname, state = NULL`.
    #'
    #' @md
    #' @param dataname `character` name of the dataset
    #' @param varname `character` column within the dataset;
    #' @param state new state to set; when `varname` is `NULL`, `state` must be
    #'   a named list with the new filter state for each variable (variables
    #'   that are not included are unaffected)
    #'
    #' @return `logical` if the state was changed
    set_filter_state = function(dataname, varname, state) {
      # sodo3: make varname required and only allow setting one variable at a time as the same can currently
      # be achieved with `lapply`
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
      inexistent_columns <- which(!(state_names %in% names(self$get_data(dataname, filtered = FALSE))))
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
    #' @md
    #' @param dataname `character` name of the dataset
    #' @param varname `character` column within the dataset;
    #'   must be provided
    get_default_filter_state = function(dataname, varname) {
      stopifnot(is_character_single(varname))
      private$check_data_varname(dataname, varname)

      filter_info <- self$get_filter_info(dataname, varname)
      state <- switch(
        filter_info$type,
        choices = list(choices = filter_info$choices),
        range = list(range = filter_info$range),
        logical = list(status = "TRUE"),
        stop("unknown type")
      )
      return(c(state, list(keep_na = FALSE)))
    },

    #' @details
    #' Restores previous filter state (if the filter was removed in between)
    #'
    #' If there is no previous filter, it takes the filter's default state.
    #'
    #' If a previous filter state exists and the state is `s` before calling
    #' this function twice, then the state will be `s` again.
    #'
    #' @md
    #' @param dataname `character` name of the dataset
    #' @param varname `character` column within the dataset;
    #'   must be provided
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
    #' @md
    #' @param dataname `character` name of the dataset
    #' @param varname `character` column within the dataset;
    #'   must be provided
    #' @return whether the variable can be filtered (type not unknown)
    can_be_filtered = function(dataname, varname) {
      stopifnot(is_character_single(dataname))
      stopifnot(is_character_single(varname))

      # sodo3: do we want to implement some additional logic that a categorical variable cannot be filtered
      # if it has more than e.g. 1000 levels as the UI will otherwise get stuck
      return(self$get_filter_type(dataname, varname) != "unknown")
    },

    #' @details
    #' Get the call to filter the dataset according to the filter state
    #'
    #' This can be used for the `Show R Code` generation.
    #'
    #' When the dataname is ADSL, it is simply the filtered ADSL.
    #' Otherwise, it is the filtered dataset that only contains subjects which
    #' are also in the filtered ADSL.
    #'
    #' Note: The `merge()` function in returned call will fail if the
    #' corresponding dataset is NULL.
    #'
    #' @md
    #' @param dataname `character` name of the dataset
    #' @return call to filter dataset (taking out patients not in ADSL)
    get_filter_call = function(dataname) {
      private$check_data_varname(dataname)

      if (dataname == "ADSL") {
        filter_call <- as.call(list(
          as.name("<-"), as.name("ADSL_FILTERED"),
          private$get_pure_filter_call("ADSL")
        ))
        return(list(filter_call))
      } else {

        filtered_alone <- paste0(dataname, "_FILTERED_ALONE")
        filter_call <- as.call(list(
          as.name("<-"), as.name(filtered_alone),
          private$get_pure_filter_call(dataname)
        ))

        # ADSL has a special status in the sense that filtering in ADSL impacts filtering of the other datasets
        # example: ADLB_FILTERED_ALONE is ADLB with filter applied
        # ADLB_FILTERED is ADLB_FILTERED_ALONE with only the (USUBJID, STUDYID) combinations appearing in ADSL_FILTERED
        # sodo3: instead of a merge call, it would be simpler to just check %in% USUBJID assuming a single studyid
        filtered_joined_adsl <- paste0(dataname, "_FILTERED") # filtered_alone with

        keys <- self$get_data_attr("ADSL", "keys")$primary
        stopifnot(!is.null(keys)) #non-NULL because dataset is not ADSL
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
    #' @param dataname `character` name of the dataset
    #' @param filtered_vars_only `logical` whether to only consider filtered
    #'   vars or unfiltered as well
    #' @param variables (`character` vector) variables to consider; if NULL, takes all
    #' @return `NULL`, only prints
    print_filter_info = function(dataname, filtered_vars_only = FALSE, variables = NULL) {
      private$check_data_varname(dataname)
      stopifnot(is_logical_single(filtered_vars_only))
      stopifnot(is.null(variables) || is_character_vector(variables))

      # log with newline
      log2 <- function(...) {
        cat(paste(...))
        cat("\n")
      }

      log2("====", dataname, "=======================")


      df <- self$get_data(dataname, filtered = FALSE)
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
        var_maxlength <- max(c(0, nchar(varnames)))
        for (varname in varnames) {
          var_info <- filter_infos[[varname]]
          var_state <- self$get_filter_state(dataname, varname)

          # right alignment: %10s, left alignment: %-10s
          txt <- sprintf(paste0("%-", var_maxlength, "s has filter type %-10s: "), varname, var_info$type)
          log2(paste0(txt, filter_state_to_str(var_info$type, var_info)))
          log2(paste0(
            sprintf(paste0("%", nchar(txt), "s"), "\\--> selected: "),
            filter_state_to_str(var_info$type, var_state)
          ))
        }
        if (length(varnames) == 0) {
          log2("There are no", if (filtered_vars_only) "filtered", "variables for dataset", dataname)
        }
      }
      log2("===========================")
      return(invisible(NULL))
    },

    #' @details
    #' Get info about dataname, e.g. dimensions and
    #' number of patients (in long datasets different from ADSL,
    #' a patient can occur in zero or more rows)
    #'
    #' @md
    #' @param dataname `character` name of the dataset
    #' @param filtered `logical` whether to obtain this info for the
    #'   filtered dataset
    #' @return a list with names `dim, patients`
    get_data_info = function(dataname, filtered) {
      stopifnot(is_character_single(dataname))
      stopifnot(is_logical_single(filtered))

      data <- self$get_data(dataname, filtered = filtered)

      keys <- self$get_data_attr(dataname, "keys")
      # foreign is NULL if ADSL, otherwise foreign refers to primary keys of ADSL
      unique_patient_colnames <- if_null(keys$foreign, keys$primary)
      stopifnot(!is.null(unique_patient_colnames))

      list(
        dim = dim(data),
        patients = nrow(dplyr::distinct(data[unique_patient_colnames]))
      )
    },

    # Functions useful for restoring from another dataset ----

    #' Returns the state to be bookmarked
    #'
    #'
    #' `md5` sums of `datasets`, `filter_states` and `preproc_code` are bookmarked,
    #' `previous_filter_state` is not bookmarked.
    #'
    #' @md
    #' @return named list
    get_bookmark_state = function() {
      # We cannot simply create a new `FilteredData` with the datasets set to NULL
      # (to protect the data from unauthorized access). Therefore, we return a list
      # which is independent of this class. It is also better to store a list
      # rather than a class because this allows bookmarking of type `url` and
      # not just `server`
      return(list(
        # must be a list and not atomic vector, otherwise jsonlite::toJSON gives a warning
        data_md5sums = setNames(
          lapply(self$datanames(), self$get_data_attr, "md5sum"),
          self$datanames()
        ),
        filter_states = reactiveValuesToList(private$filter_state),
        preproc_code = self$get_preproc_code()
      ))
    },

    #' Set this object from a bookmarked state
    #'
    #' Only sets the filter state, does not set the data, the previous filter state
    #' and the preprocessing code.
    #' Also checks the preprocessing code is identical if provided in the `state`.
    #'
    #' Since this function is used from the end-user part, its error messages
    #' are more verbose. We don't call the Shiny modals from here because this
    #' class may be used outside of a Shiny app.
    #'
    #' @md
    #' @param state `list` containing fields `data_md5sums`, `filter_states`
    #'   and `preproc_code`.
    #' @param check_data_md5sums `logical` whether to check that `md5sums` agree
    #'   for the data; may not make sense with randomly generated data per session
    set_from_bookmark_state = function(state, check_data_md5sums = TRUE) {
      stopifnot(
        is.list(state),
        is_logical_single(check_data_md5sums)
      )

      # stops with verbose error message
      stop_setequal <- function(x, y, pre_msg = "") {
        if (!setequal(x, y)) {
          stop(paste0(paste(
            pre_msg,
            toString(x),
            "is not equal to",
            toString(y),
            sep = "\n"
          )))
        }
      }
      stop_setequal(
        names(state$data_md5sums), self$datanames(),
        pre_msg = "The names of the stored md5 sums and the datanames don't agree:"
      )
      stop_setequal(
        names(state$filter_states), self$datanames(),
        pre_msg = "The names of the stored filters and the datanames don't agree:"
      )
      if (check_data_md5sums) {
        datasets_equal <- vapply(self$datanames(), self$get_data_attr, character(1), "md5sum") == state$data_md5sums
        if (!all(datasets_equal)) {
          stop("The following datasets are not identical and probably have changed since bookmarking: ", toString(names(datasets_equal)))
        }
      }
      if (!is.null(state$preproc_code)) {
        stop_setequal(
          self$get_preproc_code(), state$preproc_code,
          pre_msg = "The preprocessing codes don't agree:"
        )
      }

      # we have to be careful with `reactiveValues` to restore each item and not simply
      # reference the old reactive value, i.e. loop over it
      lapply(self$datanames(), function(dataname) {
        self$set_filter_state(dataname, varname = NULL, state = state$filter_states[[dataname]])
      })
      # other reactive endpoint don't need to be set because they are computing through reactivity

      return(invisible(NULL))
    }
  ), # end of public functions

  ## __Private Methods---------------------
  private = list(

    # private attributes ----

    # the following attributes are (possibly reactive lists) per dataname
    # unfiltered datasets, attributes per dataname are possible like `keys`, `data_label` etc.
    datasets = NULL,
    # stores reactive which return filtered dataset after applying filter to unfiltered dataset
    filtered_datasets = NULL,
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

    preproc_code = NULL, # preprocessing code used to generate the unfiltered datasets as a string

    # check functions ----
    # we implement these functions as checks rather than returning logicals so they can
    # give informative error messages immediately

    # @details
    # Validate object to inspect if something is wrong
    #
    validate = function() {
      .log("## validating FilteredData object consistency")

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

    # @details
    # Checks if the dataname exists and
    # (if provided) that varname is a valid column in the dataset
    #
    # Stops when this is not the case.
    #
    # @md
    # @param dataname `character` name of the dataset
    # @param varname `character` column within the dataset;
    #   if `NULL`, this check is not performed
    check_data_varname = function(dataname, varname = NULL) {
      stopifnot(is_character_single(dataname))
      stopifnot(is.null(varname) || is_character_single(varname))

      isolate({
        # we isolate everything because we don't want to trigger again when datanames change
        if (!(dataname %in% self$datanames())) {
          # data must be set already
          stop(paste("data", dataname, "is not available"))
        }
        if (!is.null(varname) && !(varname %in% names(self$get_data(dataname, filtered = FALSE)))) {
          stop(paste("variable", varname, "is not in data", dataname))
        }
      })

      return(invisible(NULL))
    },

    # @details
    # Checks that the given filter state is valid for the specified
    # variable in the specified dataset.
    #
    # This function is typically called before setting a new filter state
    # to validate it.
    #
    # It stops when there is an error.
    #
    # @md
    # @param dataname `character` name of the dataset
    # @param varname `character` column within the dataset;
    #   must be provided
    # @param varstate `list` variable state
    check_valid_filter_state = function(dataname, varname, var_state) {
      stopifnot(is_character_single(varname))
      private$check_data_varname(dataname, varname)

      var_info <- self$get_filter_info(dataname, varname) # filter characteristics for var, e.g. range

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

      switch(
        var_info$type,
        choices = {
          selection_state <- var_state$choices
          if (any(!(selection_state %in% var_info$choices))) {
            stop(paste(
              "data", dataname, "variable", varname, ":", "choices (", toString(selection_state),
              ") not all in valid choices (", toString(var_info$choices), ")"
            ))
          }
        },
        range = {
          selection_state <- var_state$range
          if ((length(selection_state) != 2) ||
            (selection_state[1] > selection_state[2]) ||
            ((selection_state[1] < var_info$range[1]) || (selection_state[2] > var_info$range[2]))
          ) {
            stop(paste(
              "data", dataname, "variable", varname, "range (", toString(selection_state),
              ") not valid for full range (", toString(var_info$range), ")"
            ))
          }
        },
        logical = {
          selection_state <- var_state$status
          # the conceptual difference to type 'choices' is that it allows exactly one value rather than a subset
          stopifnot(length(selection_state) == 1)

          if (!(selection_state %in% var_info$choices)) {
            stop(paste(
              "data", dataname, "variable", varname, "choices (", toString(selection_state),
              ") not in valid choices (", toString(var_info$choices), ")"
            ))
          }
        },
        stop(paste("Unknown filter type", var_info$type, "for data", dataname, "and variable", varname))
      )
      return(invisible(NULL))
    },

    # @details
    # Creates a call that filters the dataset to obtain the filtered dataset.
    # `get_filter_call` calls this function to filter out patients that are not in
    # the filtered ADSL.
    #
    # @md
    # @param dataname `character` name of the dataset
    get_pure_filter_call = function(dataname) {
      private$check_data_varname(dataname)

      data_filter_call_items <- Map(
        function(filter_info, filter_state, varname) {
          stopifnot(!is.null(filter_state)) # filter_state only contains non-NULL values

          type <- filter_info$type
          if (is.null(type)) {
            # such a filter should never have been set
            stop(paste("filter type for variable", varname, "in", dataname, "not known"))
          }

          filter_call <- switch(
            type,
            choices = {
              selection_state <- filter_state$choices
              if (length(selection_state) == 1) {
                call("==", as.name(varname), selection_state)
              } else {
                call("%in%", as.name(varname), selection_state)
              }
            },
            range = {
              selection_state <- filter_state$range
              call(
                "&",
                call(">=", as.name(varname), selection_state[1]),
                call("<=", as.name(varname), selection_state[2])
              )
            },
            logical = {
              selection_state <- filter_state$status
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
          if (filter_state$keep_na) {
            # we add `is.na` independent of whether the variable has na values or not
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
        # subset is meant for interactive use, so we use filter
        return(call_with_colon("dplyr::filter", as.name(dataname), unlist_args = unname(data_filter_call_items)))
      }
    },

    # generate reactive expressions for reactive attributes that depend on data ----

    # @details
    # This returns a reactive that returns the filtered dataset
    # it refilters whenever the source datasets
    # change or the filter state
    #
    # @param dataname `character` name of the dataset
    reactive_filtered_dataset = function(dataname) {
      stopifnot(is_character_single(dataname))
      reactive({
        # use isolate to avoid triggering due to adding a dataset via `set_data`
        if (!dataname %in% isolate(self$datanames())) {
          stop("Cannot filter data ", dataname, " as it needs to be set first")
        }

        .log("################################################# Refiltering dataset ", dataname)

        # sodo3: why not use a chunks object for this from teal.devel (and put chunks code into teal?)
        # filter data directly in an empty environment to make sure no global variables or
        # other variables from this class are used
        # this assumes that no other packages are attached in the call, as this modifies the
        # parent of the `globalenv`
        # we prefer to take a parent of the `globalenv` to avoid picking up user-defined global variables
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

    # @details
    # Sets filter infos
    #
    # Detect variable type (categorical, numeric range)
    # and accordingly set the filter characteristics
    #
    # @param dataname `character` name of the dataset
    reactive_filter_infos = function(dataname) {
      reactive({
        df <- self$get_data(dataname, filtered = FALSE)

        filter_info <- Map(function(var, varname) {
          res <- if (all(is.na(var))) {
            .log("all elements in", varname, "are NA")
            list(
              type = "unknown",
              label = if_null(attr(var, "label"), ""),
              class = class(var)
            )
          } else if (is.factor(var) || is.character(var)) {
            if (!is(var, "factor")) {
              var <- factor(var, levels = sort(unique(as.character(var))))
              add_counts <- FALSE
            } else {
              add_counts <- TRUE
            }
            var <- droplevels(var)

            choices <- levels(var)
            if (add_counts) {
              names(choices) <- paste0(choices, " (", tabulate(var), ")")
            }
            histogram_data <- data.frame(x = levels(var), y = tabulate(var))

            list(
              type = "choices",
              label = if_null(attr(var, "label"), ""),
              choices = as.list(choices), # convert named vector to named list as Shiny `toJSON` otherwise complains
              histogram_data = histogram_data
            )
          } else if (is.numeric(var)) {
            density <- stats::density(var, na.rm = TRUE)
            list(
              type = "range",
              label = if_null(attr(var, "label"), ""),
              range = range(var, na.rm = TRUE),
              histogram_data = data.frame(x = density$x, y = density$y)
            )
          } else if (is.logical(var)) {
            var <- factor(var, levels = c("TRUE", "FALSE"))
            choices <- levels(var)
            names(choices) <- paste0(choices, " (", tabulate(var), ")")
            histogram_data <- data.frame(x = levels(var), y = tabulate(var))
            list(
              type = "logical",
              label = if_null(attr(var, "label"), ""),
              choices = as.list(choices), # convert named vector to named list as Shiny `toJSON` otherwise complains
              histogram_data = histogram_data
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

#' Print the state in a nice format
#'
#' The `keep_na` is not printed.
#' @md
#' @param var_type `character` variable type
#' @param state `list` with elements corresponding to variable
#'   type; can be filter state or filter info
#' @param add_na `logical` whether to display information about NA
filter_state_to_str <- function(var_type, state, add_na = FALSE) {
  stopifnot(is_logical_single(add_na))

  txt <- switch(
    var_type,
    range = paste(state$range, collapse = " - "),
    choices = {
      if (length(state$choices) > 5) {
        paste0(toString(state$choices[1:5]), ", ...")
      } else {
        toString(state$choices)
      }
    },
    logical = toString(state),
    "" # no special info when filter type is unknown
  )
  if (add_na) {
    txt <- if (!is.null(state$keep_na)) {
      # filter state
      paste0(txt, "; keep_na: ", toString(state$keep_na))
    } else {
      # filter info
      stopifnot(!is.null(state$na_count))
      paste0(txt, "; na_count: ", toString(state$na_count))
    }
  }
  return(txt)
}
