#' @name FilteredData
#' @docType class
#' @title Class to encapsulate filtered data sets
#'
#' @details
#' The main purpose of this class is to provide a collection of reactive datasets,
#' each of which can be filtered through manipulation of right panel of teal apps.
#'
#' Other classes then take care of actually merging together the datasets.
#'
#' In the constructor, all available datanames are specified. These can then be set
#' using the `set_data` method. A method to load from a file is also available.
#'
#' This class is ADSL-centric in the sense that ADSL is required to apply the filters.
#' Every dataset, in addition to its own filter, is filtered to make sure that it only
#' contains keys present in ADSL (defaulting to (USUBJID, STUDYID)).
#' Once the ADSL dataset is set, the filters are applied.
#'
#' In order to set several datasets or change several filters at once without intermediate
#' refiltering, the functions `hold_filtering` and `continue_filtering` can be used.
#'
#' By the design of the class, datanames must be provided in the beginning. They are not
#' reactive and changing them despite of that will not trigger reactivity.
#'
#' @md
#'
#' @importFrom digest digest
#' @importFrom dplyr n_groups group_by_
#' @importFrom haven read_sas
#' @importFrom R6 R6Class
#' @importFrom readr read_csv
#' @importFrom tools file_ext file_path_sans_ext
#'
#' @examples
#' \dontrun{
#'
#' # todo: example with random.cdisc.data as well, not so many blank lines everywher in this file
#' path <- "/opt/BIOSTAT/qa/cdt7876a/libraries/asl.sas7bdat"
#'
#' x <- FilteredData$new()
#'
#' x$load_data(path)
#'
#' x$datanames()
#'
#' x$list_data_info("ADSL")
#' x$get_filter_chars("ADSL")
#'
#' df <- x$get_data("ADSL")
#'
#' x$get_filter_chars("ADSL")[['USUBJID']]$type
#'
#' x$set_filter("ADSL", list(AGE=c(3,5), SEX=c('M', 'F')))
#' }
NULL

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
      for (dataname in datanames) {
        if (grepl("[[:space:]]", dataname)) {
          stop(paste0("dataname '", dataname, "' must not contain spaces."))
        }
      }

      create_rv <- function() {
        do.call(reactiveValues, setNames(lapply(datanames, function(x) NULL), datanames))
      }
      # create reactiveValues for unfiltered and filtered dataset and filter state
      # each reactiveValues is a list with one entry per dataset name
      private$datasets          <- create_rv()
      private$filtered_datasets <- create_rv()
      private$filter_state      <- create_rv()
      private$filter_chars       <- create_rv()

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

    # add data into dataname slot together with md5 sum
    set_data = function(dataname, data) {
      stopifnot(is_character_single(dataname))
      stopifnot(is.data.frame(data))

      private$datasets[[dataname]] <- data
      self$set_data_attr(dataname, "md5sum", digest(data, algo = "md5"))

      private$set_filter_chars(dataname)
      private$apply_filter(dataname)

      return(invisible(self))
    },

    # set info in the attributes field of self (not equivalent to attributes(self))
    # from the attributes in data
    set_attrs = function(data) {
      private$attr <- attributes(data)
      return(invisible(self))
    },

    # get info in the attributes field of self (not equivalent to attributes(self))
    get_attrs = function() {
      return(private$attr)
    },

    # todo: rename attr to name
    get_attr = function(attr) {
      stopifnot(is_character_single(attr))
      return(private$attr[[attr]])
    },

    set_data_attr = function(dataname, attr, value) {
      stopifnot(is_character_single(dataname))
      stopifnot(is_character_single(attr))

      private$data_attr[[dataname]][[attr]] <- value
      return(private$data_attr[[dataname]][[attr]])
    },

    get_data_attr = function(dataname, attr) {
      stopifnot(is_character_single(dataname))
      stopifnot(is_character_single(attr))
      return(private$data_attr[[dataname]][[attr]])
    },

    get_data_attrs = function(dataname) {
      stopifnot(is_character_single(dataname))
      return(private$data_attr[[dataname]])
    },

    # get non-NA labels of columns in data
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

    # name of dataset (to display in the UI)
    get_data_label = function(dataname) {
      stopifnot(is_character_single(dataname))
      return(self$get_data_attr(dataname, "data_label"))
    },

    # todo: refactor this function
    list_data_info = function(dataname, filtered = FALSE, variables = NULL) {
      stopifnot(is_character_single(dataname))
      stopifnot(is_logical_single(filtered))
      stopifnot(is.null(variables) || is_character_vector(variables))
      private$check_data_varname(dataname)

      # log with newline
      log2 <- function(...) {
        cat(paste(..., collapse = " ")); cat("\n")
      }

      log2("====", dataname, "=======================")

      df <- isolate(private$datasets[[dataname]])
      if (is.null(df)) {
        log2("The", dataname, "data is NULL") # not set yet with set_data
      } else {
        fi <- private$filter_chars[[dataname]]

        vars <- if (filtered) {
          x <- names(self$get_filter_state(dataname))
          if (is.null(x)) character(0) else x
        } else {
          names(df)
        }

        if (!is.null(variables)) {
          vars <- intersect(vars, variables)
        }

        nmax <- if (length(vars) > 0) max(nchar(vars)) else 0

        for (name in vars) {

          fi_i <- fi[[name]]

          info <- switch(
            fi_i$type,
            range = paste(fi_i$range, collapse = " - "),
            choices = {
              if (length(fi_i$choices) > 5) {
                paste0(paste(fi_i$choices[1:5], collapse = ", "), ", ...")
              } else {
                paste(fi_i$choices, collapse = ", ")
              }
            },
            logical = paste(fi_i$choices, collapse = ", "),
            ""
          )

          log2(sprintf(paste0("%-", nmax, "s has filter type %-10s: %s"), name, fi_i$type, info))

        }
      }
      log2("===========================")
    },

    # check data with dataname was set
    has_data = function(dataname) {
      stopifnot(is_character_single(dataname))
      return(!is.null(private$datasets[[dataname]]))
    },

    # check dataname exists and contains column varname
    has_variable = function(dataname, varname) {
      stopifnot(is_character_single(dataname))
      stopifnot(is_character_single(varname))

      self$has_data(dataname) && (varname %in% names(private$datasets[[dataname]]))
    },

    # reactive: whether returned expression should be isolated or not
    # filtered: return filtered or full dataset
    # todo: why reactive = FALSE by default? should be TRUE and as well everywhere else in this file
    get_data = function(dataname, reactive = FALSE, filtered = FALSE) {
      stopifnot(is_character_single(dataname))
      stopifnot(is_logical_single(reactive))
      stopifnot(is_logical_single(filtered))
      warn_if_nonreactive(reactive)

      private$check_data_varname(dataname)

      # isolate if not reactive
      f <- if (reactive) identity else isolate
      if (filtered) {
        if (private$on_hold) {
          stop("You have to resume filtering first to get the filtered data.")
        }
        f(private$filtered_datasets[[dataname]])
      } else {
        f(private$datasets[[dataname]])
      }
    },

    # get info about dataname, e.g. number of patients
    get_data_info = function(dataname, filtered = TRUE, reactive = FALSE) {
      stopifnot(is_character_single(dataname))
      stopifnot(is_logical_single(reactive))
      stopifnot(is_logical_single(filtered))
      warn_if_nonreactive(reactive)

      # isolate if not reactive
      f <- if (reactive) identity else isolate
      data <- if (filtered) {
        f(private$filtered_datasets[[dataname]])
      } else {
        f(private$datasets[[dataname]])
      }

      keys <- self$get_data_attr(dataname, "keys") #todo: are keys non-NULL?
      unique_patients_cols <- if_null(keys$foreign, keys$primary)

      list(
        name = dataname,
        dim = dim(data),
        patients = dplyr::n_groups(dplyr::group_by_(data, .dots = unique_patients_cols))
      )
    },

    # return variable information (categorical, numeric range etc) for dataname
    # if varname  is specified, only returns it for the column varname in dataname
    # this can be used to see how the variable must be filtered
    get_filter_chars = function(dataname, varname = NULL) {
      stopifnot(is_character_single(dataname))
      stopifnot(is.null(varname) || is_character_single(varname))
      private$check_data_varname(dataname, varname)

      if (is.null(varname)) {
        private$filter_chars[[dataname]]
      } else {
        private$filter_chars[[dataname]][[varname]]
      }
    },

    # similar to get_filter_chars, only return data type (numerical, categorical)
    get_filter_type = function(dataname, varname) {
      return(self$get_filter_chars(dataname, varname)[["type"]])
    },

    # todo: reorder arguments for default arguments to be last
    # varname = NULL -> set state for all variables in dataname, state is a named list with one item per column (name)
    # state = NULL erase filter state -> use remove_filter
    #
    # varname != NULL and state is missing, set to default value
    # if varname is provided, only the state of this varname is set, otherwise the whole state is set (and non-provided columns are unset)
    # initial: whether to apply filter, not necessary when filter is empty anyways
    set_filter_state = function(dataname, varname = NULL, state) {
      # todo: make apply_filter detect in function
      stopifnot(is_character_single(dataname))
      stopifnot(is.null(varname) || is_character_single(varname))
      private$check_data_varname(dataname, varname)

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

      # check if new filter states are valid (e.g. in correct range)
      fi <- private$filter_chars[[dataname]]

      for (var in state_names) {
        fii <- fi[[var]]
        state_i <- state[[var]]

        # when state is NULL, this means no filtering is applied
        if (!is.null(state_i)) {
          switch(
            fii$type,
            choices = {
              if (any(!(state_i %in% fii$choices)))
                stop(paste("data", dataname, "variable", var, "not all valid choices"))
            },
            range = {
              if (length(state_i) != 2)
                stop(paste("data", dataname, "variable", var, "not of length 2"))
            },
            logical = NULL,
            stop(paste("data", dataname, "variable", var, ": cannot filter this variable (type issue)"))
          )
        }
      }

      previous_state <- private$filter_state[[dataname]]

      # set state after checking above
      if (is.null(varname)) {
        # reset whole state
        private$filter_state[[dataname]] <- state
      } else {
        # reset state for that varname only
        stopifnot(length(state) == 1)
        fs <- self$get_filter_state(dataname)
        fs[[varname]] <- state[[1]]
        private$filter_state[[dataname]] <- fs
      }

      # for this to work reliably, the previous state must really capture all info
      # i.e. NA filtering or not
      # all.equal returns TRUE if all equal, otherwise character vector of differences
      if (!isTRUE(all.equal(previous_state, private$filter_state[[dataname]]))) {
        private$apply_filter(dataname)
      }

      return(invisible(self))
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

    get_filter_state = function(dataname, varname = NULL) {
      stopifnot(is_character_single(dataname))
      stopifnot(is.null(varname) || is_character_single(varname))
      private$check_data_varname(dataname, varname)

      if (is.null(varname)) {
        private$filter_state[[dataname]]
      } else {
        return(private$filter_state[[dataname]][[varname]])
      }
    },

    # default filter state is as if no filtering were occurring, i.e. filtering for all
    # called when adding a new filter in the UI
    # does not execute the filter, this needs to be handled outside of this function
    set_default_filter_state = function(dataname, varname) {
      stopifnot(is_character_single(dataname))
      stopifnot(is_character_single(varname))
      private$check_data_varname(dataname, varname)

      fi <- self$get_filter_chars(dataname, varname)
      default_state <- switch(
        fi$type,
        choices = fi$choices,
        range = fi$range,
        logical = "TRUE or FALSE",
        stop("unknown filter type", fi$type)
      )

      # we don't need to apply the filter because it contains the full range,
      # this simplifies the ShowRCode as well
      self$set_filter_state(dataname, varname, default_state)

      return(invisible(self))
    },

    # whether the variable can be filtered (type not unknown)
    is_filter_variable = function(dataname, varname) {
      stopifnot(is_character_single(dataname))
      stopifnot(is_character_single(varname))

      return(self$get_filter_type(dataname, varname) != "unknown")
    },

    # returns call to filter dataset
    # merge: whether to include merge call that removes any keys from filtered data that are not also in filtered ADSL afterwards
    # adsl: whether to include adsl filtering call before
    # todo: when do we want adsl = TRUE?
    # todo: rename to adsl_filter_call, rename to merge_adsl_call
    # todo: rename to get_filter_calls
    get_filter_call = function(dataname, merge = TRUE, adsl = TRUE) {
      stopifnot(is_character_single(dataname))
      stopifnot(is_logical_single(merge))
      stopifnot(is_logical_single(adsl))
      private$check_data_varname(dataname)

      adsl_filter_call <- private$get_subset_call("ADSL", "ADSL_FILTERED")

      if (dataname == "ADSL") {
        adsl_filter_call
      } else {
        # ADSL has a special status in the sense that filtering in ADSL impacts filtering in the other datasets
        # example: ADLB_FILTERED_ALONE is ADLB with filter applied
        # ADLB_FILTERED is ADLB_FILTERED_ALONE with only the (USUBJID, STUDYID) combinations appearing in ADSL_FILTERED
        filtered_alone <- paste0(dataname, "_FILTERED_ALONE")
        filtered_joined_adsl <- paste0(dataname, "_FILTERED") # filtered_alone with

        filter_call <- private$get_subset_call(dataname, filtered_alone)
        keys <- self$get_data_attrs("ADSL")$keys$primary
        if (is.null(keys)) {
          # todo: print warning, should not happen
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
      private$on_hold <- TRUE
      return(invisible(NULL))
    },

    # reactivate filtering which can result in several datasets being filtered
    continue_filtering = function() {
      private$on_hold <- FALSE
      private$apply_filter() # rerun all filtering
      return(invisible(NULL))
    },

    # todo: where is this function used
    # set class variables to default values, i.e. empty lists or NULL
    # following attributes are not set: attr, on_hold
    reset_class_vars = function(dataname = NULL) {
      stopifnot(is.null(dataname) || is_character_single(dataname))

      # if dataset is ADSL, we need to reset filtered datasets
      # to simplify, here we simply reset the other datasets as well then
      datanames <- if (is.null(dataname) || (dataname == "ADSL")) {
        self$datanames() # delete all datasets
      } else {
        private$check_data_varname(dataname)
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
        private$data_attr[[name]] <- list()
      }

      return(invisible(self))
    }
  ),

  ## __Private Methods---------------------

  private = list(

    # ---- attributes

    # the following attributes are (possibly reactive lists) per dataname
    datasets = NULL, # unfiltered datasets, todo: rename to unfiltered_datasets
    data_attr = NULL,  # attributes for dataname, todo: rename to data_attrs
    filtered_datasets = NULL, # filtered dataset after applying filter to unfiltered dataset
    # filter to apply to obtain filtered dataset from unfiltered one, NULL (for a dataname) means
    # no filter applied: it does not mean that it does not show up as a filtering element,
    # NULL just means that filtering has no effect
    filter_state = NULL,
    # filter characteristics to create filter that has no effect (i.e. full range of variable),
    # useful for UI to show sensible ranges
    filter_chars = NULL,

    # whether filtering is currently performed or it is postponed, useful when several fields
    # are changed at once and it should not run for each of the steps, but just once at the end
    on_hold = FALSE, # todo: rename to filter_onhold or wait_with_filtering
    attr = list(), # attributes of this class retrievable with get_attrs and set_attrs # todo: rename to attrs

    # methods ----

    # checks if dataname is in datanames (of this class) and
    # (if provided) that varname is a valid column in data
    # todo: rename to check_dataname_valid
    check_data_varname = function(dataname, varname = NULL) {
      if (!(dataname %in% self$datanames())) {
        stop(paste("data", dataname, "is not available"))
      }

      if (!is.null(varname) && !(varname %in% names(self$get_data(dataname)))) {
        stop(paste("variable", varname, "is not in data", dataname))
      }

      return(invisible(NULL))
    },

    # helper function to detect variable type (categorical, numeric range)
    # and accordingly set the filter characteristics
    set_filter_chars = function(dataname) {
      df <- self$get_data(dataname)

      fi <- Map(function(var, varname) {
        if (all(is.na(var))) {
          .log("all elements in", varname, "are NA")
          list(
            type = "unknown",
            label = "",
            class = class(var)
          )
        } else if (is.factor(var) || is.character(var)) {
          list(
            type = "choices",
            label = if_null(attr(var, "label"), ""),
            choices = if (is.factor(var)) {
              levels(var)
            } else {
              sort(unique(as.character(var)))
            }
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
            # todo: document what "TRUE or FALSE" is doing, it should just be "TRUE" and "FALSE" both
            # selected, probably not needed as separate option
            choices = c("TRUE", "FALSE", "TRUE or FALSE")
          )
        } else {
          .log("variable '", varname, "' is of class '",
               class(var), "' which has currently no filter UI element", sep = "")
          list(
            type = "unknown",
            label = "",
            class = class(var)
          )
        }
      }, df, names(df))

      private$filter_chars[[dataname]] <- setNames(fi, names(df))
      return(private$filter_chars[[dataname]])
    },

    validate = function() {
      # number of tests to check whether the FilteredData object is consistent
      msg <- NULL
      is_valid <- TRUE
    },

    # todo: rename out to out_dataname, maybe even remove it or default to NULL (no assignment)
    # creates a call that assigns the operates on dataname to obtain the filtered dataset and assigns it to the variable named out
    # as opposed to get_filter_call, this function only returns the call that affects the dataset without accounting for ADSL
    get_subset_call = function(dataname, out) {
      fs <- isolate(private$filter_state[[dataname]]) # todo: why isolate

      data_filter_call <- if (length(fs) == 0) {
        # no filtering applied to the dataset
        call("<-", as.name(out), as.name(dataname))
      } else {

        fi <- private$filter_chars[[dataname]]
        data_filter_call_items <- Map(function(name) {

          type <- fi[[name]]$type
          state <- fs[[name]]

          # switch below cannot handle NULL
          if (is.null(type)) stop(paste("filter type for variable", name, "in", dataname, "not known"))

          filter_call <- switch(
            type,
            choices = {
              if (length(state) == 1) {
                call("==", as.name(name), state)
              } else {
                call("%in%", as.name(name), state)
              }
            },
            range   = call("(", call("&", call(">=", as.name(name), state[1]), call("<=", as.name(name), state[2]))),
            logical = {
              switch(
                state,
                "TRUE" = as.name(name),
                "FALSE" = call("!", as.name(name)),
                "TRUE or FALSE" = call("%in%", as.name(name), c(TRUE, FALSE)) # todo: is the purpose to filter out NA
              )
            },
            stop(paste("filter type for variable", name, "in", dataname, "not known"))
          )
          # allow NA as well, i.e. do not filter it out (todo: also provide an option for this)
          # todo: add again: call("|", filter_call, call("is.na", as.name(name)))
        }, names(fs))

        # concatenate with "&" when several filters need to be applied to this dataset
        # when the list has a single element, reduce simply returns the element
        combined_filters <- Reduce(function(x, y) call("&", x, y), data_filter_call_items)

        # do not use subset (this is a convenience function intended for use interactively, stated in the doc)
        # as a result of subset and filter, NA is filtered out; this can be circumvented by explicitly
        # adapting the filtering condition above to watch out for NAs and keep them
        return(as.call(list(as.name("<-"), as.name(out), call("subset", as.name(dataname), combined_filters))))
      }

    },

    # apply filter unless on_hold is true (in which case filtering is postponed)
    # i.e. set filtered data from unfiltered data
    # when the data is set in the beginning and ADSL is not first, the filtered data is not set
    # once ADSL is set, one must explicitly call this function again so that the filtered_datasets
    # for all datasets will be set
    # todo: return if filter was actually applied or delayed
    apply_filter = function(dataname = NULL) {
      if (private$on_hold) {
        return(FALSE)
      }

      if (identical(dataname, "ADSL")) {
        # re-run all filters
        dataname <- NULL
      }

      .log("apply filter for", dataname) # todo: write something similar to withr: with_logging(code)

      if (is.null(self$get_data("ADSL", filtered = FALSE))) {
        # ADSL was not yet provided with set_data, so we remove also filters for all datasets

        # todo: why is this here? an error should be thrown that the filter cannot be applied

        for (name in self$datanames()) {
          private$filtered_datasets[[name]] <- NULL
        }

        return(FALSE)
      } else {

        # filter data directly in an empty environment
        e <- new.env(parent = globalenv()) # todo: globalenv is bad style, should be removed

        # todo: review horrible code below
        # todo: why not use a chunks object for this?
        # todo: don't do ADSL outside of this loop, just make sure that it runs first so the other datasets work

        # ADSL
        e$ADSL <- self$get_data("ADSL", filtered = FALSE) # nolint

        eval(self$get_filter_call("ADSL", merge = FALSE, adsl = FALSE), e)

        if (is.null(dataname)) {
          # all filters

          # filter all except adsl
          dnnadsl <- setdiff(self$datanames(), "ADSL")

          for (name in dnnadsl) {

            df <- self$get_data(name, filtered = FALSE)

            if (is.null(df)) {
              # why would it be NULL? simply not do anything?
              e[[paste0(name, "_FILTERED")]] <- NULL
            } else {
              e[[name]] <- self$get_data(name, reactive = FALSE, filtered = FALSE)
              calls <- self$get_filter_call(name, merge = TRUE, adsl = FALSE)
              eval(calls[[1]], e)
              eval(calls[[2]], e)
            }
          }

          ## to not trigger multiple events
          private$filtered_datasets[["ADSL"]] <- e[["ADSL_FILTERED"]]
          for (name in dnnadsl) {
            fdn <- paste0(name, "_FILTERED")
            private$filtered_datasets[[name]] <- e[[fdn]]
          }

        } else {
          # refilter single dataset
          fdn <- paste0(dataname, "_FILTERED")

          df <- self$get_data(dataname, filtered = FALSE)

          if (is.null(df)) {
            e[[FDN]] <- NULL
          } else {
            e[[dataname]] <- df
            calls <- self$get_filter_call(dataname, merge = TRUE, adsl = FALSE)
            eval(calls[[1]], e)
            eval(calls[[2]], e)
          }

          private$filtered_datasets[[dataname]] <- e[[fdn]]
        }
        rm(e) # todo: not needed

        return(TRUE)
      }
    }
  )
)

# todo: remove eventually
# warns that reactive = TRUE
warn_if_nonreactive <- function(reactive) {
  stopifnot(is_logical_single(reactive))
  if (!reactive) {
    # todo: add again
    #warning("Please use reactive = TRUE and instead isolate your call")
  }
}
