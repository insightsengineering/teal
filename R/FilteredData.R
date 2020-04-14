#' @name FilteredData
#' @docType class
#' @title Class to encapsulate filtered data sets
#'
#' @details This class encapsulates data import and filtering.
#'   Once a dataname was specified it won't be deleted at runtime to keep
#'   reactive relations intact. If a dataset is not needed anymore then you can
#'   set it to NULL with load_data.
#'   Every data set is also filtered with ADSL, hence ADSL needs to be provided
#'
#' load_data
#' 1 load data
#' 2 specify filter_info
#' 3 apply filters
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
#' x$get_filter_info("ADSL")
#'
#' df <- x$get_data("ADSL")
#'
#' x$get_filter_info("ADSL")[['USUBJID']]$type
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

      private$init_datanames <- datanames

      create_rv <- function() {
        do.call(reactiveValues, setNames(lapply(datanames, function(x) NULL), datanames))
      }

      # create reactiveValues for unfiltered and filtered dataset and filter state
      # each reactiveValues is a list with one entry per dataset name
      private$datasets          <- create_rv()
      private$filtered_datasets <- create_rv()
      private$filter_state      <- create_rv()

      return(invisible(self))
    },

    datanames = function() {
      # todo: why isolate here?
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
      private$error_if_not_valid(dataname)

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

      private$update_filter_info(dataname)
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
      # todo: put this into initialize
      if (!dataname %in% names(private$data_attr)) {
        private$data_attr[[dataname]] <- list()
      }

      return(private$data_attr[[dataname]][[attr]] <- value)
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

    # get non-NULL labels of columns in dataname
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

    list_data_info = function(dataname, filtered = FALSE, variables = NULL) {
      stopifnot(is_character_single(dataname))
      stopifnot(is_logical_single(filtered))
      stopifnot(is.null(variables) || is_character_vector(variables))
      private$error_if_not_valid(dataname)

      # log with newline
      log2 <- function(...) {
        cat(paste(..., collapse = " ")); cat("\n")
      }

      log2("====", dataname, "=======================")

      df <- isolate(private$datasets[[dataname]])
      if (is.null(df)) {
        log2("The", dataname, "data is NULL") # not set yet with set_data
      } else {
        fi <- private$filter_info[[dataname]]

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

    # delete all data (set to NULL) or only dataname
    # todo: where is this function used
    reset_data = function(dataname = NULL) {
      stopifnot(is.null(dataname) || is_character_single(dataname))

      datanames <- if (is.null(dataname)) {
        self$datanames() # delete all datasets
      } else {
        private$error_if_not_valid(dataname)
        dataname
      }

      for (name in datanames) {
        private$datasets[[name]]                <- NULL
        private$filtered_datasets[[name]]       <- NULL
        private$filter_state[[name]]            <- NULL
        private$filter_info[[name]]             <- NULL
      }

      # run if only adsl data was reset
      if (identical(dataname, "ADSL")) {
        private$apply_filter() # todo: why is it here: it won't do anything because datasets is NULL
      }

      return(invisible(self))
    },

    # check data with dataname is non NULL
    has_data = function(dataname) {
      stopifnot(is_character_single(dataname))

      (dataname %in% self$datanames()) &&
        !is.null(self$get_data(dataname))
    },

    # check dataname exists and contains column varname
    has_variable = function(dataname, varname) {
      stopifnot(is_character_single(dataname))
      stopifnot(is_character_single(varname))

      self$has_data(dataname) && (varname %in% names(self$get_data(dataname)))
    },

    # reactive: whether returned expression should be isolated or not
    # filtered: return filtered or full dataset
    get_data = function(dataname, reactive = FALSE, filtered = FALSE) {
      stopifnot(is_character_single(dataname))
      stopifnot(is_logical_single(reactive))
      stopifnot(is_logical_single(filtered))

      private$error_if_not_valid(dataname)

      # isolate if not reactive
      f <- if (reactive) identity else isolate
      if (filtered) {
        f(private$filtered_datasets[[dataname]])
      } else {
        f(private$datasets[[dataname]])
      }
    },

    get_data_info = function(dataname, filtered = TRUE, reactive = FALSE) {
      stopifnot(is_character_single(dataname))
      stopifnot(is_logical_single(reactive))
      stopifnot(is_logical_single(filtered))

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
    get_filter_info = function(dataname, varname = NULL) {
      stopifnot(is_character_single(dataname))
      stopifnot(is.null(varname) || is_character_single(varname))
      private$error_if_not_valid(dataname, varname)

      if (is.null(varname)) {
        private$filter_info[[dataname]]
      } else {
        private$filter_info[[dataname]][[varname]]
      }
    },

    # similar to get_filter_info, only return data type (numerical, categorical)
    get_filter_type = function(dataname, varname) {
      stopifnot(is_character_single(dataname))
      stopifnot(is_character_single(varname))
      private$error_if_not_valid(dataname, varname)

      private$filter_info[[dataname]][[varname]][["type"]]
    },

    # todo: reorder arguments for default arguments to be last
    # varname = NULL -> set state for all variables in dataname, state is a named list with one item per column (name)
    # state = NULL erase filter state -> use remove_filter
    #
    # varname != NULL and state is missing, set to default value
    set_filter_state = function(dataname, varname = NULL, state, initial = FALSE) {
      stopifnot(is_character_single(dataname))
      stopifnot(is.null(varname) || is_character_single(varname))
      private$error_if_not_valid(dataname, varname)

      browser()

      if (is.null(state)) {
        stop("use remove_filter and not set_filter_state to remove a filter")
      }

      if (is.null(varname)) {
        if (!(is.null(state) || is.list(state))) {
          stop("filter state needs to be a list when specified for all variables")
        }

        fs_names <- names(state)
        varnames <- names(self$get_data(dataname))

        # check if variables exist
        non_valid_vars_i <- which(!(fs_names %in% varnames))
        if (length(non_valid_vars_i) > 0) {
          stop(paste("variables", toString(fs_names[non_valid_vars_i]),
                     "are not available in data", dataname))
        }

        # check if filter state is possible
        fi <- private$filter_info[[dataname]]

        for (var in fs_names) {
          fii <- fi[[var]]
          state_i <- state[[var]]

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

        private$filter_state[[dataname]] <- state

      } else {
        # single varname given

        fii <- self$get_filter_info(dataname, varname)
        state_i <- state
        var <- varname
        switch(
          fii$type,
          choices = {
            if ((length(state_i) > 0) && any(!(state_i %in% fii$choices)))
              stop(paste("data", dataname, "variable", var, "not all valid choices"))
          },
          range = {
            if (length(state_i) != 2)
              stop(paste("data", dataname, "variable", var, "not of length 2"))
          },
          logical = NULL,
          stop(paste("data", dataname, "variable", var, "has unknown type ", fii$type))
        )

        fs <- if_null(self$get_filter_state(dataname), list())
        fs[[varname]] <- state
        private$filter_state[[dataname]] <- fs
      }

      if (!initial) {
        private$apply_filter(dataname)
      }

      return(invisible(self))
    },

    # removes the filter and resets the default state corresponding to no state change at all
    remove_filter = function(dataname, varname) {
      stopifnot(is_character_single(dataname))
      stopifnot(is_character_single(varname))
      private$error_if_not_valid(dataname, varname)

      browser()

      current_state <- self$get_filter_state(dataname) # state before removal
      new_state <- private$filter_info[[dataname]][[varname]][["init_state"]]
      browser() # todo: check stopifnot
      stopifnot(!is.null(new_state)) # initial state should have been set when set_filter_state was called

      # all.equal returns TRUE if no difference, otherwise a character vector with changes
      needs_apply_filter <- is.character(all.equal(current_state[[varname]], new_state))

      current_state[[varname]] <- NULL

      self$set_filter_non_executed(dataname, varname, NULL)

      private$filter_state[[dataname]] <- current_state
      private$filter_info[[dataname]][[varname]][["executed"]] <- NULL

      if (needs_apply_filter) {
        private$apply_filter(dataname)
      }

      return(invisible(self))
    },

    get_filter_state = function(dataname, varname = NULL, reactive = FALSE) {
      stopifnot(is_character_single(dataname))
      stopifnot(is.null(varname) || is_character_single(varname))
      stopifnot(is_logical_single(reactive))
      private$error_if_not_valid(dataname, varname)

      if (is.null(varname)) {
        return(if (reactive) {
          private$filter_state[[dataname]]
        } else {
          isolate(private$filter_state[[dataname]])
        })
      } else {
        if (reactive) {
          # todo: document why?
          stop("cannot access reactive value for individual variable")
        }

        return(isolate(private$filter_state[[dataname]][[varname]]))
      }
    },

    # default filter state is as if no filtering were occurring, i.e. filtering for all
    # todo: used where?
    set_default_filter_state = function(dataname, varname) {
      stopifnot(is_character_single(dataname))
      stopifnot(is_character_single(varname))
      private$error_if_not_valid(dataname, varname)

      fi <- self$get_filter_info(dataname, varname)
      default_state <- switch(
        fi$type,
        choices = fi$choices,
        range = fi$range,
        logical = "TRUE or FALSE",
        stop("unknown filter type", fi$type)
      )

      self$set_filter_state(dataname, varname, default_state, initial = TRUE)
      self$set_filter_non_executed(dataname, varname, default_state)

      return(invisible(self))
    },

    # todo: rename to set_filter_not_executed
    set_filter_non_executed = function(dataname, varname, state) {
      private$filter_info[[dataname]][[varname]][["executed"]] <- FALSE
      private$filter_info[[dataname]][[varname]][["init_state"]] <- state
      return(invisible(NULL))
    },

    set_filter_executed = function(dataname, varname) {
      private$filter_info[[dataname]][[varname]][["executed"]] <- TRUE
      return(invisible(NULL))
    },

    # todo: rename to get_filter_executed
    get_filter_execution = function(dataname, varname) {
      return(private$filter_info[[dataname]][[varname]][["executed"]])
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
      private$error_if_not_valid(dataname)

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
    }
  ),

  ## __Private Methods---------------------

  private = list(

    init_datanames = NULL, #todo: remove: used nowhere
    datasets = NULL, # todo: rename to unfiltered_datasets
    attr = list(), # attributes of this class retrievable with get_attrs and set_attrs # todo: rename to attrs
    data_attr = list(),  # list with one item per dataname containing attributes for dataname, todo: rename to data_attrs
    filtered_datasets = NULL,
    filter_state = NULL, # filter to apply to obtain filtered dataset from unfiltered one
    filter_info = list(), # info about dataset and filter applied with fields: type, label, class, init_state, executed (filter applied already, when on_hold is on)
    # whether filtering is currently performed or it is postponed, useful when several fields
    # are changed at once and it should not run for each of the steps, but just once at the end
    on_hold = FALSE, # rename to filter_hold or wait_with_filtering

    # checks if dataname is in datanames (of this class) and
    # (if provided) that varname is a valid column in data
    # todo: rename to check_dataname_valid
    error_if_not_valid = function(dataname, varname = NULL) {
      if (!(dataname %in% self$datanames())) {
        stop(paste("data", dataname, "is not available"))
      }

      if (!is.null(varname) && !(varname %in% names(self$get_data(dataname)))) {
        stop(paste("variable", varname, "is not in data", dataname))
      }

      return(invisible(NULL))
    },

    # detect variable type (categorical, numeric range)
    # does not set executed and init_state fields
    # rename to update_dataname_type
    update_filter_info = function(dataname) {
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

      private$filter_info[[dataname]] <- setNames(fi, names(df))
      return(private$filter_info[[dataname]])
    },

    validate = function() {
      # number of tests to check whether the FilteredData object is consistent
      msg <- NULL
      is_valid <- TRUE
    },

    # todo: rename out to out_dataname, maybe even remove it or default to NULL (no assignment)
    # creates a call that assigns the operates on dataname to obtain the filtered dataset and assigns it to the variable named out
    get_subset_call = function(dataname, out) {
      fs <- isolate(private$filter_state[[dataname]]) # todo: why isolate

      data_filter_call <- if (length(fs) == 0) {
        # no filtering applied to the dataset
        call("<-", as.name(out), as.name(dataname))
      } else {

        fi <- private$filter_info[[dataname]]
        data_filter_call_items <- Map(function(name) {

          type <- fi[[name]]$type
          state <- fs[[name]]

          # switch below cannot handle NULL
          if (is.null(type)) stop(paste("filter type for variable", name, "in", dataname, "not known"))

          switch(
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
        }, names(fs))

        # concatenate with "&" when several filters need to be applied to this dataset
        # when the list has a single element, reduce simply returns the element
        condition <- Reduce(function(x, y) call("&", x, y), data_filter_call_items)

        return(as.call(list(as.name("<-"), as.name(out), call("subset", as.name(dataname), condition))))
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
