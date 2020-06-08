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
#' Other classes then take care of actually merging together all the datasets.
#'
#' This class is `ADSL`-centric in the sense that `ADSL` is required to apply the filters.
#' Every dataset, in addition to its own filter, is filtered to make sure that it only
#' contains keys present in `ADSL` (defaulting to `(USUBJID, STUDYID)`).
#' Once the `ADSL` dataset is set, the filters are applied.
#'
#' The datasets are filtered lazily, i.e. only when needed (in the Shiny app).
#'
#' By the design of the class (and `reactiveValues`), any dataname set through `set_data`
#' cannot be removed because other code may already depend on it. As a workaround, the
#' underlying data can be set to `NULL`.
#'
#' The class currently supports variables of the following types within datasets:
#' - `choices`: variable of type `factor`, e.g. `ADSL$COUNTRY`
#'      zero or more options can be selected, when the variable is a factor
#' - `logical`: variable of type `logical`, e.g. `ADSL$TRT_FLAG`
#'      exactly one option must be selected, `TRUE` or `FALSE`
#' - `ranges`: variable of type `numeric`, e.g. `ADSL$AGE`
#'      numerical range, a range within this range can be selected
#' Other variables cannot be filtered for.
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
#'   datasets
#'
#'   datasets$set_data("ADSL", ADSL)
#'   datasets
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
#'   datasets
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
    #' initialize a FilteredData object
    initialize = function() {
      # we have to create the `reactiveValues` here because they are R6 objects with reference
      # semantics and would otherwise be shared across classes
      private$datasets <- reactiveValues()
      private$filter_states <- reactiveValues()
    },
    #' @details
    #' Get datanames, ADSL appears first
    #' @md
    #' @return `character` vector of datanames
    datanames = function() {
      return(list_adsl_first(names(private$datasets)))
    },

    # getters and setters for attributes ----

    #' @details
    #' Get filtered or unfiltered dataset
    #'
    #' @param dataname `character` name of the dataset
    #' @param filtered `logical` whether to return filtered or unfiltered dataset
    get_data = function(dataname, filtered) {
      private$check_data_varname_exists(dataname)
      stopifnot(is_logical_single(filtered))

      if (filtered) {
        # We must be careful not to create a new reactive here as a new reactive
        # would be created each time we call this function. Instead, the reactive
        # conductors are created in the `set_data` method once only.
        # By creating it outside and storing it in the list, we benefit from
        # the reactive caching mechanism and lazy evaluation.
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
    #' @return `self` object of this class
    set_data = function(dataname, data) {
      stopifnot(is_character_single(dataname))
      if (grepl("[[:space:]]", dataname)) {
        stop(paste0("dataname '", dataname, "' must not contain spaces.", collapse = "\n"))
      }
      stopifnot(is.data.frame(data))
      # column_labels and data_label may be NULL, so attributes will not be present
      stopifnot("keys" %in% names(attributes(data)))

      # due to the data update, the old filter may no longer be valid, so we unset it
      private$filter_states[[dataname]] <- list()
      private$previous_filter_states[[dataname]] <- list()

      is_new_dataname <- !dataname %in% self$datanames()

      # save `md5sum` for reproducibility
      attr(data, "md5sum") <- digest(data, algo = "md5")
      private$datasets[[dataname]] <- data

      if (is_new_dataname) {
        # new dataname
        # We only want to set the reactive when a new dataname is added.
        # Otherwise, we don't want to update the reactive as we would otherwise
        # lose all observers listening the old reactive.
        # The below functions check that the dataname already exists, so we already set it above.
        private$filter_infos[[dataname]] <- private$reactive_filter_infos(dataname)
        private$filtered_datasets[[dataname]] <- private$reactive_filtered_dataset(dataname)
      }

      return(invisible(self))
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
      private$check_data_varname_exists(dataname)
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
      private$check_data_varname_exists(dataname)
      stopifnot(is_character_single(attr))

      attr(private$datasets[[dataname]], attr) <- value
      return(invisible(NULL))
    },

    # no corresponding set_ function
    #' @details
    #' Get non-NA labels of variables in the data
    #'
    #' Variables may be columns.
    #'
    #' @md
    #' @param dataname `character` name of the dataset
    #' @param variables (`character` vector) variables to get labels for;
    #'   if `NULL`, for all variables
    get_variable_labels = function(dataname, variables = NULL) {
      private$check_data_varname_exists(dataname)
      stopifnot(is.null(variables) || is_character_empty(variables) || is_character_vector(variables))

      labels <- self$get_data_attr(dataname, "column_labels")
      if (!is.null(variables)) {
        labels <- labels[variables]
      }
      labels <- labels[!is.na(labels)]

      return(labels)
    },

    # filtering state and characteristics, remove and continue filtering ----

    #' @details
    #' Return filter characteristics for a variable in a dataset
    #' e.g. `range` for a `numeric` variable, `choices` for a
    #' `factor` variable, `logical` for a `logical` variable.
    #'
    #' This can be used to see how the variable must be filtered.
    #'
    #' @md
    #' @param dataname `character` name of the dataset
    #' @param varname `character` column within the dataset,
    #'   if `NULL`, return all (filtered) variables
    #' @param all_filterable_vars `logical` (only applies if `varname` is NULL)
    #'   whether to include non-filtered variables; only filterable variables
    #'   are included
    get_filter_info = function(dataname, varname = NULL, all_filterable_vars = FALSE) {
      private$check_data_varname_exists(dataname, varname)
      stopifnot(
        is_logical_single(all_filterable_vars),
        is.null(varname) || !all_filterable_vars
      )

      infos <- private$filter_infos[[dataname]]()
      return(if (is.null(varname)) {
        # filter out variables that are not filtered
        if (all_filterable_vars) {
          # all filterable variables
          infos[vapply(infos, function(var_info) var_info$type != "unknown", logical(1))]
        } else {
          # only filtered variables, a subset of filterable variables
          infos[names(private$filter_states[[dataname]])]
        }
      } else {
        infos[[varname]]
      })
    },

    #' @details
    #' See `get_filter_info`, only returns filter type (`choices`, `range`, `logical`).
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
      private$check_data_varname_exists(dataname, varname)

      if (is.null(varname)) {
        private$filter_states[[dataname]]
      } else {
        return(private$filter_states[[dataname]][[varname]])
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
      private$check_data_varname_exists(dataname, varname)

      # checking and adapting arguments
      if (!is.null(varname)) {
        state <- setNames(list(state), varname)
        # we don't set varname to NULL so we can detect it below and only set the state of varname
      }
      stopifnot(is.list(state))

      # each item in the state list indicates what filter to apply to that column
      state_names <- names(state)

      # check if all names of state are columns of the dataname
      inexistent_columns <- which(!(state_names %in% colnames(self$get_data(dataname, filtered = FALSE))))
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
      if (isTRUE(all.equal(private$filter_states[[dataname]], new_state))) {
        return(FALSE)
      }


      private$previous_filter_states[[dataname]] <- private$filter_states[[dataname]]
      private$filter_states[[dataname]] <- new_state

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
      private$check_data_varname_exists(dataname, varname)

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
      private$check_data_varname_exists(dataname, varname)

      state <- private$previous_filter_states[[dataname]][[varname]]
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
    is_filterable = function(dataname, varname) {
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
    #' Note: The `dplyr::inner_join()` function in returned call will fail if the
    #' corresponding dataset is NULL.
    #'
    #' @md
    #' @param dataname `character` name of the dataset
    #' @return call to filter dataset (taking out patients not in ADSL)
    get_filter_call = function(dataname) {
      private$check_data_varname_exists(dataname)

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
        filtered_joined_adsl <- paste0(dataname, "_FILTERED") # filtered_alone with

        keys <- self$get_data_attr("ADSL", "keys")$primary
        stopifnot(!is.null(keys)) #non-NULL because dataset is not ADSL
        # filter additionally to only have combinations of keys that are in ADSL_FILTERED
        merge_call <- call(
          "<-", as.name(filtered_joined_adsl),
          call_with_colon(
            "dplyr::inner_join", # better than merge since we use `dplyr` everywhere
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
    #' Print method
    #'
    #' To obtain the default printing of R6 objects with method listing,
    #' use `cat(format(x))`.
    #'
    #' @md
    #' @param ... unused arguments
    print = function(...) {

      # to filter all datasets before the print message as they are only lazily evaluated
      private$update_all_filtered_data()

      cat(class(self), "object", "\n")

      datanames <- self$datanames()
      if (length(datanames) == 0) {
        cat("- Contains currently no Datasets.", "\n")
      } else {
        cat("- Contains Datasets:", "\n")
        cat("     dataname (dim. unfiltered) (dim. filtered)", "\n")
        lapply(datanames, function(name) {
          cat("   -", name,
              paste0("(", paste(dim(self$get_data(name, filtered = FALSE)), collapse = " x "), ")"),
              paste0("(", paste(dim(self$get_data(name, filtered = TRUE)), collapse = " x "), ")"), "\n"
          )
          fs <- self$get_filter_info(name)
          if (length(fs) > 0) {
            cat("      - active filter variables:", paste(names(fs), collapse = ", "), "\n")
          }
        })
      }

      cat("- Introspect this object (x) with the following methods:", "\n")
      cat("   - cat(format(x))", "\n")
      if (length(datanames) > 0) {
        cat('   - x$print_filter_info("ADSL")', "\n") #nolintr
        cat('   - x$get_data_info("ADSL", filtered = FALSE)', "\n") #nolintr
      }
      return(invisible(NULL))
    },

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
      private$check_data_varname_exists(dataname)
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
      private$check_data_varname_exists(dataname)
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
    #' `previous_filter_states` is not bookmarked.
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
        filter_states = reactiveValuesToList(private$filter_states),
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
    restore_state_from_bookmark = function(state, check_data_md5sums = TRUE) {
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

    # reactiveValues attributes (reactive sources)
    # `reactiveValues` with one entry per dataname

    # unfiltered / raw datasets
    # e.g. for two datasets `ADSL` and `ADAE` we would have
    # nolintr start
    # datasets <- reactiveValues(
    #   ADSL = <object that inherits from data.frame>,
    #   ADAE =  <object that inherits from data.frame>
    # )
    # nolintr end
    # the `data.frames` may have attributes like keys
    datasets = NULL, # reactiveValues(),
    # filter state to apply to obtain the filtered dataset from the unfiltered one
    # `NULL` (for a dataname) means no filter applied, it does not mean that it does not show up
    # as a filtering item in the UI, `NULL` just means that filtering has no effect
    # therefore, it may not contain all variables for a given dataname,
    # which means that the state is `NULL`
    filter_states = NULL, # reactiveValues(),

    # reactive attributes (conductors)
    # non-reactive list of `reactives``
    # These are computed from other reactive inputs like the above
    # The rule is: Always store unevaluated reactive expressions, never store evaluated
    # expressions. Evaluated reactive expressions are no longer reactive.
    # We must be careful not to evaluate it in the wrong context. Suppose that
    # `set_data` adds the data into a reactive slot. When we simultaneously register the
    # reactive expression to obtain the filtered dataset, we should not evaluate it because
    # we do not want to re-evaluate the context of `set_data` each time the filter state changes.
    # Instead, we want to re-evaluate the context of `get_data` which actually depends on the
    # data.
    # For caching, we must also make sure that we don't recreate identical reactive expressions.
    # Instead, we create them once the data is set in `set_data`.

    # list of reactive expressions to obtain filtered datasets
    filtered_datasets = list(),
    # list of filter characteristics to create filter that has no effect (i.e. full range of variable),
    # useful for UI to show sensible ranges and initial state
    filter_infos = list(),

    # non-reactive attributes

    # previous filter state when you want to revert, e.g. when filter previously removed is added again
    # we currently don't want reactivity for it, it is enough for `filter_states`
    previous_filter_states = list(),
    # preprocessing code used to generate the unfiltered datasets as a string
    preproc_code = NULL,

    # check functions ----
    # we implement these functions as checks rather than returning logicals so they can
    # give informative error messages immediately

    # @details
    # Validate object to inspect if something is wrong
    # The call to this function should be isolated to avoid a reactive dependency.
    # Getting the names of a reactivevalues also needs a reactive context.
    validate = function() {
      .log("## validating FilteredData object consistency")

      stopifnot(
        # check classes
        is.reactivevalues(private$datasets),
        # no `reactiveValues`, but a list of `reactiveVal`
        all(vapply(private$filtered_datasets, is.reactive, logical(1))),
        is.reactivevalues(private$filter_states),
        is.list(private$previous_filter_states),
        is.list(private$filter_infos),

        # check names are the same
        setequal(names(private$datasets), names(private$filtered_datasets)),
        setequal(names(private$datasets), names(private$filter_infos)),
        all_true(self$datanames(), function(dataname) !is.null(attr(private$datasets[[dataname]], "md5sum"))),
        setequal(names(private$datasets), names(private$filter_states)),
        setequal(names(private$datasets), names(private$previous_filter_states))
      )

      # check filter_states
      lapply(names(private$filter_states), function(dataname) {
        lapply(names(private$filter_states[[dataname]]), function(varname) {
          private$check_valid_filter_state(
            dataname,
            varname = varname,
            var_state = private$filter_states[[dataname]][[varname]]
          )
        })
      })

      # check previous_filter_states
      lapply(names(private$previous_filter_states), function(dataname) {
        lapply(names(private$previous_filter_states[[dataname]]), function(varname) {
          private$check_valid_filter_state(
            dataname,
            varname = varname,
            var_state = private$previous_filter_states[[dataname]][[varname]]
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
    check_data_varname_exists = function(dataname, varname = NULL) {
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
      stopifnot(is_character_single(varname)) # must be non-NULL
      private$check_data_varname_exists(dataname, varname)

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
      private$check_data_varname_exists(dataname)

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

    # Reactive to compute the filtered dataset
    # @details
    # This returns a reactive that computes the filtered dataset.
    # Through reactivity, it is only lazily evaluated and also re-evaluates
    # whenever the unfiltered datasets it depends on change.
    #
    # @md
    # @param dataname `character` name of the dataset
    # @return `reactive` expression for the provided `dataname` that returns
    #   the filtered dataset when evaluated
    reactive_filtered_dataset = function(dataname) {
      private$check_data_varname_exists(dataname)

      reactive({
        .log("################################################# Refiltering dataset ", dataname)

        # We could use a chunks object here, but don't do so for simplicity as we don't need its capabilities.
        # We take the parent of the global env, i.e. the last attached package, to avoid picking up
        # any variables defined in the global environment. This is okay as it takes whatever is the current
        # `globalenv` when this reactive is evaluated, i.e. when the relevant packages are already loaded.
        env <- new.env(parent.env(globalenv()))

        # put dependencies of filter call into environment
        if (dataname != "ADSL") {
          # need to add ADSL_filtered as filter call for dataset depends on it
          env[["ADSL_FILTERED"]] <- self$get_data("ADSL", filtered = TRUE)
        }
        env[[dataname]] <- self$get_data(dataname, filtered = FALSE)

        lapply(
          # 1 call for ADSL to apply filter, 2 calls for non-ADSL to additionally filter out rows not in ADSL
          self$get_filter_call(dataname),
          eval,
          envir = env
        )

        return(env[[paste0(dataname, "_FILTERED")]])
      })
    },

    # @details
    # Update all the filtered datasets.
    #
    # Datasets do not get calculated if there is no absorbing observer requiring
    # the dataset (lazy feature of reactivity).
    # This function ensures that all the filtered datasets are updated
    # @md
    # @return `self`
    update_all_filtered_data = function() {
      lapply(self$datanames(), function(x) self$get_data(x, filtered = TRUE))
      return(invisible(self))
    },

    # Reactive to compute the filter infos
    # @details
    # Reactive to compute the filter infos for a `dataname`
    #
    # For each variable in the dataset, it computes the filter info.
    # This filter info depends on the variable type (`numeric`, `factor`, `logical`)
    # and returns the least restrictive filter state, i.e.
    # - `factor or character`: `type: "choices", choices: , histogram_data: `
    # - `numeric`: `type: "range", range: , histogram_data: `
    # - `logical`: `type: "logical", choices: , histogram_data: `
    # - default (or all `NA` entries): `type: unknown, class: `, cannot be filtered
    # Each variable's filter info is a list with entries `type`, `labels` and others
    # that depend on the variable type as outlined above.
    #
    # @md
    # @param dataname `character` name of the dataset
    #
    # @return reactive that returns a list of variable infos, one entry per variable
    reactive_filter_infos = function(dataname) {
      private$check_data_varname_exists(dataname)

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
#'   type; can be `filter_state` or `filter_info`
#' @param add_na `logical` whether to display information about NA,
#'   either `keep_na` (for `filter_state`) or `na_count` (for `filter_info`)
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
