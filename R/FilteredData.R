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
#' @importFrom digest digest
#'
#' @examples
#' datasets <- teal:::FilteredData$new()
#'
#' # to avoid using isolate(), you can provide a default isolate context by calling
#' # options(shiny.suppressMissingContextError = TRUE) #nolint
#' # don't forget to deactivate this option at the end
#' # options(shiny.suppressMissingContextError = FALSE) #nolint
#' isolate({
#'   datasets
#'   # we don't set keys because datasets are independent
#'   datasets$set_data("iris", iris)
#'   datasets$set_data("mtcars", mtcars)
#'
#'   datasets
#'
#'   datasets$datanames()
#'   datasets$get_data_info("iris", filtered = FALSE)
#'
#'   datasets$get_filter_type("iris", "Species")
#'   datasets$set_filter_state("iris", state = list(
#'     Sepal.Length = list(range = c(4.5, 4.9), keep_na = FALSE),
#'     Species = list(choices = c("setosa", "virginica"), keep_na = FALSE)
#'   ))
#'   datasets
#'   datasets$get_filter_type("iris", "Species")
#'   datasets$get_filter_info("iris")[["Species"]]
#'
#'   datasets$get_data("iris")
#'
#'   # will fail because of invalid range
#'   # datasets$set_filter_state("iris", list(
#'   #   Sepal.Length = list(range = c(3, 7), keep_na = FALSE)
#'   # ))
#'
#'   # wrapper functions
#'   teal:::set_single_filter_state(datasets, "mtcars", "cyl", c(4, 8))
#'   teal:::set_single_filter_state(datasets, "mtcars", "qsec", c(14.5, 16))
#'   datasets$get_data("mtcars")
#'
#'   datasets$get_filter_state("iris")
#'   datasets$get_filter_state("mtcars")
#'
#'   datasets$get_default_filter_state("iris", "Sepal.Length")
#'   datasets$get_filter_expr("iris")
#'
#'   datasets$print_filter_info("mtcars")
#' })
FilteredData <- R6::R6Class( # nolint
  "FilteredData",
  ## __Public Methods ====
  public = list(
    #' @description
    #' Initialize a `FilteredData` object
    initialize = function() {
      # we make these reactive values so that any change to them propagates
      # to reactive expressions that depend on them like `filtered_datasets`
      # we have to create the `reactiveValues` here because they are R6 objects with reference
      # semantics and would otherwise be shared across classes
      private$unfiltered_datasets <- reactiveValues()
      private$filter_states <- reactiveValues()
      return(invisible(self))
    },
    #' @description
    #' Get datanames
    #'
    #' The datanames are returned in the order in which they must be
    #' evaluated (in case of dependencies).
    #' @return (`character` vector) of datanames
    datanames = function() {
      return(as.character(names(private$unfiltered_datasets)))
    },

    #' @description
    #' Get variable names
    #'
    #' @param dataname (`character`) name of the dataset
    #' @return (`character` vector) of variable names
    get_varnames = function(dataname) {
      names(self$get_data(dataname, filtered = FALSE))
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

    # getters and setters for attributes ----

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

      if (filtered) {
        # We must be careful not to create a new reactive here as a new reactive
        # would be created each time we call this function. Instead, the reactive
        # conductors are created in the `set_data` method once only.
        # By creating it outside and storing it in the list, we benefit from
        # the reactive caching mechanism and lazy evaluation.
        private$filtered_datasets[[dataname]]()
      } else {
        private$unfiltered_datasets[[dataname]]
      }
    },

    #' @description
    #' Add data
    #'
    #' Will also add the hash sum of the data.
    #'
    #' Note: due to the nature of `reactiveValues()`, once a dataname
    #' is added, it cannot be removed anymore because some code may depend
    #' on it. You can try to achieve this by setting the data to NULL
    #' and the observers must then know that NULL means that the dataset
    #' was removed.
    #'
    #' Any attributes attached to the data are kept in the unfiltered data.
    #'
    #' @param dataname (`character`) name of the dataset, without spaces
    #' @param data (`data.frame`) data that corresponds to `dataname`
    #' @return (`self`) object of this class
    set_data = function(dataname, data) {
      stopifnot(is_character_single(dataname))
      # to include it nicely in the Show R Code; the UI also uses datanames in ids, so no whitespaces allowed
      check_simple_name(dataname)
      stopifnot(is.data.frame(data))

      is_new_dataname <- !dataname %in% self$datanames()

      private$unfiltered_datasets[[dataname]] <- data

      private$data_attrs[[dataname]] <- list()
      data_attrs <- attributes(data)
      data_attrs$hash <- digest::digest(data, algo = "md5")
      self$set_data_attrs(dataname, data_attrs)

      # due to the data update, the old filter may no longer be valid, so we unset it
      private$filter_states[[dataname]] <- list()

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

    #' @description
    #' Get the R preprocessing code string that generates the unfiltered datasets
    #' @param dataname (`character`) name(s) of dataset(s)
    #' @return (`character`) deparsed code
    get_code = function(dataname = self$datanames()) {
      return(paste0(private$code$get_code(dataname), collapse = "\n"))
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
    #' @description
    #' Get join keys between two datasets.
    #' @param dataset_1 (`character`) one dataset name
    #' @param dataset_2 (`character`) other dataset name
    #' @return (`named character`) vector with column names
    get_join_keys = function(dataset_1, dataset_2) {
      private$join_keys$get(dataset_1, dataset_2)
    },

    #' @description
    #' Get dataset primary keys
    #' @param dataname (`character`) name of the dataset
    #' @return (`character`) vector with column names
    get_primary_keys = function(dataname) {
      return(self$get_data_attr(dataname, "keys"))
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
      return(private$data_attrs[[dataname]][[attr]])
    },

    #' @description
    #' Set data attribute for the dataset
    #'
    #' @param dataname (`character`) name of the dataset
    #' @param attr attribute to get from the data attributes of the dataset
    #' @param value value to set attribute to
    #' @return (`self`) invisibly for chaining
    set_data_attr = function(dataname, attr, value) {
      private$check_data_varname_exists(dataname)
      stopifnot(is_character_single(attr))

      private$data_attrs[[dataname]][[attr]] <- value
      return(invisible(self))
    },
    #' @description
    #' Set data attributes for the dataset
    #'
    #' @param dataname (`character`) name of the dataset
    #' @param attrs (named `list`) data attributes of the dataset
    #' @return (`self`) invisibly for chaining
    set_data_attrs = function(dataname, attrs) {
      stopifnot(is_fully_named_list(attrs))
      for (idx in seq_along(attrs)) {
        self$set_data_attr(dataname, attr = names(attrs)[[idx]], value = attrs[[idx]])
      }
      return(invisible(self))
    },

    # Convenience functions to get data attributes ----

    #' @description `r lifecycle::badge("soft-deprecated")`
    #' Get labels of variables in the data
    #'
    #' @param dataname (`character`) name of the dataset
    #' @param variables (`character` vector) variables to get labels for;
    #'   if `NULL`, for all variables in data
    get_variable_labels = function(dataname, variables = NULL) {
      # lifecycle cannot handle deprecating class method
      # nolint start
      # lifecycle::deprecate_warn(
      #   "0.9.2",
      #   "teal:::FilteredData$public_methods$get_variable_labels()",
      #   details = "Please use FilteredData$get_varlabels() instead."
      # )
      # nolint end
      warning(paste(
        "`FilteredData$get_variable_labels()` is deprecated as of teal 0.9.2.",
        "Please use FilteredData$get_varlabels() instead.",
        sep = "\n"
      ))
      self$get_varlabels(dataname, variables)
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
      private$check_data_varname_exists(dataname)
      stopifnot(is.null(variables) || is_character_vector(variables, min_length = 0L))

      labels <- self$get_data_attr(dataname, "column_labels")
      if (is.null(labels)) {
        return(NULL)
      }

      if (!is.null(variables)) {
        check_in_subset(
          variables,
          self$get_varnames(dataname),
          pre_msg = paste0("Variables do not exist in data ", dataname, ": ")
        ) # otherwise, NA values will be added (also as names)
        labels <- labels[variables]
      }

      return(labels)
    },

    #' Get keys for the dataset
    #' @param dataname (`character`) name of the dataset
    #' @return (`character`) keys of dataset
    get_keys = function(dataname) {
      self$get_primary_keys(dataname)
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

    # Filter state, default filter and generated filter expression ----

    #' @description
    #' Return filter characteristics for a variable in a dataset
    #' e.g. `range` for a `numeric` variable, `choices` for a
    #' `factor` variable, `logical` for a `logical` variable.
    #'
    #' This can be used to see filtering conditions of a variable or a vector of variables.
    #' This method looks into `private$filter_infos` and returns a list of multiple `filter information` in case
    #' `varname` is a list or a named list of a single `filter information` in case `varname` is a singular name.
    #' The `include_unknown` argument is considered if and only when the `varname` is `NULL`.
    #'
    #' @param dataname (`character`) name of the dataset
    #' @param varname (`character`) vector of variable names to include in the result.
    #' Default: all variables in `dataname`
    #' @param include_unknown (`logical`) whether to include variables with unknown filter type.
    #' Is ignored when `varname` is not `NULL`. Default: TRUE
    #'
    #' @return a (named `list`) with filter information of `varname` variables.
    #' If `include_unknown` is TRUE, then also return `filter information`
    #' of variables with `filter information` of type unknown.
    #'
    #' @examples
    #' library(random.cdisc.data)
    #'
    #' ADSL <- radsl(cached = TRUE)
    #' attr(ADSL, "keys") <- get_cdisc_keys("ADSL")
    #' datasets <- teal:::FilteredData$new()
    #'
    #' # to avoid using isolate(), you can provide a default isolate context by calling
    #' # options(shiny.suppressMissingContextError = TRUE) #nolint
    #' # don't forget to deactivate this option at the end
    #' # options(shiny.suppressMissingContextError = FALSE) #nolint
    #'
    #' isolate({
    #'   datasets
    #'
    #'   datasets$set_data("ADSL", ADSL)
    #'   datasets
    #'   datasets$get_data_info("ADSL", filtered = FALSE)
    #'   datasets$datanames()
    #'   # filters dataset to obtain information
    #'   datasets$get_data_info("ADSL", filtered = TRUE)
    #'   datasets$get_filter_info("ADSL")
    #'   df <- datasets$get_data("ADSL", filtered = FALSE)
    #'   # df
    #'
    #'   datasets$get_filter_type("ADSL", "SEX")
    #'   datasets$set_filter_state("ADSL", state = list(
    #'     AGE = list(range = c(33, 44), keep_na = FALSE),
    #'     SEX = list(choices = c("M", "F"), keep_na = FALSE)
    #'   ))
    #'
    #'   infos <- datasets$get_filter_info("ADSL") # will return a named list of all active filters
    #'   names(infos) # notice that names correspond to variable names
    #'
    #'   single_info <- datasets$get_filter_info("ADSL", "AGE") # will return a list of AGE filter infos
    #'   names(single_info) # note that this contains filter informations, not variable names as names
    #'
    #'   single_info2 <- datasets$get_filter_info("ADSL", include_unknown = TRUE)
    #'   single_info2 # will include active filters of type unknown
    #'
    #'   single_info3 <- datasets$get_filter_info("ADSL",
    #'     varname = c("AGE", "SEX"),
    #'     include_unknown = FALSE)
    #'   single_info3 # make note that include_unknown will be ignored, when varname is not NULL
    #' })
    #'
    get_filter_info = function(dataname,
                               varname = NULL,
                               include_unknown = TRUE) {
      lapply(varname, function(x) private$check_data_varname_exists(dataname, x))
      stopifnot(is_logical_single(include_unknown))

      infos <- private$filter_infos[[dataname]]()

      if (is.null(varname)) {
        active_filter_names <- names(self$get_filter_state(dataname, include_unknown = include_unknown))
        infos <- infos[vapply(names(infos),
          function(x) x %in% active_filter_names,
          FUN.VALUE = logical(1),
          USE.NAMES = FALSE)]
        infos <- infos[active_filter_names]  # needed, because otherwise the order does not match
      } else if (length(varname) == 1) {
        infos <- infos[[varname]]
      } else {
        infos <- infos[varname]
      }

      infos
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
        function(x) self$get_data_info(x, filtered = TRUE),
        USE.NAMES = TRUE,
        simplify = FALSE
      )
      data_info_nfiltered <- sapply(
        datanames,
        function(x) self$get_data_info(x, filtered = FALSE),
        USE.NAMES = TRUE,
        simplify = FALSE
      )

      res_list <- Map(
        function(x, y) {
          setNames(paste0(x, "/", y), names(x))
        },
        x = data_info_filtered,
        y = data_info_nfiltered
      )

      res_df <- as.data.frame(do.call(rbind, res_list))
      res_df[, "Dataset"] <- rownames(res_df)
      rownames(res_df) <- NULL
      res_df <- res_df[, c("Dataset", setdiff(names(res_df), "Dataset"))]

      return(res_df)
    },

    #' @description
    #' See `get_filter_info`, only returns filter type (`choices`, `range`, `logical`).
    #'
    #' @param dataname (`character`) name of the dataset
    #' @param varname (`character`) column within the dataset,
    #'   must be provided
    #' @return (`character`) filter type
    get_filter_type = function(dataname, varname) {
      stopifnot(is_character_single(varname)) # check that varname is non-NULL
      return(self$get_filter_info(dataname, varname)[["type"]])
    },

    #' @description
    #' Returns active filter state for a dataset (or only a variable within it).
    #'
    #' @param dataname (`character`) name of the dataset
    #' @param varname (`character`) column within the dataset;
    #'   if `NULL`, return all variables in a list
    #' @param include_unknown (`logical`) whether to include the "unknown" type of variables
    #'   (TRUE) or not to include them (FALSE). Default: TRUE
    #' @return (`character` vector) filter state or list of these
    get_filter_state = function(dataname, varname = NULL, include_unknown = TRUE) {
      private$check_data_varname_exists(dataname, varname)
      stopifnot(is_logical_single(include_unknown))

      filter_states <- private$filter_states[[dataname]]

      if (!is.null(varname)) {
        return(filter_states[[varname]])
      }

      # filter out unknowns
      if (!include_unknown) {
        filter_states <- filter_states[vapply(
          filter_states,
          function(x) !is.null(x[[1]]),  # unknown filters have NULL as the first element
          FUN.VALUE = logical(1)
        )]
      }

      filter_states
    },

    #' @description
    #' Set the filter state for a dataname
    #'
    #' The state is only updated and triggers reactive behavior when it actually
    #' changes.
    #'
    #' To set a state for a single variable, you can write:
    #' `set_filter_state(dataname, setNames(list(single_state), varname))`
    #' or use the wrapper function `set_single_filter_state`.
    #' To remove a filter for a variable, pass in `NULL`, e.g.
    #' `state = list(SEX = NULL, ..)`.
    #' To remove all filters for a dataset, call
    #' `set_filter_state(dataname, state = list(), remove_omitted = TRUE)`.
    #'
    #' @param dataname (`character`) name of the dataset to set filter state for
    #' @param state (named `list`) of new state to set; for each `varname`,
    #'   `state[[varname]]` is the new state to set for `varname`
    #' @param remove_omitted (`logical`) whether to remove the filters for the
    #'   variables that are not present in the named list `state`
    #'
    #' @return (`logical`) if the state for the `dataname` was changed
    set_filter_state = function(dataname, state, remove_omitted = FALSE) {
      private$check_data_varname_exists(dataname)

      if (is.null(state)) {
        state <- list()
      }
      stopifnot(
        is_fully_named_list(state),
        is_logical_single(remove_omitted)
      )

      # check columns given in state are in dataset
      check_in_subset(
        names(state),
        self$get_filterable_varnames(dataname),
        pre_msg = paste0("data ", dataname, ": ")
      )

      internal_state <- Map(
        function(varname, var_state) {
          private$external_to_internal_state(dataname, varname, var_state)
        },
        varname = names(state),
        var_state = state
      )

      for (varname in names(internal_state)) {
        if (is.null(private$filter_states[[dataname]][[varname]])) {
          private$check_valid_filter_state(
            dataname = dataname,
            varname = varname,
            var_state = internal_state[[varname]]
          )
        }
      }

      new_state <- if (remove_omitted) {
        # these will be effectively removed as the entire state for the dataname is set to this new one
        internal_state
      } else {
        tmp_state <- self$get_filter_state(dataname)
        # overwrite those entries that were provided
        tmp_state[names(internal_state)] <- internal_state
        tmp_state
      }
      # filter out `NULL` entries as `NULL` means to not filter the state and the rest
      # of the code assumes `NULL` elements are not present in the list (simply removed)
      new_state <- Filter(Negate(is.null), new_state)

      # for this to work reliably, the previous state must really capture all info
      # i.e. NA filtering or not
      # identical returns TRUE if all equal, otherwise FALSE
      if (identical(private$filter_states[[dataname]], new_state)) {
        return(FALSE)
      }

      private$filter_states[[dataname]] <- new_state

      return(TRUE)
    },

    #' @description
    #' Get the default filter state (useful for initial UI state)
    #' This is different to NULL state which means no filter applied.
    #' Instead, this filter is a sensible default, e.g. filter out NAs.
    #' for numerics, full range; for factors, all choices etc.
    #'
    #' @param dataname (`character`) name of the dataset
    #' @param varname (`character`) column within the dataset;
    #'   must be provided
    #' @return (`character` vector) default filter state for this variable
    get_default_filter_state = function(dataname, varname) {
      stopifnot(is_character_single(varname))
      private$check_data_varname_exists(dataname, varname)

      filter_info <- self$get_filter_info(dataname, varname)
      state <- switch(
        filter_info$type,
        # the list might a named list of choices so Shiny displays it nicely in the UI
        # here, we unname it and convert it to a vector because we want the internal state
        choices = list(choices = unlist(unname(filter_info$choices))),
        range = list(range = unlist(unname(filter_info$range))),
        logical = list(status = "TRUE"),
        date = list(daterange = unlist(unname(filter_info$daterange))),
        unknown = list(NULL),
        stop(paste0(varname, " is of unrecognized type: ", filter_info$class))
      )
      return(c(state, list(keep_na = FALSE, keep_inf = FALSE)))
    },

    #' @description
    #' Get an expression to filter the dataset according to the filter state
    #'
    #' Helper function for `reactive_filtered_dataset`. The latter actually
    #' makes sure the dependent datasets are available and then evaluates the
    #' dataset.
    #'
    #' It returns an expression to filter the dataset only, assuming the
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
    #' @return (`expression`) which returns the filtered dataset
    get_filter_expr = function(dataname) {
      private$check_data_varname_exists(dataname)

      filtered_dataname <- private$filtered_dataname(dataname)

      bquote({
        .(as.call(list(
          as.name("<-"),
          as.name(filtered_dataname),
          private$get_pure_filter_call(dataname)
        )))
      })
    },

    # info functions for end user ----

    #' @description
    #' Print method
    #'
    #' To obtain the default printing of R6 objects with method listing,
    #' use `cat(format(x))`.
    #'
    #' @param ... unused arguments
    print = function(...) {

      # to filter all datasets before the print message as they are only lazily evaluated
      lapply(self$datanames(), function(x) self$get_data(x, filtered = TRUE))

      cat(class(self), "object", "\n")

      cat("- Introspect this object (x) with the following methods:", "\n")
      cat("   - cat(format(x))", "\n")

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

        cat(paste0('   - x$print_filter_info("', datanames[[1]], '")', "\n")) #nolint
        cat(paste0('   - x$get_data_info("', datanames[[1]], '", filtered = FALSE)', "\n")) #nolint
      }

      return(invisible(NULL))
    },

    #' @description
    #' Prints filter characteristics of each variable for a given dataset to the
    #' console.
    #'
    #' @param dataname (`character`) name of the dataset
    #' @param filtered_vars_only (`logical`) whether to only consider filtered
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
      varnames <- if (filtered_vars_only) {
        names(self$get_filter_state(dataname))
      } else {
        names(df)
      }
      if (!is.null(variables)) {
        varnames <- intersect(varnames, variables)
      }

      var_maxlength <- max(c(0, nchar(varnames)))
      for (varname in varnames) {
        var_info <- self$get_filter_info(dataname, varname = varname)
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
      log2("===========================")
      return(invisible(NULL))
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

      c(
        Rows = nrow(self$get_data(dataname, filtered = filtered))
      )
    },

    # Functions useful for restoring from another dataset ----

    #' @description
    #' Returns the state to be bookmarked
    #'
    #' hash sums of `datasets`, `filter_states` and `preproc_code` are bookmarked.
    #'
    #' @return named list
    get_bookmark_state = function() {
      # We cannot simply create a new `FilteredData` with the datasets set to NULL
      # (to protect the data from unauthorized access). Therefore, we return a list
      # which is independent of this class. It is also better to store a list
      # rather than a class because this allows bookmarking of type `url` and
      # not just `server`
      res <- list(
        # must be a list and not atomic vector, otherwise jsonlite::toJSON gives a warning
        data_hash = setNames(
          lapply(self$datanames(), self$get_data_attr, "hash"),
          self$datanames()
        ),
        filter_states = reactiveValuesToList(private$filter_states),
        code = self$get_code()
      )
      return(res)
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
    #' @param state (`list`) containing fields `data_hash`, `filter_states`
    #'   and `preproc_code`.
    #' @param check_data_hash (`logical`) whether to check that `md5sums` agree
    #'   for the data; may not make sense with randomly generated data per session
    restore_state_from_bookmark = function(state, check_data_hash = TRUE) {
      stopifnot(
        is.list(state),
        is_logical_single(check_data_hash)
      )
      check_setequal(
        names(state$data_hash), self$datanames(),
        pre_msg = "The names of the stored hash sums and the datanames don't agree:"
      )
      check_setequal(
        names(state$filter_states), self$datanames(),
        pre_msg = "The names of the stored filters and the datanames don't agree:"
      )
      if (check_data_hash) {
        datasets_equal <- vapply(self$datanames(), self$get_data_attr, character(1), "hash") == state$hash
        if (!all(datasets_equal)) {
          stop(
            "The following datasets are not identical and probably have changed since bookmarking: ",
            toString(names(datasets_equal))
          )
        }
      }
      if (!is.null(state$code)) {
        # currently, code serializes nicely, so bookmarking by URL still works
        # check they are actually equal (compare two lists with one element each)
        check_setequal(
          list(self$get_code()), list(state$code),
          pre_msg = "The preprocessing codes don't agree:"
        )
      }

      # we have to be careful with `reactiveValues` to restore each item and not simply
      # reference the old reactive value, i.e. loop over it
      lapply(self$datanames(), function(dataname) {
        self$set_filter_state(dataname, state = state$filter_states[[dataname]], remove_omitted = TRUE)
      })
      # reactive endpoints don't need to be set because they are computed through reactivity

      return(invisible(NULL))
    }
  ),

  ## __Private Methods---------------------
  private = list(

    # private attributes ----

    # reactiveValues attributes (reactive sources)
    # `reactiveValues` with one entry per dataname

    # unfiltered / raw datasets
    # e.g. for two datasets `ADSL` and `ADAE` we would have
    # nolint start
    # datasets <- reactiveValues(
    #   ADSL = <object that inherits from data.frame>,
    #   ADAE =  <object that inherits from data.frame>
    # )
    # nolint end
    # the `data.frames` may have attributes like keys
    unfiltered_datasets = NULL, # reactiveValues() after init
    # filter state to apply to obtain the filtered dataset from the unfiltered one
    # filter out `NULL` entries as `NULL` means to not filter the state and the rest
    # of the code assumes `NULL` elements are not present in the list (simply removed)
    filter_states = NULL, # reactiveValues() after init

    # reactive attributes (conductors)
    # non-reactive list of `reactives`
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
    # Instead, we create them once for all when the data is set in `set_data`.

    # list of reactive expressions to obtain filtered datasets
    filtered_datasets = list(),
    # list of filter characteristics to create filter that has no effect (i.e. full range of variable),
    # useful for UI to show sensible ranges and initial state
    filter_infos = list(),

    # non-reactive attributes

    # preprocessing code used to generate the unfiltered datasets as a string
    code = CodeClass$new(),

    # for each dataname: contains keys and optionally `data_label`, `column_labels`
    data_attrs = list(),

    join_keys = NULL,

    # check functions ----
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
      stopifnot(
        # check classes
        is.reactivevalues(private$unfiltered_datasets),
        is.reactivevalues(private$filter_states),
        # no `reactiveValues`, but a list of `reactiveVal`
        all(vapply(private$filtered_datasets, is.reactive, logical(1))),
        all(vapply(private$filter_infos, is.reactive, logical(1))),

        # check names are the same
        has_same_names(private$unfiltered_datasets, private$filter_states),
        has_same_names(private$unfiltered_datasets, private$filtered_datasets),
        has_same_names(private$unfiltered_datasets, private$filter_infos),
        has_same_names(private$unfiltered_datasets, private$data_attrs),

        # check attributes are there: hash
        all_true(self$datanames(), function(dataname) !is.null(private$data_attrs[[dataname]][["hash"]])),
      )

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
              private$check_valid_filter_state(
                dataname,
                varname = varname,
                var_state = var_state
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
        if (!(dataname %in% names(private$unfiltered_datasets))) {
          # data must be set already
          stop(paste("data", dataname, "is not available"))
        }
        if (!is.null(varname) && !(varname %in% self$get_varnames(dataname))) {
          stop(paste("variable", varname, "is not in data", dataname))
        }
      })

      return(invisible(NULL))
    },

    # @description
    # Checks that the given filter state is valid for the specified
    # variable in the specified dataset.
    #
    # This function is typically called before setting a new filter state
    # to validate it.
    #
    # It stops when there is an error.
    #
    # @param dataname (`character`) name of the dataset
    # @param varname (`character`) column within the dataset;
    #   must be provided
    # @param varstate (`list`) variable state
    check_valid_filter_state = function(dataname, varname, var_state) {
      stopifnot(is_character_single(varname)) # must be non-NULL
      private$check_data_varname_exists(dataname, varname)

      var_info <- self$get_filter_info(dataname, varname) # filter characteristics for var, e.g. range

      if (is.null(var_state)) {
        # when state is `NULL`, this means no filtering is applied
        # although it cannot be `NULL` internally (it must simply be removed from the list instead
        # of being `NULL`), we filter out `NULL`s only once we set the state
        return()
      }

      if (!is_logical_single(var_state$keep_na)) {
        stop(
          "data", dataname, "variable", varname, ":",
          "NA selection must be one of TRUE, FALSE, but is ", var_state$keep_na
        )
      }

      if (!is_logical_single(var_state$keep_inf)) {
        stop(
          "data", dataname, "variable", varname, ":",
          "Inf selection must be one of TRUE, FALSE, but is ", var_state$keep_Inf
        )
      }

      pre_msg <- paste0("data ", dataname, ", variable ", varname, ": ")
      switch(
        var_info$type,
        choices = {
          selection_state <- var_state$choices
          check_in_subset(selection_state, var_info$choices, pre_msg = pre_msg)
        },
        range = {
          check_in_range(var_state$range, var_info$range, pre_msg = pre_msg)
        },
        logical = {
          selection_state <- var_state$status
          # the conceptual difference to type 'choices' is that it allows exactly one value rather than a subset
          stopifnot(length(selection_state) == 1)
          check_in_subset(selection_state, var_info$choices, pre_msg = pre_msg)
        },
        date = {
          check_in_range(var_state$daterange, var_info$daterange, pre_msg = pre_msg)
        },
        unknown = {
          stopifnot(is.null(var_state[[1]]))
        },
        stop(paste("Unknown filter type", var_info$type, "for data", dataname, "and variable", varname))
      )
      return(invisible(NULL))
    },

    # @description
    # Creates a call that filters the dataset to obtain the filtered dataset.
    # This method is called by `get_filter_expr`. The latter may additionally perform
    # join operations, e.g. an inner-join with `ADSL` for `CDISC` data.
    #
    # @param dataname (`character`) name of the dataset
    # @return (`call`) to obtain the filtered dataset from the unfiltered one
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
            date = {
              # format daterange to human readable string for R code
              if (filter_info$is_datetime) {
                attributes(filter_state$daterange)$tzone <- Sys.timezone()
                # use maximum precision of 6 decimal places for POSIXct
                selection_state <- format(filter_state$daterange, "%Y-%m-%d %H:%M:%OS6")
                # timezone in server may be different from browser, so user is
                # informed which timezone is being used (could use timezone from
                # browser with filter_state$timezone but using Sys.timezone here)
                filter_state$daterange <- format(filter_state$daterange, tz = Sys.timezone())
                timezone <- Sys.timezone()

                call(
                  "&",
                  call(">=", as.name(varname), call("as.POSIXct", selection_state[1], tz = timezone)),
                  call("<=", as.name(varname), call("as.POSIXct", selection_state[2], tz = timezone))
                  )
              } else {
                selection_state <- format(filter_state$daterange, "%Y-%m-%d")
                call(
                  "&",
                  call(">=", as.name(varname), call("as.Date", selection_state[1])),
                  call("<=", as.name(varname), call("as.Date", selection_state[2]))
                )
              }
            },
            unknown = {
              NULL
            },
            stop(paste("filter type for variable", varname, "in", dataname, "not known"))
          )

          if (type == "unknown") {
            return(filter_call)
          } else {
            if (type == "range" && isTRUE(filter_state$keep_inf)) {
              filter_call <- call("|", call("is.infinite", as.name(varname)), filter_call)
            }

            # allow NA as well, i.e. do not filter it out
            if (isTRUE(filter_state$keep_na)) {
              # we add `is.na` independent of whether the variable has na values or not
              # dplyr's filter allows for: c(NA, "F", "M") == "F" - which normally throws NA, T, F
              filter_call <- call("|", call("is.na", as.name(varname)), filter_call)
            }

            return(filter_call)
          }

        },
        self$get_filter_info(dataname, include_unknown = FALSE),
        self$get_filter_state(dataname, include_unknown = FALSE),
        names(self$get_filter_info(dataname, include_unknown = FALSE))
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

    # return name of filtered dataset in generated code expressions
    filtered_dataname = function(dataname) {
      private$check_data_varname_exists(dataname)
      paste0(dataname, "_FILTERED")
    },

    # @description
    # Reactive to compute the filtered dataset
    # This returns a reactive that computes the filtered dataset.
    # Through reactivity, it is only lazily evaluated and also re-evaluates
    # whenever the unfiltered datasets it depends on change.
    #
    # @param dataname (`character`) name of the dataset
    # @return (`reactive`) expression for the provided `dataname` that returns
    #   the filtered dataset when evaluated
    reactive_filtered_dataset = function(dataname) {
      private$check_data_varname_exists(dataname)

      reactive({
        .log("################################################# Refiltering dataset ", dataname)

        # We could use a chunks object here, but don't do so for simplicity as we don't need its capabilities.
        # We take the parent of the global env, i.e. the last attached package, to avoid picking up
        # any variables defined in the global environment. This is okay as it takes whatever is the current
        # `globalenv` when this reactive is evaluated, i.e. when the relevant packages are already loaded.
        env <- new.env(parent = parent.env(globalenv()))

        env[[dataname]] <- self$get_data(dataname, filtered = FALSE)

        eval(self$get_filter_expr(dataname), envir = env)
        return(env[[private$filtered_dataname(dataname)]])
      })
    },

    # @description
    # Reactive to compute the filter infos
    # Reactive to compute the filter infos for a `dataname`
    #
    # For each variable in the dataset, it computes the filter info.
    # This filter info depends on the variable type (`numeric`, `factor`, `logical`)
    # and returns the least restrictive filter state, i.e.
    # - `factor or character`: `type: "choices", choices: , histogram_data: `
    # - `numeric`: `type: "range", range: , histogram_data: `
    # - `logical`: `type: "logical", choices: , histogram_data: `
    # - all `NA` entries: `type: unknown, class: , all_na = TRUE`, cannot be filtered
    # - default: `type: unknown, class: `, cannot be filtered
    # Each variable's filter info is a list with entries `type`, `labels` and others
    # that depend on the variable type as outlined above.
    #
    # @param dataname (`character`) name of the dataset
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
              label = if_null(attr(var, "label"), varname),
              class = class(var),
              all_na = TRUE
            )
          } else if (is.factor(var) ||
                     is.character(var) ||
                     (is.numeric(var) && length(unique(var[!is.na(var)])) < .threshold_slider_vs_checkboxgroup)) {

            add_counts <- TRUE

            label_var <- if_null(attr(var, "label"), "")

            if (!is(var, "factor")) {
              var <- factor(var, levels = as.character(sort(unique(var))))
            } else if (is.character(var)) {
              add_counts <- FALSE
            }
            var <- droplevels(var)

            choices <- levels(var)
            if (add_counts) {
              names(choices) <- paste0(choices, " (", tabulate(var), ")")
            }
            histogram_data <- data.frame(x = levels(var), y = tabulate(var))

            list(
              type = "choices",
              label = label_var,
              choices = as.list(choices), # convert named vector to named list as Shiny `toJSON` otherwise complains
              histogram_data = histogram_data
            )
          } else if (is.numeric(var)) {
            num_vals <- sum(is.finite(var))
            density <- if (num_vals >= 2) {
              stats::density(var, na.rm = TRUE, n = 100) # 100 bins only
            } else {
              data.frame(x = NA_real_, y = NA_real_)
            }
            list(
              type = "range",
              label = if_null(attr(var, "label"), ""),
              range = range(var, finite = TRUE),
              histogram_data = data.frame(x = density$x, y = density$y),
              inf_count = sum(is.infinite(var)),
              is_integer = is.integer(var)
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
          } else if (inherits(var, "Date") || inherits(var, "POSIXct") || inherits(var, "POSIXlt")) {
            list(
              type = "date",
              label = if_null(attr(var, "label"), ""),
              daterange = range(var, finite = TRUE),
              is_datetime = inherits(var, "POSIXct") || inherits(var, "POSIXlt"),
              timezone = NULL
            )
          } else {
            .log(
              "variable '", varname, "' is of class '",
              class(var), "' which currently does not support filtering",
              sep = ""
            )
            list(
              type = "unknown",
              label = if_null(attr(var, "label"), varname),
              class = class(var)
            )
          }
          res$na_count <- sum(is.na(var))
          return(res)
        }, df, names(df))

        return(setNames(filter_info, names(df)))
      })
    },
    # @description
    # external to internal filter state representation ----
    # Convert the external to an internal filter state
    #
    # It also converts `default_filter` to the actual state,
    # replaces `NA` in the vector by the `keep_na` option.
    #
    # If the input is a list and does not already have names, `keep_NA` is set
    # to whether the list contains `NA` or not and the choices are set
    # by removing all NA elements.
    #
    # This is done to simplify the filter specification for the end-user
    # in `init()`.
    #
    # @param dataname (`character`) name of the dataset
    # @param varname (`character`) column within the dataset,
    #   must be provided
    # @param var_state (`list`) state of single variable
    # @return internal filter state: `list(spec = .., keep_na = .., keep_inf = ..)`,
    #   where `spec` can be `choices`, `range` or similar
    # @examples # nolint start
    # var_state = list(choices = c("M", "F"))
    # var_state = list(choices = c("M", "F"), keep_na = TRUE)
    # var_state = c("M", "F")
    # var_state = c("M", "F", NA)
    # var_state = default_filter()
    # var_state = NULL # no filter applied
    # # all these return slight variations of: list(choices = c("M", "F"), keep_na = TRUE)
    # nolint end
    external_to_internal_state = function(dataname, varname, var_state) {
      private$check_data_varname_exists(dataname, varname)

      if (is.null(var_state)) {
        return(NULL)
      }

      if (is_default_filter(var_state)) {
        var_state <- self$get_default_filter_state(dataname, varname)
      } else if (is.null(names(var_state))) {
        # then, we assume that var_state is a unnamed vector
        # need to assume choice type (choice, range, ...), keep_na and keep_inf
        var_state <- setNames(
          # is.na is vectorized
          object = list(
            Filter(
              function(x) {
                all(!is.na(x))
              },
              var_state
            ),
            any(is.na(var_state)),
            any(is.infinite(var_state))
          ),
          nm = c(self$get_filter_type(dataname, varname), "keep_na", "keep_inf")
        )
      }
      # if `keep_na/inf` is not provided, default to `FALSE`
      var_state$keep_na <- if_null(var_state$keep_na, FALSE)
      var_state$keep_inf <- if_null(var_state$keep_inf, FALSE)
      var_state
    }
  )
)

# default filter state ----

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

is_default_filter <- function(x) is(x, "default_filter")

#' @export
print.default_filter <- function(x, ...) {
  cat("This will pick the default filter state for the variable.\n")
}

# Wrapper functions for `FilteredData` class ----

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
get_filter_expr <- function(datasets, datanames = datasets$get_datanames()) {
  stopifnot(
    is(datasets, "FilteredData"),
    is_character_vector(datanames)
  )

  # reorder datanames
  datanames <- intersect(datasets$datanames(), datanames)

  # remove first `{}` around expression, then concatenate all lines of all expression and convert to call
  # see https://stackoverflow.com/questions/9744729/how-can-i-concatenate-compound-language-objects-in-r
  concat_exprs <- function(exprs) {
    as.call(c(as.symbol("{"), do.call(c, lapply(exprs, function(expr) as.list(expr)[-1]))))
  }

  concat_exprs(lapply(datanames, datasets$get_filter_expr))
}


#' Get filter variable names that are currently active
#'
#' Only works in a reactive context.
#'
#' @param datasets (`FilteredData`)
#' @param dataname (`character`) dataname to get filter variables for
#' @param include_unknown (`logical`) whether to include unknown type variables. Default: TRUE
#'
#' @return `character` vector of active filter variables
get_filter_vars <- function(datasets, dataname, include_unknown = TRUE) {
  stopifnot(
    is(datasets, "FilteredData"),
    is_logical_single(include_unknown))
  names(datasets$get_filter_state(dataname, include_unknown = include_unknown))
}

#' Set the filter state for a single variable
#'
#' See `FilteredData$set_filter_state`.
#'
#' @param datasets (`FilteredData`)
#' @param dataname (`character`) dataname
#' @param varname (`character`) variable name
#' @param state (`list`) new state for the variable, can be
#'   `default_filter()`
set_single_filter_state <- function(datasets, dataname, varname, state) {
  stopifnot(is_character_single(varname))
  datasets$set_filter_state(dataname, state = setNames(list(state), varname))
}

#' Print the state in a nice format
#'
#' The `keep_na` is not printed.
#'
#' @param var_type (`character`) variable type
#' @param state (`list`) with elements corresponding to variable
#'   type; can be `filter_state` or `filter_info`
#' @param add_na (`logical`) whether to display information about NA,
#'   either `keep_na` (for `filter_state`) or `na_count` (for `filter_info`)
filter_state_to_str <- function(var_type, state, add_na = FALSE) {
  stopifnot(
    is_character_single(var_type),
    is_logical_single(add_na)
  )

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
    date = paste(state$daterange, collapse = " - "),
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
