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
#' The datasets are filtered lazily, i.e. only when requested / needed in a Shiny app.
#'
#' By design, any dataname set through `set_data` cannot be removed because
#' other code may already depend on it. As a workaround, the underlying
#' data can be set to `NULL`.
#'
#' The class currently supports variables of the following types within datasets:
#' - `choices`: variable of type `factor`, e.g. `ADSL$COUNTRY`, `iris$Species`
#'      zero or more options can be selected, when the variable is a factor
#' - `logical`: variable of type `logical`, e.g. `ADSL$TRT_FLAG`
#'      exactly one option must be selected, `TRUE` or `FALSE`
#' - `ranges`: variable of type `numeric`, e.g. `ADSL$AGE`, `iris$Sepal.Length`
#'      numerical range, a range within this range can be selected
#' - `dates`: variable of type `Date`, `POSIXlt`
#' Other variables cannot be used for filtering the data in this class.
#'
#' Common arguments are:
#' 1. `filtered`: whether to return a filtered result or not
#' 2. `dataname`: the name of one of the datasets in this FilteredData
#' 3. `varname`: one of the columns in a dataset
#'
#' @examples
#' library(shiny)
#' datasets <- teal:::FilteredData$new()
#'
#' # setting the data
#' datasets$set_dataset(dataset("iris", iris))
#' datasets$set_dataset(dataset("mtcars", mtcars))
#'
#' isolate({
#'   datasets$datanames()
#'   datasets$get_filter_overview("iris")
#'
#'   # filters dataset to obtain information
#'   datasets$get_filter_overview("mtcars")
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
#'   extract_type = "list"
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
#'   extract_type = "list"
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
      invisible(self)
    },
    #' @description
    #' Gets datanames
    #'
    #' The datanames are returned in the order in which they must be
    #' evaluated (in case of dependencies).
    #' @return (`character` vector) of datanames
    datanames = function() {
      names(private$filtered_datasets)
    },


    #' Gets data label for the dataset
    #'
    #' Useful to display in `Show R Code`.
    #'
    #' @param dataname (`character`) name of the dataset
    #' @return (`character`) keys of dataset
    get_datalabel = function(dataname) {
      self$get_data_attr(dataname, "dataset_label")
    },

    #' @description
    #' Gets dataset names of a given dataname for the filtering.
    #'
    #' @param dataname (`character` vector) names of the dataset
    #' @return (`character` vector) of dataset names
    get_filterable_datanames = function(dataname) {
      dataname
    },
    #' @description
    #' Gets variable names of a given dataname for the filtering.
    #'
    #' @param dataname (`character`) name of the dataset
    #' @return (`character` vector) of variable names
    get_filterable_varnames = function(dataname) {
      self$get_varnames(dataname)
    },

    # datasets methods ----
    #' @description
    #' Gets a `call` to filter the dataset according to the filter state
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
    #' Gets the R preprocessing code string that generates the unfiltered datasets
    #' @param dataname (`character`) name(s) of dataset(s)
    #' @return (`character`) deparsed code
    get_code = function(dataname = self$datanames()) {
      paste0(private$code$get_code(dataname), collapse = "\n")
    },

    #' @description
    #' Gets `FilteredDataset` object which contains all informations
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
    #' Gets filtered or unfiltered dataset
    #'
    #' For `filtered = FALSE`, the original data set with
    #' `set_data` is returned including all attributes.
    #'
    #' @param dataname (`character`) name of the dataset
    #' @param filtered (`logical`) whether to return a filtered or unfiltered dataset
    get_data = function(dataname, filtered = TRUE) {
      private$check_data_varname_exists(dataname)
      stopifnot(is_logical_single(filtered))

      self$get_filtered_datasets(dataname)$get_data(filtered = filtered)
    },

    #' @description
    #' Gets data attributes for a given dataset
    #'
    #' Sets and gets the data attribute on unfiltered data as it is never modified
    #' as attributes.
    #'
    #' @param dataname (`character`) name of the dataset
    #' @param attr (`character`) attribute to get from the data attributes of the dataset
    #' @return value of attribute, may be `NULL` if it does not exist
    get_data_attr = function(dataname, attr) {
      private$check_data_varname_exists(dataname)
      stopifnot(is_character_single(attr))
      get_attrs(self$get_filtered_datasets(dataname)$get_dataset())[[attr]]
    },

    #' @description
    #' Get join keys between two datasets.
    #' @param dataset_1 (`character`) one dataset name
    #' @param dataset_2 (`character`) other dataset name
    #' @return (`named character`) vector with column names
    get_join_keys = function(dataset_1, dataset_2) {
      res <- if (!missing(dataset_1) && !missing(dataset_2)) {
        self$get_filtered_datasets(dataset_1)$get_join_keys()[[dataset_2]]
      } else if (!missing(dataset_1)) {
        self$get_filtered_datasets(dataset_1)$get_join_keys()
      } else if (!missing(dataset_2)) {
        self$get_filtered_datasets(dataset_2)$get_join_keys()
      } else {
        res_list <- lapply(
          self$datanames(), function(dat_name) {
            self$get_filtered_datasets(dat_name)$get_join_keys()
          }
        )
        names(res_list) <- self$datanames()
        res_list
      }
      if (is_empty(res)) {
        return(character(0))
      }

      return(res)
    },

    #' @description
    #' Get filter overview table in form of X (filtered) / Y (non-filtered)
    #'
    #' This is intended to be presented in the application.
    #' The content for each of the data names is defined in `get_filter_overview_info` method.
    #'
    #' @param datanames (`character` vector) names of the dataset
    #'
    #' @return (`matrix`) matrix of observations and subjects of all datasets
    get_filter_overview = function(datanames) {
      if (identical(datanames, "all")) {
        datanames <- self$datanames()
      }
      check_in_subset(datanames, self$datanames(), "Some datasets are not available: ")

      rows <- lapply(
        datanames,
        function(dataname) {
          self$get_filtered_datasets(dataname)$get_filter_overview_info()
        }
      )

      do.call(rbind, rows)
    },

    #' Get keys for the dataset
    #' @param dataname (`character`) name of the dataset
    #' @return (`character`) keys of dataset
    get_keys = function(dataname) {
      self$get_filtered_datasets(dataname)$get_keys()
    },
    #' @description
    #' Gets labels of variables in the data
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
    #' Gets variable names
    #'
    #' @param dataname (`character`) the name of the dataset
    #' @return (`character` vector) of variable names
    get_varnames = function(dataname) {
      self$get_filtered_datasets(dataname)$get_varnames()
    },

    #' When active_datanames is "all", sets them to all datanames
    #' otherwise, it makes sure that it is a subset of the available datanames
    #'
    #' @param datanames `character vector` datanames to pick
    #'
    #' @return the intersection of `self$datanames()` and `datanames`
    handle_active_datanames = function(datanames) {
      if (identical(datanames, "all")) {
        datanames <- self$datanames()
      } else {
        for (dataname in datanames) {
          tryCatch(
            private$check_data_varname_exists(dataname = dataname),
            error = function(e) {
              message(e$message)
            }
          )
        }
      }
      datanames <- self$get_filterable_datanames(datanames)
      intersect(self$datanames(), datanames)
    },


    #' @description
    #' Adds a `Dataset` object to this FilteredData
    #'
    #' Adds a dataset and preserve all attributes attached to this object.
    #' Technically `set_dataset` created `FilteredDataset` which keeps
    #' `dataset` for filtering purpose.
    #'
    #' @param dataset (`Dataset` or `DatasetConnector`)\cr
    #'   the object containing data and attributes.
    #' @return (`self`) invisibly this FilteredData
    set_dataset = function(dataset) {
      stopifnot(is(dataset, "Dataset") || is(dataset, "DatasetConnector"))
      dataname <- get_dataname(dataset)
      # to include it nicely in the Show R Code; the UI also uses datanames in ids, so no whitespaces allowed
      check_simple_name(dataname)
      private$filtered_datasets[[dataname]] <- init_filtered_dataset(
        get_dataset(dataset)
      )

      invisible(self)
    },

    #' @description
    #' Sets the R preprocessing code for single dataset
    #'
    #' @param code `CodeClass` preprocessing code that can be parsed to generate the
    #'   unfiltered datasets
    #' @return (`self`)
    set_code = function(code) {
      stopifnot(inherits(code, "CodeClass"))
      private$code <- code
      invisible(self)
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
      stop("Pure virtual method.")
    },

    #' @description
    #' Sets bookmark state
    #' @param state (`named list`)\cr
    #'  nested list of filter selections applied to datasets.
    #' @return invisibly NULL
    set_bookmark_state = function(state) {
      stopifnot(
        all(names(state) %in% self$datanames())
      )
      for(dataname in names(state)) {
        fd <- self$get_filtered_datasets(dataname = dataname)
        fd$set_bookmark_state(state = state[[dataname]])
      }

      invisible(NULL)
    },

    #' @description
    #' Sets this object from a bookmarked state
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
      stop("Pure virtual method")
    },

    # shiny modules -----

    #' Module for the right filter panel in the teal app
    #' with a filter overview panel and a filter variable panel.
    #'
    #' This panel contains info about the number of observations left in
    #' the (active) datasets and allows to filter the datasets.
    #'
    #' @param id (`character(1)`)\cr
    #'   module id
    ui_filter_panel = function(id) {
      ns <- NS(id)
      div(
        id = ns("filter_panel_whole"), # used for hiding / showing
        div(
          id = ns("filters_overview"), # not used, can be used to customize CSS behavior
          class = "well",
          tags$div(
            class = "row",
            tags$div(
              class = "col-sm-9",
              tags$label("Active Filter Summary", class = "text-primary", style = "margin-bottom: 15px;")
            ),
            tags$div(
              class = "col-sm-3",
              tags$a(
                href = "javascript:void(0)",
                class = "remove pull-right",
                onclick = sprintf(
                  "$('#%s').toggle();",
                  ns("filters_overview_contents")
                ),
                title = "minimise panel",
                tags$span(icon("minus-circle", lib = "font-awesome"))
              )
            )
          ),
          tags$br(),
          div(
            id = ns("filters_overview_contents"),
            self$ui_filter_overview(ns("teal_filters_info"))
          )
        ),

        div(
          id = ns("filter_active_vars"), # not used, can be used to customize CSS behavior
          class = "well",
          tags$div(
            class = "row",
            tags$div(
              class = "col-sm-6",
              tags$label("Active Filter Variables", class = "text-primary", style = "margin-bottom: 15px;")
            ),
            tags$div(
              class = "col-sm-6",
              actionLink(
                ns("remove_all_filters"),
                "",
                icon("times-circle", lib = "font-awesome"),
                title = "remove active filters",
                class = "remove_all pull-right"
              ),
              tags$a(
                href = "javascript:void(0)",
                class = "remove pull-right",
                onclick = sprintf(
                  "$('#%s').toggle();",
                  ns("filter_active_vars_contents")
                ),
                title = "minimise panel",
                tags$span(icon("minus-circle", lib = "font-awesome"))
              )
            )
          ),

          div(
            id = ns("filter_active_vars_contents"),
            tagList(
              lapply(
                self$datanames(),
                function(dataname) {
                  dataset_filters <- self$get_filtered_datasets(dataname)
                  dataset_filters$ui(
                    id = ns(sprintf("%s_filters", dataname))
                  )
                }
              )
            )
          )
        ),

        div(
          id = ns("filter_add_vars"), # not used, can be used to customize CSS behavior
          class = "well",
          tags$div(
            class = "row",
            tags$div(
              class = "col-sm-9",
              tags$label("Add Filter Variables", class = "text-primary", style = "margin-bottom: 15px;")
            ),
            tags$div(
              class = "col-sm-3",
              tags$a(
                href = "javascript:void(0)",
                class = "remove pull-right",
                onclick = sprintf("$('#%s').toggle();", ns("filter_add_vars_contents")),
                title = "minimise panel",
                tags$span(icon("minus-circle", lib = "font-awesome"))
              )
            )
          ),
          div(
            id = ns("filter_add_vars_contents"),
            tagList(
              lapply(
                self$datanames(),
                function(dataname) {
                  dataset_filters <- self$get_filtered_datasets(dataname)
                  id <- ns(sprintf("add_%s_filter", dataname))
                  # add span with same id to show / hide
                  return(
                    span(
                      id = id,
                      dataset_filters$ui_add_filter_state(id)
                    )
                  )
                }
              )
            )
          )
        )
      )
    },

    #' Server function for filter panel
    #'
    #' @param input (`shiny`)\cr
    #' @param output (`shiny`)\cr
    #' @param session (`shiny`)\cr
    #' @param active_datanames `function / reactive` returning datanames that
    #'   should be shown on the filter panel,
    #'   must be a subset of the `datanames` argument provided to `ui_filter_panel`;
    #'   if the function returns `NULL` (as opposed to `character(0)`), the filter
    #'   panel will be hidden
    #'
    srv_filter_panel = function(input, output, session, active_datanames = function() "all") {
      stopifnot(
        is.function(active_datanames) || is.reactive(active_datanames)
      )
      callModule(
        self$srv_filter_overview,
        "teal_filters_info",
        active_datanames = active_datanames
      )

      # use isolate because we assume that the number of datasets does not change over the course of the teal app
      # alternatively, one can proceed as in modules_filter_items to dynamically insert, remove UIs
      isol_datanames <- isolate(self$datanames()) # they are already ordered
      # should not use for-loop as variables are otherwise only bound by reference and last dataname would be used
      lapply(
        isol_datanames,
        function(dataname) {
          dataset_filters <- self$get_filtered_datasets(dataname)
          callModule(
            module = dataset_filters$server,
            id = sprintf("%s_filters", dataname)
          )
        }
      )

      lapply(
        isol_datanames,
        function(dataname) {
          dataset_filters <- self$get_filtered_datasets(dataname)
          callModule(
            module = dataset_filters$srv_add_filter_state,
            id = sprintf("add_%s_filter", dataname)
          )
        }
      )


      # we keep anything that may be selected to add (happens when the variable is not available for filtering)
      # lapply(isol_datanames, function(dataname) paste0("teal_add_", dataname, "_filter")) #nolint
      setBookmarkExclude(names = c(
        # these will be regenerated dynamically
        lapply(isol_datanames, function(dataname) paste0(dataname, "filters"))
      ))

      # rather than regenerating the UI dynamically for the dataset filtering,
      # we instead choose to hide/show the elements
      # the filters for this dataset are just hidden from the UI, but still applied
      # optimization: we set `priority = 1` to execute it before the other
      # observers (default priority 0), so that they are not computed if they are hidden anyways
      observeEvent(active_datanames(), priority = 1, {
        if (length(active_datanames()) == 0 || is.null(active_datanames())) {
          # hide whole module UI when no datasets or when NULL
          shinyjs::hide("filter_panel_whole")
        } else {
          shinyjs::show("filter_panel_whole")

          # selectively hide / show to only show `active_datanames` out of all datanames
          lapply(
            self$datanames(),
            function(dataname) {
              id_add_filter <- sprintf("add_%s_filter", dataname)
              id_filter_dataname <- sprintf("%s_filters", dataname)

              if (dataname %in% active_datanames()) {
                # shinyjs takes care of the namespace around the id
                shinyjs::show(id_add_filter)
                shinyjs::show(id_filter_dataname)
              } else {
                shinyjs::hide(id_add_filter)
                shinyjs::hide(id_filter_dataname)
              }
            }
          )
        }
      }, ignoreNULL = FALSE)

      observeEvent(input$remove_all_filters, {
        .log("removing all active filters from filter panel")
        lapply(self$datanames(), function(dataname) {
          dataset_filter <- self$get_filtered_datasets(dataname = dataname)
          dataset_filter$queues_empty()
        })
      })
    },

    #' Creates the UI for the module showing counts for each dataset
    #' contrasting the filtered to the full unfiltered dataset
    #'
    #' Per dataset, it displays
    #' the number of rows/observations in each dataset,
    #' the number of unique subjects.
    #'
    #' @param id module id
    ui_filter_overview = function(id) {
      ns <- NS(id)

      div(
        class = "teal_active_summary_filter_panel",
        tableOutput(ns("table"))
      )
    },

    #' Server function to display the number of patients in the filtered and unfiltered
    #' data
    #'
    #' @param input (`shiny`)\cr
    #' @param output (`shiny`)\cr
    #' @param session (`shiny`)\cr
    #' @param active_datanames (`function`, `reactive`)\cr
    #'   returning datanames that should be shown on the filter panel,
    #'   must be a subset of the `datanames` argument provided to `ui_filter_panel`;
    #'   if the function returns `NULL` (as opposed to `character(0)`), the filter
    #'   panel will be hidden.
    srv_filter_overview = function(input, output, session, active_datanames = function() "all") {
      stopifnot(
        is.function(active_datanames) || is.reactive(active_datanames)
      )

      output$table <- renderUI({
        .log("update uifiltersinfo")
        datanames <- if (identical(active_datanames(), "all")) {
          self$datanames()
        } else {
          active_datanames()
        }

        datasets_df <- self$get_filter_overview(datanames = datanames)

        body_html <- lapply(
          seq_len(nrow(datasets_df)),
          function(x) {
            tags$tr(
              tags$td(rownames(datasets_df)[x]),
              tags$td(datasets_df[x, 1]),
              tags$td(datasets_df[x, 2])
            )
          }
        )

        header_html <- tags$tr(
          tags$td(""),
          tags$td(colnames(datasets_df)[1]),
          tags$td(colnames(datasets_df)[2])
        )

        table_html <- tags$table(
          class = "table custom-table",
          tags$thead(header_html),
          tags$tbody(body_html)
        )

        table_html
      })

      return(invisible(NULL))
    }

  ),

  ## __Private Methods ====
  private = list(

    # private attributes ----
    filtered_datasets = list(),

    # preprocessing code used to generate the unfiltered datasets as a string
    code = CodeClass$new(),

    # we implement these functions as checks rather than returning logicals so they can
    # give informative error messages immediately

    # @details
    # Validates the state of this FilteredData.
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
        if (!dataname %in% names(self$get_filtered_datasets())) {
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

#' Refers the default filter state
#'
#' @description `r lifecycle::badge("maturing")`
#' You can use it to refer to the variable's default filter state,
#' which will be set when `FilteredData$set_data` is called.
#' It can be used together with `teal::init`.
#'
#' @details
#' This is a simple wrapper around an S3 class.
#'
#' @return `default_filter` a default filter object
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


#' Gets filter expression for multiple datanames taking into account its order.
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
