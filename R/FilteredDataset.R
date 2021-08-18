#' Initializes `FilteredDataset`
#'
#' `FilteredDataset` contains `Dataset`
#' @param dataset (`Dataset`)\cr
#' @param join_keys (`named list`)\cr
#'   named list of join keys to merge this dataset with
#'   it's relatives.
#' @examples
#' library(scda)
#' adsl <- cdisc_dataset("ADSL", synthetic_cdisc_data("latest")$adsl)
#' adtte <- cdisc_dataset("ADTTE", synthetic_cdisc_data("latest")$adtte)
#' data <- cdisc_data(adsl, adtte)
#'
#' filtered_dataset <- teal:::init_filtered_dataset(
#'   dataset = adtte,
#'   join_keys = data$get_join_keys()$get("ADTTE")
#' )
#'
#' \dontrun{
#' shinyApp(
#'   ui = fluidPage(
#'     actionButton("clear", span(icon("times"), "Remove all filters")),
#'     filtered_dataset$ui_add_filter_state(id = "add"),
#'     filtered_dataset$ui("dataset"),
#'     verbatimTextOutput("call"),
#'     tableOutput("tbl")
#'   ),
#'   server = function(input, output, session) {
#'     callModule(
#'       filtered_dataset$srv_add_filter_state,
#'       id = "add"
#'     )
#'
#'     callModule(filtered_dataset$server, id = "dataset")
#'
#'     output$call <- renderText({
#'       paste(
#'         vapply(filtered_dataset$get_call(), pdeparse, character(1)),
#'         collapse = "\n"
#'       )
#'     })
#'
#'     output$tbl <- renderTable({
#'       filtered_dataset$get_data(filtered = TRUE)
#'     })
#'
#'     observeEvent(
#'       input$clear,
#'       filtered_dataset$queues_empty()
#'     )
#'   }
#' )
#' }
init_filtered_dataset <- function(dataset, join_keys) { #nolint
  UseMethod("init_filtered_dataset")
}

#' @export
init_filtered_dataset.Dataset <- function(dataset, join_keys) { #nolint #nousage
  DefaultFilteredDataset$new(dataset, join_keys)
}

#' @export
init_filtered_dataset.CDISCDataset <- function(dataset, join_keys) { #nolint #nousage
  CDISCFilteredDataset$new(dataset, join_keys)
}

#' @export
init_filtered_dataset.MAEDataset <- function(dataset, join_keys) { #nolint #nousage
  MAEFilteredDataset$new(dataset, join_keys)
}

# FilteredDataset abstract --------
#' @title `FilterStates` R6 class
#' @description
#' `FilteredDataset` is a class which renders/controls `FilterStates`(s) for `Dataset`.
#' Each `FilteredDataset` contains `filter_states` field - a `list` which contains one
#' (`data.frame`) or multiple (`MultiAssayExperiment`) `FilterStates` objects.
#' Each `FilterStates` is responsible for one filter/subset expression applied for specific
#' components of the `Dataset`.
FilteredDataset <- R6::R6Class( # nolint
  "FilteredDataset",
  ## __Public Methods ====
  public = list(
    #' @description
    #' Initializes this `FilteredDataset` object
    #'
    #' @param dataset (`Dataset`)\cr
    #'  single dataset for which filters are rendered
    #' @param join_keys (`list`)\cr
    #'  keys to join this `dataset` with the other
    initialize = function(dataset, join_keys) {
      stopifnot(is.null(join_keys) || is_fully_named_list(join_keys))
      stopifnot(is(dataset, "Dataset"))
      private$dataset <- dataset
      private$join_keys <- join_keys

      dataname <- self$get_dataname()
      private$reactive_data <- reactive({
        .log("################################################# Refiltering dataset ", dataname)
        env <- new.env(parent = parent.env(globalenv()))
        for (idx in seq_along(private$eval_env)) {
          env[[names(private$eval_env)[idx]]] <- if (is.reactive(private$eval_env[[idx]])) {
            private$eval_env[[idx]]()
          } else {
            private$eval_env[[idx]]
          }
        }
        env[[dataname]] <- self$get_data(filtered = FALSE)
        filter_call <- self$get_call()
        eval_expr_with_msg(filter_call, env)
        get(x = self$get_filtered_dataname(), envir = env)
      })

      invisible(self)
    },

    #' @description
    #' Adds objects to the filter call evaluation environment
    #' @param name (`character`) object name
    #' @param value object value
    #' @return invisibly this FilteredDataset
    add_to_eval_env = function(name, value) {
      stopifnot(is_character_single(name))
      private$eval_env <- c(private$eval_env, setNames(value, name))
      invisible(self)
    },


    #' @description
    #' Removes all active filter items applied to this dataset
    #' @return NULL
    queues_empty = function() {
      lapply(
        self$get_filter_states(),
        function(queue) queue$queue_empty()
      )
      NULL
    },

    # getters ----
    #' @description
    #' Gets a filter expression
    #'
    #' This functions returns filter calls equivalent to selected items
    #' within each of `filter_states`. Configuration of the calls is constant and
    #' depends on `filter_states` type and order which are set during initialization.
    #' @return filter `call` or `list` of filter calls
    get_call = function() {
      stop("Pure virtual method.")
    },

    #' @description
    #' Gets raw data of this dataset
    #' @param filtered (`logical(1)`)\cr
    #'   whether returned data should be filtered or not
    #' @return type of returned object depending on a data stored in
    #' `Dataset`. Currently `data.frame` or `MultiAssayExperiment`
    get_data = function(filtered) {
      if (isTRUE(filtered)) {
        self$get_data_reactive()()
      } else {
        get_raw_data(self$get_dataset())
      }
    },

    #' @description
    #' Gets the reactive object which returns filtered data
    #' @return (`reactive`)
    get_data_reactive = function() {
      private$reactive_data
    },

    #' @description
    #' Gets the filter states
    #' @param id (`character(1)`, `character(0)`)\cr
    #'   the id of the `private$filter_states` list element where `FilterStates` is kept.
    #' @return `FilterStates` or `list` of `FilterStates` objects.
    get_filter_states = function(id = character(0)) {
      if (is_empty(id)) {
        private$filter_states
      } else {
        private$filter_states[[id]]
      }
    },

    #' @description
    #' Gets data info
    #' @param filtered (`logical(1)`)\cr
    #'   whether `data.info` should depend on filtered data.
    #' @return `integer(1)` number of rows
    get_data_info = function(filtered = FALSE) {
      nrow(self$get_data(filtered = filtered))
    },

    #' @description
    #' Gets subjects info
    #' @param filtered (`logical(1)`)\cr
    #'   whether `data.info` should depend on filtered data.
    #' @return `integer` number of unique subjects
    get_subjects_info = function(filtered) {
      integer(0)
    },

    #' @description
    #' Gets the name of the dataset
    #'
    #' Get name of the dataset
    #' @return `character(1)` as a name of this dataset
    get_dataname = function() {
      get_dataname(self$get_dataset())
    },

    #' @description
    #' Gets the dataset in this FilteredDataset
    #' @return `Dataset`
    get_dataset = function() {
      private$dataset
    },

    #' @description
    #' Returns the hash of the unfiltered dataset
    #' @return (`character(1)`) the hash
    get_hash = function() {
      private$dataset$get_hash()
    },

    #' @description
    #' Gets the keys for the dataset of this FilteredDataset
    #' @return (`character`) the keys of dataset
    get_keys = function() {
      self$get_dataset()$get_keys()
    },

    #' Gets join keys to join the dataset of this FilteredDataset
    #' with other Dataset objects.
    #' @return `list` of keys
    get_join_keys = function() {
      private$join_keys
    },

    #' @description
    #' Gets labels of variables in the data
    #'
    #' Variables are the column names of the data.
    #' Either, all labels must have been provided for all variables
    #' in `set_data` or `NULL`.
    #'
    #' @param variables (`character` vector) variables to get labels for;
    #'   if `NULL`, for all variables in data
    #' @return (`character` or `NULL`) variable labels, `NULL` if `column_labels`
    #'   attribute does not exist for the data
    get_varlabels = function(variables = NULL) {
      stopifnot(is.null(variables) || is_character_vector(variables, min_length = 0L))

      labels <- self$get_dataset()$get_column_labels()
      if (is.null(labels)) {
        return(NULL)
      }
      if (!is.null(variables)) labels <- labels[intersect(self$get_varnames(), variables)]
      labels
    },

    #' @description
    #' Gets variable names from dataset
    #' @return `character` the variable names
    get_varnames = function() {
      colnames(self$get_data(filtered = FALSE))
    },

    #' @description
    #' Gets the suffixed dataname
    #' Used when filtering the data to get `<dataname>_FILTERED`,
    #' `<dataname>_FILTERED_ALONE` or any other name.
    #' @param dataname (`character(1)`) dataname
    #' @param suffix (`character(1)`) string to be putted after dataname
    #' @return `character(1)`
    get_filtered_dataname = function(dataname = self$get_dataname(), suffix = "_FILTERED") {
      paste0(dataname, suffix)
    },

    #' @description
    #' Sets the bookmark state
    #'
    #' @param state (`named list`)\cr
    #'  containing values of the initial filter. Values should be relevant
    #'  to the referred column.
    set_bookmark_state = function(state) {
      stop("Pure virtual method.")
    },

    # modules ------
    #' @description
    #' UI module for dataset active filters
    #'
    #' UI module containing dataset active filters along with
    #' title and remove button.
    #' @param id (`character(1)`)\cr
    #'  identifier of the element - preferably containing dataset name
    #'
    #' @return function - shiny UI module
    ui = function(id) {
      dataname <- self$get_dataname()
      stopifnot(
        is_character_single(dataname)
      )
      ns <- NS(id)
      if_multiple_filter_states <- length(self$get_filter_states()) > 1
      span(
        id = id,
        div(
          id = ns("whole_ui"), # to hide it entirely
          fluidRow(
            column(
              width = 8,
              tags$span(dataname, class = "filter_panel_dataname")
            ),
            column(
              width = 4,
              actionLink(
                ns("remove_filters"),
                label = "",
                icon = icon("times-circle", lib = "font-awesome"),
                class = "remove pull-right"
              )
            )
          ),
          div(
            # id needed to insert and remove UI to filter single variable as needed
            # it is currently also used by the above module to entirely hide this panel
            id = ns("filters"),
            class = "parent-hideable-list-group",
            tagList(
              lapply(
                names(self$get_filter_states()),
                function(x) {
                  tagList(self$get_filter_states(id = x)$ui(id = ns(x)))
                }
              )
            )
          )
        )
      )
    },

    #' @description
    #' Server module for a dataset active filters
    #'
    #' Server module managing a  active filters.
    #' @param input (`shiny`)\cr
    #' @param output (`shiny`)\cr
    #' @param session (`shiny`)\cr
    #'  single dataset for which filters are rendered
    #' @return function - shiny server module
    server = function(input, output, session) {
      dataname <- self$get_dataname()
      stopifnot(
        is_character_single(dataname)
      )

      observeEvent(input$remove_filters, {
        .log("removing all filters for data", self$get_dataname())
        lapply(
          self$get_filter_states(),
          function(x) x$queue_empty()
        )
      })
    },

    #' @description
    #' UI module to add filter variable for this dataset
    #'
    #' UI module to add filter variable for this dataset
    #' @param id (`character(1)`)\cr
    #'  identifier of the element - preferably containing dataset name
    #'
    #' @return function - shiny UI module
    ui_add_filter_state = function(id) {
      stop("Pure virtual method")
    },

    #' @description
    #' Server module to add filter variable for this dataset
    #'
    #' Server module to add filter variable for this dataset
    #' @param input (`shiny`)\cr
    #' @param output (`shiny`)\cr
    #' @param session (`shiny`)\cr
    #' @return function - shiny server module
    srv_add_filter_state = function(input, output, session) {
      stop("Pure virtual method")
    }
  ),
  ## __Private Fields ====
  private = list(
    dataset = NULL, # Dataset
    reactive_data = NULL, # reactive
    eval_env = list(),
    filter_states = list(),
    join_keys = NULL, # JoinKeySet

    # Adds `FilterStates` to the `private$filter_states`.
    # `FilterStates` is added once for each element of the dataset.
    # @param filter_states (`FilterStates`)
    # @param id (`character(1)`)
    add_filter_states = function(filter_states, id) {
      stopifnot(is(filter_states, "FilterStates"))
      stopifnot(is_character_single(id))

      x <- setNames(list(filter_states), id)
      private$filter_states <- c(self$get_filter_states(), x)
    },

    # @description
    # Checks if the dataname exists and
    # (if provided) that varname is a valid column in the dataset
    #
    # Stops when this is not the case.
    #
    # @param varname (`character`) column within the dataset;
    #   if `NULL`, this check is not performed
    check_data_varname_exists = function(varname = NULL) {
      stopifnot(is.null(varname) || is_character_single(varname))

      isolate({
        if (!is.null(varname) && !(varname %in% self$get_varnames())) {
          stop(
            sprintf("variable '%s' does not exist in data '%s'", varname, dataname)
          )
        }
      })

      return(invisible(NULL))
    }
  )
)

# DefaultFilteredDataset ------
#' @title `DefaultFilteredDataset` R6 class
DefaultFilteredDataset <- R6::R6Class( # nolint
  classname = "DefaultFilteredDataset",
  inherit = FilteredDataset,
  public = list(

    #' @description
    #' Initialize `DefaultFilteredDataset` object
    #'
    #' @param dataset (`Dataset`)\cr
    #'  single dataset for which filters are rendered
    #' @param join_keys (`list`)\cr
    #'  keys to join this `dataset` with the other
    initialize = function(dataset, join_keys) {
      stopifnot(is(dataset, "Dataset"))
      super$initialize(dataset, join_keys)
      dataname <- get_dataname(dataset)

      private$add_filter_states(
        filter_states = init_filter_states(
          data = get_raw_data(dataset),
          input_dataname = as.name(dataname),
          output_dataname = as.name(sprintf("%s_FILTERED", dataname)),
          varlabels = self$get_varlabels(),
          keys = self$get_keys()
        ),
        id = "filter"
      )
      return(invisible(self))
    },

    #' @description
    #' Get filter expression
    #'
    #' This functions returns filter calls equivalent to selected items
    #' within each of `filter_states`. Configuration of the calls is constant and
    #' depends on `filter_states` type and order which are set during initialization.
    #' This class contains single `FilterStates`
    #' which contains single `ReactiveQueue` and all `FilterState` objects
    #' applies to one argument (`...`) in `dplyr::filter` call.
    #' @return filter `call` or `list` of filter calls
    get_call = function() {
      Filter(
        f = Negate(is.null),
        x = lapply(
          self$get_filter_states(),
          function(x) x$get_call()
        )
      )
    },

    #' @description
    #' Set bookmark state
    #'
    #' @param state (`named list`)\cr
    #'  containing values of the initial filter. Values should be relevant
    #'  to the referred column.
    set_bookmark_state = function(state) {
      stopifnot(is.list(state))
      data <- self$get_data(filtered = FALSE)
      fs <- self$get_filter_states()[[1]]
      fs$set_bookmark_state(
        state = state,
        data = data
      )
      return(invisible(NULL))
    },

    #' @description
    #' UI module to add filter variable for this dataset
    #'
    #' UI module to add filter variable for this dataset
    #' @param id (`character(1)`)\cr
    #'  identifier of the element - preferably containing dataset name
    #'
    #' @return function - shiny UI module
    ui_add_filter_state = function(id) {
      ns <- NS(id)
      tagList(
        tags$label("Add", tags$code(self$get_dataname()), "filter"),
        self$get_filter_states(id = "filter")$ui_add_filter_state(
          id = ns("filter"),
          data = get_raw_data(self$get_dataset())
        )
      )
    },

    #' @description
    #' Server module to add filter variable for this dataset
    #'
    #' Server module to add filter variable for this dataset.
    #' For this class `srv_add_filter_state` calls single module
    #' `srv_add_filter_state` from `FilterStates` (`DefaultFilteredDataset`
    #' contains single `FilterStates`)
    #'
    #' @param input (`shiny`)\cr
    #' @param output (`shiny`)\cr
    #' @param session (`shiny`)\cr
    #' @return function - shiny server module
    srv_add_filter_state = function(input, output, session) {
      data <- get_raw_data(self$get_dataset())
      callModule(
        module = self$get_filter_states(id = "filter")$srv_add_filter_state,
        id = "filter",
        data = data
      )
    }
  ),
  private = list(
  )
)


# CDISCFilteredDataset ------
#' @title `CDISCFilteredDataset` R6 class
CDISCFilteredDataset <- R6::R6Class( # nolint
  classname = "CDISCFilteredDataset",
  inherit = DefaultFilteredDataset,
  public = list(
    #' @description
    #' Get filter expression
    #'
    #' This functions returns filter calls equivalent to selected items
    #' within each of `filter_states`. Configuration of the calls is constant and
    #' depends on `filter_states` type and order which are set during initialization.
    #' This class contains single `FilterStates`
    #' which contains single `ReactiveQueue` and all `FilterState` objects
    #' applies to one argument (`...`) in `dplyr::filter` call.
    #' It's also possible within this class to return `merge` call
    #' with other `data.frame` which is a parent.
    #' @return filter `call` or `list` of filter calls
    get_call = function() {
      if (is_empty(self$get_dataset()$get_parent())) {
        super$get_call()
      } else {
        parent_dataname <- self$get_dataset()$get_parent()
        keys <- self$get_join_keys()[[parent_dataname]]
        parent_keys <- names(keys)
        dataset_keys <- unname(keys)

        filtered_dataname_alone <- self$get_filtered_dataname(suffix = "_FILTERED_ALONE")
        filtered_dataname <- self$get_filtered_dataname()
        filtered_parentname <- self$get_filtered_dataname(dataname = parent_dataname)

        premerge_call <- Filter(
          f = Negate(is.null),
          x = lapply(
            self$get_filter_states(),
            function(x) x$get_call()
          )
        )
        premerge_call[[1]][[2]] <- as.name(filtered_dataname_alone)
        merge_call <- call(
          "<-",
          as.name(filtered_dataname),
          call_with_colon(
            "dplyr::inner_join",
            x = as.name(filtered_dataname_alone),
            y = if (is_empty(parent_keys)) {
              as.name(filtered_parentname)
            } else {
              call_extract_array(
                dataname = filtered_parentname,
                column = parent_keys,
                aisle = call("=", as.name("drop"), FALSE)
              )
            },
            unlist_args = if (is_empty(parent_keys) || is_empty(dataset_keys)) {
              list()
            } else if (identical(parent_keys, dataset_keys)) {
              list(by = parent_keys)
            } else {
              list(by = setNames(parent_keys, nm = dataset_keys))
            }
          )
        )
        c(premerge_call, merge_call)
      }
    },

    #' @description
    #' Get subjects info
    #' @param filtered (`logical(1)`)\cr
    #'   whether subject number should depend on filtered data.
    #' @return `integer(1)` number of unique subjects
    get_subjects_info = function(filtered) {
      subject_keys <- if (!is_empty(self$get_dataset()$get_parent())) {
        self$get_join_keys()[[self$get_dataset()$get_parent()]]
      } else {
        self$get_keys()
      }
      if (is_empty(subject_keys)) {
        dplyr::n_distinct(self$get_data(filtered = filtered))
      } else {
        dplyr::n_distinct(self$get_data(filtered = filtered)[subject_keys])
      }
    },

    #' @description
    #' UI module to add filter variable for this dataset
    #'
    #' UI module to add filter variable for this dataset
    #' @param id (`character(1)`)\cr
    #'  identifier of the element - preferably containing dataset name
    #'
    #' @return function - shiny UI module
    ui_add_filter_state = function(id) {
      ns <- NS(id)
      tagList(
        tags$label("Add", tags$code(self$get_dataname()), "filter"),
        self$get_filter_states(id = "filter")$ui_add_filter_state(
          id = ns("filter"),
          data = private$get_data_without_parent_keys()
        )
      )
    },

    #' @description
    #' Server module to add filter variable for this dataset
    #'
    #' Server module to add filter variable for this dataset.
    #' For this class `srv_add_filter_state` calls single module
    #' `srv_add_filter_state` from `FilterStates` (`DefaultFilteredDataset`
    #' contains single `FilterStates`)
    #'
    #' @param input (`shiny`)\cr
    #' @param output (`shiny`)\cr
    #' @param session (`shiny`)\cr
    #' @return function - shiny server module
    srv_add_filter_state = function(input, output, session) {
      callModule(
        module = self$get_filter_states(id = "filter")$srv_add_filter_state,
        id = "filter",
        data = private$get_data_without_parent_keys()
      )
    }
  ),
  private = list(
  # Returns the data.frame object without the columns used to join
  # with the parent dataset specified in the join_keys argument
  # to the constructor.
    get_data_without_parent_keys = function() {
      if (is_empty(self$get_dataset()$get_parent())) {
        self$get_dataset()$data
      } else {
        join_columns_names <- self$get_join_keys()[[self$get_dataset()$get_parent()]]
        dplyr::select(self$get_dataset()$data, -all_of(join_columns_names))
      }
    }
  )
)


# MAEFilteredDataset ------
#' @title `MAEFilteredDataset` R6 class
MAEFilteredDataset <- R6::R6Class( # nolint
  classname = "MAEFilteredDataset",
  inherit = FilteredDataset,
  public = list(

    #' @description
    #' Initialize `MAEFilteredDataset` object
    #'
    #' @param dataset (`MAEDataset`)\cr
    #'  single dataset for which filters are rendered
    #' @param join_keys (`list`)\cr
    #'  keys to join this `dataset` with the other
    initialize = function(dataset, join_keys) {
      stopifnot(is(dataset, "MAEDataset"))
      super$initialize(dataset, join_keys)

      dataname <- self$get_dataname()
      raw_data <- get_raw_data(dataset)
      experiment_names <- names(raw_data)

      # subsetting by subjects means subsetting by colData(MAE)
      private$add_filter_states(
        filter_states = init_filter_states(
          data = raw_data,
          input_dataname = as.name(dataname),
          output_dataname = as.name(sprintf("%s_FILTERED", dataname)),
          varlabels = self$get_varlabels(),
          datalabel = "subjects",
          keys = self$get_keys()
        ),
        id = "subjects"
      )

      # elements of the list (experiments) are unknown
      # dispatch needed because we can't hardcode methods otherwise:
      #  if (matrix) else if (SummarizedExperiment) else if ...
      lapply(
        experiment_names,
        function(experiment_name) {
          input_dataname <- call_extract_list(
            sprintf("%s_FILTERED", dataname),
            experiment_name,
            dollar = FALSE
          )

          private$add_filter_states(
            filter_states = init_filter_states(
              data = raw_data[[experiment_name]],
              input_dataname = input_dataname,
              output_dataname = input_dataname,
              datalabel = experiment_name
            ),
            id = experiment_name
          )

        }
      )

    },

    #' @description
    #' Get filter expression
    #'
    #' This functions returns filter calls equivalent to selected items
    #' within each of `filter_states`. Configuration of the calls is constant and
    #' depends on `filter_states` type and order which are set during initialization.
    #' This class contains multiple `FilterStates`:
    #' \itemize{
    #'   \item{`colData(dataset)`}{for this object single `MAEFilterStates`
    #'   which returns `subsetByColData` call}
    #'   \item{experiments}{for each experiment single `SEFilterStates` and
    #'   `FilterStates_matrix`, both returns `subset` call}
    #' }
    #' @return filter `call` or `list` of filter calls
    get_call = function() {
      Filter(
        f = Negate(is.null),
        x = lapply(
          self$get_filter_states(),
          function(x) x$get_call()
        )
      )
    },

    #' @description
    #' Get data info
    #' @param filtered (`logical(1)`)\cr
    #'   whether `data.info` should depend on filtered data.
    #' @return `integer(1)` number of rows in `colData(data)`
    get_data_info = function(filtered = FALSE) {
      nrow(
        SummarizedExperiment::colData(
          self$get_data(filtered = filtered)
        )
      )
    },

    #' @description
    #' Set bookmark state
    #'
    #' @param state (`named list`)\cr
    #'  names of the list should correspond to the names of the initialized `FilterStates`
    #'  kept in `private$filter_states`. For this object they are `"subjects"` and
    #'  names of the experiments. Values of initial state should be relevant
    #'  to the referred column.
    #'
    set_bookmark_state = function(state) {
      stopifnot(
        is.list(state),
        all(names(state) %in% c(names(self$get_filter_states())))
      )
      data <- self$get_data(filtered = FALSE)
      for (fs_name in names(state)) {
        fs <- self$get_filter_states()[[fs_name]]
        fs$set_bookmark_state(
          state = state[[fs_name]],
          data = `if`(fs_name == "subjects", data, data[[fs_name]])
        )
      }

      return(invisible(NULL))
    },

    #' @description
    #' UI module to add filter variable for this dataset
    #'
    #' UI module to add filter variable for this dataset
    #' @param id (`character(1)`)\cr
    #'  identifier of the element - preferably containing dataset name
    #'
    #' @return function - shiny UI module
    ui_add_filter_state = function(id) {
      ns <- NS(id)
      data <- get_raw_data(self$get_dataset())
      experiment_names <- names(data)

      div(
        tags$label("Add", tags$code(self$get_dataname()), "filter"),
        br(),
        HTML("&#9658;"),
        tags$label("Add subjects filter"),
        self$get_filter_states("subjects")$ui_add_filter_state(
          id = ns("subjects"),
          data = data
        ),
        tagList(
          lapply(
            experiment_names,
            function(experiment_name) {
              tagList(
                HTML("&#9658;"),
                tags$label("Add", tags$code(experiment_name), "filter"),
                self$get_filter_states(experiment_name)$ui_add_filter_state(
                  id = ns(experiment_name),
                  data = data[[experiment_name]]
                )
              )
            }
          )
        )
      )
    },

    #' @description
    #' Server module to add filter variable for this dataset
    #'
    #' Server module to add filter variable for this dataset.
    #' For this class `srv_add_filter_state` calls multiple modules
    #' of the same name from `FilterStates` as `MAEFilteredDataset`
    #' contains one `FilterStates` object for `colData` and one for each
    #' experiment.
    #'
    #' @param input (`shiny`)\cr
    #' @param output (`shiny`)\cr
    #' @param session (`shiny`)\cr
    #' @return function - shiny server module
    srv_add_filter_state = function(input, output, session) {
      data <- get_raw_data(self$get_dataset())
      callModule(
        module = self$get_filter_states("subjects")$srv_add_filter_state,
        id = "subjects",
        data = data # MultiAssayExperiment
      )

      experiment_names <- names(data)
      lapply(
        experiment_names,
        function(experiment_name) {
          callModule(
            module = self$get_filter_states(experiment_name)$srv_add_filter_state,
            id = experiment_name,
            data = data[[experiment_name]] # SummarizedExperiment or matrix
          )
        }
      )
    }
  )
)
