.threshold_slider_vs_checkboxgroup <- 5 #nolint

# label of checkbox to keep / remove NAs
label_keep_na_count <- function(na_count) {
  sprintf("Keep NA (%s)", na_count)
}

#' Initializes `FilterState`
#'
#' Initializes `FilterState` depending on a variable class.\cr
#' @param x (`vector`)\cr
#'   values of the variable used in filter
#'
#' @param varname (`character(1)`, `name`)\cr
#'   name of the variable
#'
#' @param varlabel (`character(1)`)\cr
#'   label of the variable (optional).
#'
#' @param input_dataname (`name` or `call`)\cr
#'   name of dataset where `x` is taken from. Must be specified if `extract_type` argument
#'   is not empty.
#'
#' @param extract_type (`character(0)`, `character(1)`)\cr
#' whether condition calls should be prefixed by dataname. Possible values:
#' \itemize{
#' \item{`character(0)` (default)}{ `varname` in the condition call will not be prefixed}
#' \item{`"list"`}{ `varname` in the condition call will be returned as `<input_dataname>$<varname>`}
#' \item{`"matrix"`}{ `varname` in the condition call will be returned as `<input_dataname>[, <varname>]`}
#' }
#'
#' @examples
#' filter_state <- teal:::RangeFilterState$new(
#'   c(1:10, NA, Inf),
#'   varname = "x",
#'   varlabel = "Pretty name",
#'   input_dataname = as.name("dataname"),
#'   extract_type = "matrix"
#' )
#'
#' filter_state$get_varname()
#' filter_state$get_varlabel()
#' isolate(filter_state$get_call())
#'
#' \dontrun{
#' shinyApp(
#'   ui = fluidPage(
#'     isolate(filter_state$ui(id = "app")),
#'     verbatimTextOutput("call")
#'   ),
#'   server = function(input, output, session) {
#'     callModule(filter_state$server, "app")
#'
#'     output$call <- renderText(
#'       pdeparse(filter_state$get_call())
#'     )
#'   }
#' )
#' }
#' @return `FilterState` object
init_filter_state <- function(x,
                              varname,
                              varlabel = if_null(attr(x, "label"), character(0)),
                              input_dataname = NULL,
                              extract_type = character(0)) {
  stopifnot(is_character_single(varname) || is.name(varname))
  stopifnot(is_character_vector(varlabel, min_length = 0, max_length = 1))
  stopifnot(is.null(input_dataname) || is.name(input_dataname) || is.call(input_dataname))
  stopifnot(is_character_vector(extract_type, min_length = 0, max_length = 1))
  stopifnot(
    length(extract_type) == 0 ||
    length(extract_type) == 1 && !is.null(input_dataname)
  )
  stopifnot(extract_type %in% c("list", "matrix"))


  if (all(is.na(x))) {
    return(
      EmptyFilterState$new(
        x = x,
        varname = varname,
        varlabel = varlabel,
        input_dataname = input_dataname,
        extract_type = extract_type
      )
    )
  }

  UseMethod("init_filter_state")
}

#' @export
init_filter_state.default <- function(x, #nousage
                                      varname,
                                      varlabel = if_null(attr(x, "label"), character(0)),
                                      input_dataname = NULL,
                                      extract_type = character(0)) {
  FilterState$new(x = x,
                  varname = varname,
                  varlabel = varlabel,
                  input_dataname = input_dataname,
                  extract_type = extract_type)
}

#' @export
init_filter_state.logical <- function(x, #nousage
                                      varname,
                                      varlabel = if_null(attr(x, "label"), character(0)),
                                      input_dataname = NULL,
                                      extract_type = character(0)) {
  LogicalFilterState$new(x = x,
                         varname = varname,
                         varlabel = varlabel,
                         input_dataname = input_dataname,
                         extract_type = extract_type)
}

#' @export
init_filter_state.numeric <- function(x, #nousage
                                      varname,
                                      varlabel = if_null(attr(x, "label"), character(0)),
                                      input_dataname = NULL,
                                      extract_type = character(0)) {
  if (length(unique(x[!is.na(x)])) < .threshold_slider_vs_checkboxgroup) {
    ChoicesFilterState$new(x = x,
                           varname = varname,
                           varlabel = varlabel,
                           input_dataname = input_dataname,
                           extract_type = extract_type)
  } else {
    RangeFilterState$new(x = x,
                         varname = varname,
                         varlabel = varlabel,
                         input_dataname = input_dataname,
                         extract_type = extract_type)
  }
}

#' @export
init_filter_state.factor <- function(x, #nousage
                                     varname,
                                     varlabel = if_null(attr(x, "label"), character(0)),
                                     input_dataname = NULL,
                                     extract_type = character(0)) {
  ChoicesFilterState$new(x = x,
                         varname = varname,
                         varlabel = varlabel,
                         input_dataname = input_dataname,
                         extract_type = extract_type)
}

#' @export
init_filter_state.character <- function(x, #nousage
                                        varname,
                                        varlabel = if_null(attr(x, "label"), character(0)),
                                        input_dataname = NULL,
                                        extract_type = character(0)) {
  ChoicesFilterState$new(x = x,
                         varname = varname,
                         varlabel = varlabel,
                         input_dataname = input_dataname,
                         extract_type = extract_type)
}

#' @export
init_filter_state.Date <- function(x, #nousage
                                   varname,
                                   varlabel = if_null(attr(x, "label"), character(0)),
                                   input_dataname = NULL,
                                   extract_type = character(0)) {
  DateFilterState$new(x = x,
                      varname = varname,
                      varlabel = varlabel,
                      input_dataname = input_dataname,
                      extract_type = extract_type)
}

#' @export
init_filter_state.POSIXct <- function(x, #nousage
                                      varname,
                                      varlabel = if_null(attr(x, "label"), character(0)),
                                      input_dataname = NULL,
                                      extract_type = character(0)) {
  DatetimeFilterState$new(x = x,
                          varname = varname,
                          varlabel = varlabel,
                          input_dataname = input_dataname,
                          extract_type = extract_type)
}

#' @export
init_filter_state.POSIXlt <- function(x, #nousage
                                      varname,
                                      varlabel = if_null(attr(x, "label"), character(0)),
                                      input_dataname = NULL,
                                      extract_type = character(0)) {
  DatetimeFilterState$new(x = x,
                          varname = varname,
                          varlabel = varlabel,
                          input_dataname = input_dataname,
                          extract_type = extract_type)
}

# FilterState ------
#' @name FilterState
#' @docType class
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @title Abstract class to encapsulate filter states
#'
#' @details
#' This class is responsible for managing single filter item within
#' `FilteredData` class. Filter states depend on the variable type:
#' (`logical`, `integer`, `numeric`, `factor`, `character`, `Date`, `POSIXct`, `POSIXlt`)
#' and returns `FilterState` object with class corresponding to input variable.
#' Class controls single filter entry in `module_single_filter_item` and returns
#' code relevant to selected values.
#' - `factor`, `character`: `class = ChoicesFilterState`
#' - `numeric`: `class = RangeFilterState`
#' - `logical`: `class = LogicalFilterState`
#' - `Date`: `class = DateFilterState`
#' - `POSIXct`, `POSIXlt`: `class = DatetimeFilterState`
#' - all `NA` entries: `class: FilterState`, cannot be filtered
#' - default: `FilterState`, cannot be filtered
#' Each variable's filter state is a `R6` object which contains `choices`,
#' `selected`, `varname`, `dataname`, `labels`, `na_count`, `keep_na` and other
#' variable type specific fields (`keep_inf`, `inf_count`, `timezone`).
#' Object contains also shiny module (`ui` and `server`) which manages
#' state of the filter through reactive values `selected`, `keep_na`, `keep_inf`
#' which trigger `get_call()` and every R function call up in reactive
#' chain.
FilterState <- R6::R6Class( # nolint
  "FilterState",
  public = list(
    #' @description
    #' Initialize a `FilterState` object
    #' @param x (`vector`)\cr
    #'   values of the variable used in filter
    #' @param varname (`character`, `name`)\cr
    #'   name of the variable
    #' @param varlabel (`character(1)`)\cr
    #'   label of the variable (optional).
    #' @param input_dataname (`name` or `call`)\cr
    #'   name of dataset where `x` is taken from. Must be specified if `extract_type` argument
    #'   is not empty.
    #' @param extract_type (`character(0)`, `character(1)`)\cr
    #' whether condition calls should be prefixed by dataname. Possible values:
    #' \itemize{
    #' \item{`character(0)` (default)}{ `varname` in the condition call will not be prefixed}
    #' \item{`"list"`}{ `varname` in the condition call will be returned as `<input_dataname>$<varname>`}
    #' \item{`"matrix"`}{ `varname` in the condition call will be returned as `<input_dataname>[, <varname>]`}
    #' }
    initialize = function(x,
                          varname,
                          varlabel = character(0),
                          input_dataname = NULL,
                          extract_type = character(0)) {
      stopifnot(is.name(varname) || is.call(varname) || is_character_single(varname))
      stopifnot(is_character_vector(varlabel, min_length = 0, max_length = 1))
      stopifnot(is.null(input_dataname) || is.name(input_dataname) || is.call(input_dataname))
      stopifnot(is_character_vector(extract_type, min_length = 0, max_length = 1))
      stopifnot(
        length(extract_type) == 0 ||
          length(extract_type) == 1 && !is.null(input_dataname)
      )
      stopifnot(extract_type %in% c("list", "matrix"))

      private$input_dataname <- input_dataname
      private$varname <- if (is.character(varname)) {
        as.name(varname)
      } else {
        varname
      }
      private$varlabel <- if (identical(varlabel, as.character(varname))) {
        # to not display duplicated label
        character(0)
      } else {
        varlabel
      }
      private$extract_type <- extract_type
      private$selected <- reactiveVal(NULL)
      private$na_count <- sum(is.na(x))
      private$keep_na <- reactiveVal(value = FALSE)

      invisible(self)
    },

    #' @description
    #' Destroy observers stored in `private$observers`.
    destroy_observers = function() {
      .log(
        "Destroying observers for varname ",
        self$get_varname(deparse = TRUE),
        "in dataset",
        self$get_dataname(deparse = TRUE)
      )
      lapply(private$observers, function(x) x$destroy())
      return(invisible(NULL))
    },

    #' @description
    #' Returns reproducible condition call for current selection relevant
    #' for selected variable type.
    #' Method is using internal reactive values which makes it reactive
    #' and must be executed in reactive or isolated context.
    get_call = function() {
      NULL
    },

    #' @description
    #' Returns dataname
    #' @param deparse (`logical(1)`)\cr
    #' whether dataname should be deparsed. `TRUE` by default
    #' @return (`name` or `character(1)`)
    get_dataname = function(deparse = TRUE) {
      if (isTRUE(deparse)) {
        deparse(private$input_dataname)
      } else {
        private$input_dataname
      }
    },

    #' @description
    #' Returns current `keep_na` selection
    #' @return (`logical(1)`)
    get_keep_na = function() {
      private$keep_na()
    },

    #' @description
    #' Returns variable label
    #' @return (`character(1)`)
    get_varlabel = function() {
      private$varlabel
    },

    #' @description
    #' Get variable name
    #' @param deparse (`logical(1)`)\cr
    #' whether variable name should be deparsed. `FALSE` by default
    #' @return (`name` or `character(1)`)
    get_varname = function(deparse = FALSE) {
      if (isTRUE(deparse)) {
        deparse(private$varname)
      } else {
        private$varname
      }
    },

    #' @description
    #' Get selected values from `FilterState`
    #'
    #' @return class of the returned object depends of class of the `FilterState`
    get_selected = function() {
      private$selected()
    },

    #' @description
    #' Set if `NA` should be kept
    #' @param value (`logical(1)`)\cr
    #'  Value(s) which come from the filter selection. Value is set in `server`
    #'  modules after selecting check-box-input in the shiny interface. Values are set to
    #'  `private$keep_na` which is reactive.
    set_keep_na = function(value) {
      stopifnot(is_logical_single(value))
      private$keep_na(value)
      invisible(NULL)
    },

    #' @description
    #' Set selection
    #' @param value (`vector`)\cr
    #'  Value(s) which come from the filter selection. Values are set in `server`
    #'  modules after choosing value in app interface. Values are set to
    #'  `private$selected` which is reactive. Values type have to be the
    #'  same as `private$choices`.
    set_selected = function(value) {
      value <- private$cast_and_validate(value)
      value <- private$remove_out_of_bound_values(value)
      private$validate_selection(value)
      private$selected(value)
      invisible(NULL)
    },

    #' @description
    #' Set state
    #' @param state (`list`)\cr
    #'  contains fields relevant for a specific class
    #' \itemize{
    #' \item{`selected`}{ defines initial selection}
    #' \item{`keep_na` (`logical`)}{ defines whether to keep or remove `NA` values}
    #' }
    set_state = function(state) {
      stopifnot(is.list(state) && all(names(state) %in% c("selected", "keep_na")))
      if (!is.null(state$keep_na)) {
        self$set_keep_na(state$keep_na)
      }
      if (!is.null(state$selected)) {
        self$set_selected(state$selected)
      }

      invisible(NULL)
    },

    #' @description
    #' Server module
    #' @param input (`Shiny`)\cr input object
    #' @param output (`Shiny`)\cr output object
    #' @param session (`Shiny`)\cr session object
    #' @return nothing
    server = function(input, output, session) {
      NULL
    },

    #' @description
    #' UI Module
    #' @param id (`character(1)`)\cr
    #'  id of shiny element. UI for this class contains simple message
    #'  informing that it's not supported
    ui = function(id) {
      span("Variable type is not supported in teal framework. Please remove this filter and continue")
    }
  ),
  private = list(
    choices = NULL,  # because each class has different choices type
    input_dataname = character(0),
    keep_na = NULL,  # reactiveVal logical()
    na_count = integer(0),
    observers = NULL, # here observers are stored
    selected = NULL,  # because it holds reactiveVal and each class has different choices type
    varname = character(0),
    varlabel = character(0),
    extract_type = logical(0),

    #' description
    #' Adds `is.na(varname)` before existing condition calls if `keep_na` is selected
    #' return (`call`)
    add_keep_na_call = function(filter_call) {
      if (isTRUE(self$get_keep_na())) {
        call(
          "|",
          call("is.na", private$get_varname_prefixed()),
          filter_call
        )
      } else {
        filter_call
      }
    },

    #' description
    #' Prefixed (or not) variable
    #'
    #' Return variable name needed to condition call.
    #' If `isTRUE(private$use_dataset)` variable is prefixed by
    #' dataname to be evaluated as extracted object, for example
    #' `data$var`
    #' return (`name` or `call`)
    get_varname_prefixed = function() {
      if (isTRUE(private$extract_type == "list")) {
        utils.nest::call_extract_list(private$input_dataname, private$varname)
      } else if (isTRUE(private$extract_type == "matrix")) {
        utils.nest::call_extract_matrix(dataname = private$input_dataname, column = as.character(private$varname))
      } else {
        private$varname
      }
    },

    #' Print the state in a nice format
    #'
    #' The `keep_na` and `keep_inf` is not printed.
    #'
    log_state = function() {
      NULL
    },

    #' Sets `keep_na` field according to observed `input$keep_na`
    #' If `keep_na = TRUE` `is.na(varname)` is added to the returned call.
    #' Otherwise returned call excludes `NA` when executed.
    observe_keep_na = function(input) {
      private$observers$keep_na <- observeEvent(
        ignoreNULL = FALSE, # ignoreNULL: we don't want to ignore NULL when nothing is selected in the `selectInput`,
        ignoreInit = TRUE, # ignoreInit: should not matter because we set the UI with the desired initial state
        eventExpr = input$keep_na,
        handlerExpr = {
          self$set_keep_na(if_null(input$keep_na, FALSE))
          private$log_state()
        }
      )
      invisible(NULL)
    },
    #' Set choices
    #'
    #' Set choices is supposed to be executed once in the constructor
    #' to define set/range which selection is made from.
    #' parameter choices (`vector`)\cr
    #'  class of the vector depends on the `FilterState` class.
    #' return a `NULL`
    set_choices = function(choices) {
      private$choices <- choices
      invisible(NULL)
    },

    # Checks if the selection is valid in terms of class and length.
    # It should not return anything but throw an error if selection
    # has a wrong class or is outside of possible choices
    validate_selection = function(value) {
      invisible(NULL)
    },

    # Filters out erroneous values from an array.
    #'
    # @param values the array of values
    #'
    # @return the array of values without elements, which are outside of
    # the accepted set for this FilterState
    remove_out_of_bound_values = function(values) {
      values
    },

    # Casts an array of values to the type fitting this `FilterState`
    # and validates the elements of the casted array
    # satisfy the requirements of this `FilterState`.
    #'
    # @param values the array of values
    #'
    # @return the casted array
    #'
    # @note throws an error if the casting did not execute successfully.
    cast_and_validate = function(values) {
      values
    }
  )
)

# EmptyFilterState ---------
#' @name EmptyFilterState
#' @title `FilterState` object for empty variable
#' @docType class
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @examples
#' filter_state <- teal:::EmptyFilterState$new(
#'   NA,
#'   varname = "x",
#'   input_dataname = as.name("data"),
#'   extract_type = character(0)
#' )
#' isolate(filter_state$get_call())
#' isolate(filter_state$set_selected(TRUE))
#' isolate(filter_state$set_keep_na(TRUE))
#' isolate(filter_state$get_call())
EmptyFilterState <- R6::R6Class( # nolint
  "EmptyFilterState",
  inherit = FilterState,
  public = list(

    #' @description
    #' Initialize `EmptyFilterState` object
    #' @param x (`vector`)\cr
    #'   values of the variable used in filter
    #' @param varname (`character`, `name`)\cr
    #'   name of the variable
    #' @param varlabel (`character(1)`)\cr
    #'   label of the variable (optional).
    #' @param input_dataname (`name` or `call`)\cr
    #'   name of dataset where `x` is taken from
    #' @param extract_type (`character(0)`, `character(1)`)\cr
    #' whether condition calls should be prefixed by dataname. Possible values:
    #' \itemize{
    #' \item{`character(0)` (default)}{ `varname` in the condition call will not be prefixed}
    #' \item{`"list"`}{ `varname` in the condition call will be returned as `<input_dataname>$<varname>`}
    #' \item{`"matrix"`}{ `varname` in the condition call will be returned as `<input_dataname>[, <varname>]`}
    #' }
    initialize = function(x,
                          varname,
                          varlabel = character(0),
                          input_dataname = NULL,
                          extract_type = character(0)) {
      super$initialize(x, varname, varlabel, input_dataname, extract_type)
      private$set_choices(list())
      self$set_selected(list())

      return(invisible(self))
    },

    #' @description
    #' Returns reproducible condition call for current selection relevant
    #' for selected variable type.
    #' Method is using internal reactive values which makes it reactive
    #' and must be executed in reactive or isolated context.
    get_call = function() {
      filter_call <- if (isTRUE(self$get_keep_na())) {
        call("is.na", private$get_varname_prefixed())
      } else {
        FALSE
      }
    },

    #' @description
    #' UI Module for `EmptyFilterState`.
    #' This UI element contains checkbox input to
    #' filter or keep missing values.
    #' @param id (`character(1)`)\cr
    #'  id of shiny element
    ui = function(id) {
      ns <- NS(id)
      fluidRow(
        div(
          style = "position: relative;",
          div(
            span("Variable contains missing values only"),
            checkboxInput(
              ns("keep_na"),
              label_keep_na_count(private$na_count),
              value = FALSE
            )
          )

        )
      )
    },
    #' @description
    #' Controls selection of `keep_na` checkbox input
    #' @param input (`Shiny`)\cr input object
    #' @param output (`Shiny`)\cr output object
    #' @param session (`Shiny`)\cr session object
    #' @return nothing
    server = function(input, output, session) {
      private$observe_keep_na(input)
      return(NULL)
    },

    #' @description
    #' Set state
    #' @param state (`list`)\cr
    #'  contains fields relevant for a specific class
    #' \itemize{
    #' \item{`keep_na` (`logical`)}{ defines whether to keep or remove `NA` values}
    #' }
    set_state = function(state) {
      if (!is.null(state$selected)) {
        stop(
          sprintf(
            "All values in variable '%s' are `NA`. Unable to apply filter values \n  %s",
            self$get_varname(deparse = TRUE),
            paste(state$selected, collapse = ", ")
          )
        )
      }
      stopifnot(is.list(state) && all(names(state) == "keep_na"))
      if (!is.null(state$keep_na)) {
        self$set_keep_na(state$keep_na)
      }
      invisible(NULL)
    }
  ),
  private = list(
    log_state = function() {
      .log("all elements in", self$get_varname(deparse = TRUE), "are NA")
    }
  )
)

# LogicalFilterState ---------
#' @name LogicalFilterState
#' @title `FilterState` object for logical variable
#' @docType class
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @examples
#' filter_state <- teal:::LogicalFilterState$new(
#'   sample(c(TRUE, FALSE, NA), 10, replace = TRUE),
#'   varname = "x",
#'   input_dataname = as.name("data"),
#'   extract_type = character(0)
#' )
#' isolate(filter_state$get_call())
#'
#' isolate(filter_state$set_selected(TRUE))
#' isolate(filter_state$set_keep_na(TRUE))
#' isolate(filter_state$get_call())
LogicalFilterState <- R6::R6Class( # nolint
  "LogicalFilterState",
  inherit = FilterState,
  public = list(

    #' @description
    #' Initialize a `FilterState` object
    #' @param x (`logical`)\cr
    #'   values of the variable used in filter
    #' @param varname (`character`, `name`)\cr
    #'   label of the variable (optional).
    #' @param varlabel (`character(1)`)\cr
    #'   label of the variable (optional).
    #' @param input_dataname (`name` or `call`)\cr
    #'   name of dataset where `x` is taken from
    #' @param extract_type (`character(0)`, `character(1)`)\cr
    #' whether condition calls should be prefixed by dataname. Possible values:
    #' \itemize{
    #' \item{`character(0)` (default)}{ `varname` in the condition call will not be prefixed}
    #' \item{`"list"`}{ `varname` in the condition call will be returned as `<input_dataname>$<varname>`}
    #' \item{`"matrix"`}{ `varname` in the condition call will be returned as `<input_dataname>[, <varname>]`}
    #' }
    initialize = function(x,
                          varname,
                          varlabel = character(0),
                          input_dataname = NULL,
                          extract_type = character(0)) {
      stopifnot(is.logical(x))
      super$initialize(x, varname, varlabel, input_dataname, extract_type)
      df <- as.factor(x)
      if (length(levels(df)) != 2) {
        if (levels(df) %in% c(TRUE, FALSE)){
          choices_not_included <- c(TRUE, FALSE)[!c(TRUE, FALSE) %in% levels(df)]
          levels(df) <- c(levels(df), choices_not_included)
        }
      }

      tbl <- table(df)

      choices <- as.logical(names(tbl))
      names(choices) <- sprintf("%s (%s)", choices, tbl)

      private$set_choices(as.list(choices))
      self$set_selected(unname(choices)[1])
      private$histogram_data <- data.frame(
        x = names(choices),
        y = as.vector(tbl)
      )

      invisible(self)
    },


    #' @description
    #' Returns reproducible condition call for current selection.
    #' For `LogicalFilterState` it's a `!<varname>` or `<varname>` and optionally
    #' `is.na(<varname>)`
    get_call = function() {
      filter_call <- utils.nest::call_condition_logical(
        varname = private$get_varname_prefixed(),
        choice = self$get_selected()
      )

      filter_call <- private$add_keep_na_call(filter_call)

      filter_call
    },

    #' @description
    #' UI Module for `EmptyFilterState`.
    #' This UI element contains available choices selection and
    #' checkbox whether to keep or not keep the `NA` values.
    #' @param id (`character(1)`)\cr
    #'  id of shiny element
    ui = function(id) {
      ns <- NS(id)
      fluidRow(
        div(
          style = "position: relative;",
          # same overlay as for choices with no more than (default: 5) elements
          div(
            class = "filterPlotOverlayBoxes",
            plotOutput(ns("plot"), height = "100%")
          ),
          radioButtons(
            ns("selection"),
            label = NULL,
            choices = private$choices,
            selected = isolate(self$get_selected()),
            width = "100%"
          )
        ),
        if (private$na_count > 0) {
          checkboxInput(
            ns("keep_na"),
            label_keep_na_count(private$na_count),
            value = isolate(self$get_keep_na())
          )
        } else {
          NULL
        }
      )
    },

    #' @description
    #' Server module
    #'
    #' @param input (`Shiny`)\cr input object
    #' @param output (`Shiny`)\cr output object
    #' @param session (`Shiny`)\cr session object
    #'
    #' @return nothing
    server = function(input, output, session) {
      output$plot <- renderPlot(
        bg = "transparent",
        expr = {
          data <- private$histogram_data
          data$y <- rev(data$y / sum(data$y)) # we have to reverse because the histogram is turned by 90 degrees
          data$x <- seq_len(nrow(data)) # to prevent ggplot reordering columns using the characters in x column
          ggplot2::ggplot(data) +
            # sort factor so that it reflects checkbox order
            ggplot2::aes_string(x = "x", y = "y") +
            ggplot2::geom_col(
              width = 0.95,
              fill = grDevices::rgb(66 / 255, 139 / 255, 202 / 255),
              color = NA,
              alpha = 0.2
            ) +
            ggplot2::coord_flip() +
            ggplot2::theme_void() +
            ggplot2::scale_x_discrete(expand = c(0, 0)) +
            ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, 1))
        }
      )

      private$observers$selection <- observeEvent(
        ignoreNULL = FALSE,
        ignoreInit = TRUE,
        eventExpr = input$selection,
        handlerExpr = {
          selection_state <- input$selection
          self$set_selected(
            value = if_null(
              as.logical(selection_state),
              logical(0)
            )
          )
          private$log_state()
        }
      )

      private$observe_keep_na(input)

      NULL
    },

    #' @description
    #' Sets the selected values of this `LogicalFilterState`.
    #'
    #' @param value (`logical(1)`) the value to set. Must not contain the NA value.
    #'
    #' @returns invisibly `NULL`.
    #'
    #' @note Casts the passed object to `logical` before validating the input
    #' making it possible to pass any object coercible to `logical` to this method.
    #'
    #' @examples
    #' filter <- teal:::LogicalFilterState$new(c(TRUE), varname = "name")
    #' filter$set_selected(TRUE)
    set_selected = function(value) {
      super$set_selected(value)
    }
  ),
  private = list(
    histogram_data = data.frame(),
    log_state = function() {
      .log(
        "State for", self$get_varname(deparse = TRUE),
        "set to:", toString(self$get_selected()),
        "NA:", toString(self$get_keep_na())
      )
    },

    validate_selection = function(value) {
      if (!(is_logical_empty(value) || is_logical_single(value))) {
        stop(
          sprintf(
            "value of the selection for `%s` in `%s` should be a logical scalar (TRUE or FALSE)",
            self$get_varname(deparse = TRUE),
            self$get_dataname(deparse = TRUE)
          )
        )
      }

      pre_msg <- sprintf(
        "dataset '%s', variable '%s': ",
        self$get_dataname(deparse = TRUE),
        self$get_varname(deparse = TRUE)
      )
      check_in_subset(value, private$choices, pre_msg = pre_msg)
    },

    cast_and_validate = function(values) {
      tryCatch({
        values_logical <- as.logical(values)
        if (any(is.na(values_logical))) stop()
      },
      error = function(cond) stop("The array of set values must contain values coercible to logical.")
      )
      values_logical
    }
  )
)

# RangeFilterState ---------
#' @name RangeFilterState
#' @title `FilterState` object for numeric variable
#' @docType class
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @examples
#' filter_state <- teal:::RangeFilterState$new(
#'   c(NA, Inf, seq(1:10)),
#'   varname = "x",
#'   input_dataname = as.name("data"),
#'   extract_type = character(0)
#' )
#' isolate(filter_state$get_call())
#'
#' isolate(filter_state$set_selected(c(3L, 8L)))
#' isolate(filter_state$set_keep_na(TRUE))
#' isolate(filter_state$set_keep_inf(TRUE))
#' isolate(filter_state$get_call())
RangeFilterState <- R6::R6Class( # nolint
  "RangeFilterState",
  inherit = FilterState,
  public = list(

    #' @description
    #' Initialize a `FilterState` object
    #' @param x (`numeric`)\cr
    #'   values of the variable used in filter
    #' @param varname (`character`, `name`)\cr
    #'   name of the variable
    #' @param varlabel (`character(1)`)\cr
    #'   label of the variable (optional).
    #' @param input_dataname (`name` or `call`)\cr
    #'   name of dataset where `x` is taken from
    #' @param extract_type (`character(0)`, `character(1)`)\cr
    #' whether condition calls should be prefixed by dataname. Possible values:
    #' \itemize{
    #' \item{`character(0)` (default)}{ `varname` in the condition call will not be prefixed}
    #' \item{`"list"`}{ `varname` in the condition call will be returned as `<input_dataname>$<varname>`}
    #' \item{`"matrix"`}{ `varname` in the condition call will be returned as `<input_dataname>[, <varname>]`}
    #' }
    initialize = function(x,
                          varname,
                          varlabel = character(0),
                          input_dataname = NULL,
                          extract_type = character(0)) {
      stopifnot(is.numeric(x))
      stopifnot(any(is.finite(x)))

      super$initialize(x, varname, varlabel, input_dataname, extract_type)
      var_range <- range(x, finite = TRUE)

      private$set_choices(var_range)
      self$set_selected(var_range)

      private$histogram_data <- if (sum(is.finite(x)) >= 2) {
        as.data.frame(
          stats::density(x, na.rm = TRUE, n = 100)[c("x", "y")] # 100 bins only
        )
      } else {
        data.frame(x = NA_real_, y = NA_real_)
      }
      private$inf_count <- sum(is.infinite(x))
      private$is_integer <- is.integer(x)
      private$keep_inf <- reactiveVal(value = FALSE)

      return(invisible(self))
    },

    #' @description
    #' Returns reproducible condition call for current selection.
    #' For this class returned call looks like
    #' `<varname> >= <min value> & <varname> <= <max value>` with
    #' optional `is.na(<varname>)` and `is.finite(<varname>)`.
    #' @return (`call`)
    get_call = function() {
      filter_call <- utils.nest::call_condition_range(
        varname = private$get_varname_prefixed(),
        range = self$get_selected()
      )

      filter_call <- private$add_keep_inf_call(filter_call)
      filter_call <- private$add_keep_na_call(filter_call)

      filter_call
    },

    #' @description
    #' Returns current `keep_inf` selection
    #' @return (`logical(1)`)
    get_keep_inf = function() {
      private$keep_inf()
    },

    #' UI Module for `EmptyFilterState`.
    #' This UI element contains two values for `min` and `max`
    #' of the range and two checkboxes whether to keep the `NA` or `Inf`  values.
    #' @param id (`character(1)`)\cr
    #'  id of shiny element
    ui = function(id) {
      ns <- NS(id)
      v_pretty_range <- pretty(private$choices, n = 100)
      v_step <- ifelse(private$is_integer, 1L, v_pretty_range[2] - v_pretty_range[1])
      v_min <- v_pretty_range[1]
      v_max <- v_pretty_range[length(v_pretty_range)]
      fluidRow(
        div(
          class = "filterPlotOverlayRange",
          plotOutput(ns("plot"), height = "100%")
        ),
        optionalSliderInput(
          inputId = ns("selection"),
          label = NULL,
          min = v_min,
          max = v_max,
          value = c(v_min, v_max),
          width = "100%",
          step = v_step
        ),
        if (private$inf_count > 0) {
          checkboxInput(
            ns("keep_inf"),
            sprintf("Keep Inf (%s)", private$inf_count),
            value = isolate(self$get_keep_inf())
          )
        } else {
          NULL
        },
        if (private$na_count > 0) {
          checkboxInput(
            ns("keep_na"),
            label_keep_na_count(private$na_count),
            value = isolate(self$get_keep_inf())
          )
        } else {
          NULL
        }
      )
    },

    #' @description
    #' Server module
    #' @param input (`Shiny`)\cr input object
    #' @param output (`Shiny`)\cr output object
    #' @param session (`Shiny`)\cr session object
    #' @return nothing
    server = function(input, output, session) {
      output$plot <- renderPlot(
        bg = "transparent",
        height = 25,
        expr = {
          ggplot2::ggplot(private$histogram_data) +
            ggplot2::aes_string(x = "x", y = "y") +
            ggplot2::geom_area(
              fill = grDevices::rgb(66 / 255, 139 / 255, 202 / 255),
              color = NA,
              alpha = 0.2) +
            ggplot2::theme_void() +
            ggplot2::scale_y_continuous(expand = c(0, 0)) +
            ggplot2::scale_x_continuous(expand = c(0, 0))
        }
      )

      private$observers$selection <- observeEvent(
        ignoreNULL = FALSE, # ignoreNULL: we don't want to ignore NULL when nothing is selected in the `selectInput`,
        ignoreInit = TRUE, # ignoreInit: should not matter because we set the UI with the desired initial state
        eventExpr = input$selection,
        handlerExpr = {
          # because we extended real range into rounded one we need to apply intersect(range_input, range_real)
          selection_state <- c(max(input$selection[1], private$choices[1]), min(input$selection[2], private$choices[2]))
          if (!setequal(selection_state, self$get_selected())) {
            validate(
              need(
                input$selection[1] <= input$selection[2],
                "Left range boundary should be lower than right"
              )
            )

            self$set_selected(
              value = if_null(
                selection_state,
                numeric(0)
              )
            )
          }
          private$log_state()
        })

      private$observe_keep_na(input)

      private$observers$keep_inf <- observeEvent(
        ignoreNULL = FALSE, # ignoreNULL: we don't want to ignore NULL when nothing is selected in the `selectInput`,
        ignoreInit = TRUE, # ignoreInit: should not matter because we set the UI with the desired initial state
        eventExpr = input$keep_inf,
        handlerExpr = {
          self$set_keep_inf(if_null(input$keep_inf, FALSE))
          private$log_state()
        }
      )
    },

    #' @description
    #' Set if `Inf` should be kept
    #' @param value (`logical(1)`)\cr
    #'  Value(s) which come from the filter selection. Value is set in `server`
    #'  modules after selecting check-box-input in the shiny interface. Values are set to
    #'  `private$keep_inf` which is reactive.
    set_keep_inf = function(value) {
      stopifnot(is_logical_single(value))
      private$keep_inf(value)
    },

    #' @description
    #' Set state
    #' @param state (`list`)\cr
    #'  contains fields relevant for a specific class
    #' \itemize{
    #' \item{`selected`}{ defines initial selection}
    #' \item{`keep_na` (`logical`)}{ defines whether to keep or remove `NA` values}
    #' \item{`keep_inf` (`logical`)}{ defines whether to keep or remove `Inf` values}
    #' }
    set_state = function(state) {
      stopifnot(is.list(state) && all(names(state) %in% c("selected", "keep_na", "keep_inf")))
      if (!is.null(state$keep_inf)) {
        self$set_keep_inf(state$keep_inf)
      }
      super$set_state(state[names(state) %in% c("selected", "keep_na")])
      invisible(NULL)
    },

    #' @description
    #' Sets the selected values of this `RangeFilterState`.
    #'
    #' @param bounds (`numeric(2)`) the two-elements array of the lower and upper bound
    #'   of the selected range. Must not contain NA values.
    #'
    #' @returns invisibly `NULL`
    #'
    #' @note Casts the passed object to `numeric` before validating the input
    #' making it possible to pass any object coercible to `numeric` to this method.
    #'
    #' @examples
    #' filter <- teal:::RangeFilterState$new(c(1, 2, 3, 4), varname = "name")
    #' filter$set_selected(c(2, 3))
    set_selected = function(bounds) {
      super$set_selected(bounds)
    }
  ),
  private = list(
    histogram_data = data.frame(),
    keep_inf = NULL, # because it holds reactiveVal
    inf_count = integer(0),
    is_integer = logical(0),

    #' Adds `is.infinite(varname)` before existing condition calls if keep_inf is selected
    #' returns a call
    add_keep_inf_call = function(filter_call) {
      if (isTRUE(self$get_keep_inf())) {
        call(
          "|",
          call("is.infinite", private$get_varname_prefixed()),
          filter_call
        )
      } else {
        filter_call
      }
    },

    log_state = function() {
      .log(
        "State for", self$get_varname(deparse = TRUE),
        "set to:", paste(self$get_selected(), collapse = " - "),
        "NA:", toString(self$get_keep_na()),
        "Inf:", toString(self$get_keep_inf())
      )
    },

    validate_selection = function(value) {
      if (!is.numeric(value)) {
        stop(
          sprintf(
            "value of the selection for `%s` in `%s` should be a numeric",
            self$get_varname(deparse = TRUE),
            self$get_dataname(deparse = TRUE)
          )
        )
      }
      pre_msg <- sprintf(
        "data '%s', variable '%s': ",
        self$get_dataname(deparse = TRUE),
        self$get_varname(deparse = TRUE)
      )
      check_in_range(value, private$choices, pre_msg = pre_msg)
    },

    cast_and_validate = function(values) {
      tryCatch({
        values <- as.numeric(values)
        if (any(is.na(values))) stop()
      },
        error = function(error) stop("The array of set values must contain values coercible to numeric.")
      )
      if (length(values) != 2) stop("The array of set values must have length two.")
      values
    },

    remove_out_of_bound_values = function(values) {
      if (values[1] < private$choices[1]) {
        warning(paste("Value: ", values[1], "is outside of the possible range."))
        values[1] <- private$choices[1]
      }

      if (values[2] > private$choices[2]) {
        warning(paste("Value: ", values[2], "is outside of the possible range."))
        values[2] <- private$choices[2]
      }
      values
    }
  )
)

# ChoicesFilterState --------
#' @name ChoicesFilterState
#' @title `FilterState` object for factor or character variable
#' @docType class
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @examples
#' filter_state <- teal:::init_filter_state(
#'   c(LETTERS, NA),
#'   varname = "x",
#'   input_dataname = as.name("data"),
#'   extract_type = character(0)
#' )
#' isolate(filter_state$get_call())
#' isolate(filter_state$set_selected("B"))
#' isolate(filter_state$set_keep_na(TRUE))
#' isolate(filter_state$get_call())
ChoicesFilterState <- R6::R6Class( # nolint
  "ChoicesFilterState",
  inherit = FilterState,
  public = list(

    #' @description
    #' Initialize a `FilterState` object
    #' @param x (`character` or `factor`)\cr
    #'   values of the variable used in filter
    #' @param varname (`character`, `name`)\cr
    #'   name of the variable
    #' @param varlabel (`character(1)`)\cr
    #'   label of the variable (optional).
    #' @param input_dataname (`name` or `call`)\cr
    #'   name of dataset where `x` is taken from
    #' @param extract_type (`character(0)`, `character(1)`)\cr
    #' whether condition calls should be prefixed by dataname. Possible values:
    #' \itemize{
    #' \item{`character(0)` (default)}{ `varname` in the condition call will not be prefixed}
    #' \item{`"list"`}{ `varname` in the condition call will be returned as `<input_dataname>$<varname>`}
    #' \item{`"matrix"`}{ `varname` in the condition call will be returned as `<input_dataname>[, <varname>]`}
    #' }
    initialize = function(x,
                          varname,
                          varlabel = character(0),
                          input_dataname = NULL,
                          extract_type = character(0)) {
      stopifnot(
        is.character(x) ||
          is.factor(x) ||
          (length(unique(x[!is.na(x)])) < .threshold_slider_vs_checkboxgroup)
      )
      super$initialize(x, varname, varlabel, input_dataname, extract_type)

      add_counts <- if (!is(x, "factor")) {
        x <- factor(x, levels = as.character(sort(unique(x))))
        TRUE
      } else {
        FALSE
      }
      x <- droplevels(x)
      choices <- levels(x)

      if (add_counts) {
        names(choices) <- sprintf("%s (%s)", choices, tabulate(x))
      }

      private$set_choices(as.list(choices))
      self$set_selected(unname(choices))
      private$histogram_data <- data.frame(
        x = levels(x),
        y = tabulate(x)
      )

      return(invisible(self))
    },

    #' @description
    #' Returns reproducible condition call for current selection.
    #' For this class returned call looks like
    #' `<varname> %in%  c(<values selected>)` with
    #' optional `is.na(<varname>)`.
    #' @return (`call`)
    get_call = function() {
      filter_call <- utils.nest::call_condition_choice(
        varname = private$get_varname_prefixed(),
        choice = self$get_selected()
      )

      filter_call <- private$add_keep_na_call(filter_call)

      filter_call
    },


    #' @description
    #' UI Module for `EmptyFilterState`.
    #' This UI element contains available choices selection and
    #' checkbox whether to keep or not keep the `NA` values.
    #' @param id (`character(1)`)\cr
    #'  id of shiny element
    ui = function(id) {
      ns <- NS(id)
      fluidRow(
        if (length(private$choices) <= .threshold_slider_vs_checkboxgroup) {
          div(
            style = "position: relative;",
            div(
              class = "filterPlotOverlayBoxes",
              plotOutput(ns("plot"), height = "100%")
            ),
            checkboxGroupInput(
              ns("selection"),
              label = NULL,
              choices =  private$choices,
              selected = isolate(self$get_selected()),
              width = "100%"
            )
          )
        } else {
          optionalSelectInput(
            inputId = ns("selection"),
            choices = private$choices,
            selected = isolate(self$get_selected()),
            multiple = TRUE,
            options = shinyWidgets::pickerOptions(
              actionsBox = TRUE,
              liveSearch = (length(private$choices) > 10),
              noneSelectedText = "Select a value"
            )
          )
        },
        if (private$na_count > 0) {
          checkboxInput(
            ns("keep_na"),
            label = label_keep_na_count(private$na_count),
            value = isolate(self$get_keep_na())
          )
        } else {
          NULL
        }
      )
    },

    #' @description
    #' Server module
    #' @param input (`Shiny`)\cr input object
    #' @param output (`Shiny`)\cr output object
    #' @param session (`Shiny`)\cr session object
    #' @return nothing
    server = function(input, output, session) {
      output$plot <- renderPlot(
        bg = "transparent",
        expr = {
          if (length(private$choices) <= .threshold_slider_vs_checkboxgroup) {
            # Proportional
            data <- private$histogram_data
            data$y <- rev(data$y / sum(data$y)) # we have to reverse because the histogram is turned by 90 degrees
            data$x <- seq_len(nrow(data)) # to prevent ggplot reordering columns using the characters in x column
            ggplot2::ggplot(data) +
              # sort factor so that it reflects checkbox order
              ggplot2::aes_string(x = "x", y = "y") +
              ggplot2::geom_col(
                width = 0.95,
                fill = grDevices::rgb(66 / 255, 139 / 255, 202 / 255),
                color = NA,
                alpha = 0.2
              ) +
              ggplot2::coord_flip() +
              ggplot2::theme_void() +
              ggplot2::scale_x_discrete(expand = c(0, 0)) +
              ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, 1))
          }
        }
      )


      private$observers$selection <- observeEvent(
        ignoreNULL = FALSE, # ignoreNULL: we don't want to ignore NULL when nothing is selected in the `selectInput`,
        ignoreInit = TRUE, # ignoreInit: should not matter because we set the UI with the desired initial state
        eventExpr = input$selection,
        handlerExpr = {
          self$set_selected(if_null(input$selection, character(0)))
          private$log_state()
        }
      )
      private$observe_keep_na(input)

      return(NULL)
    },

    #' @description
    #' Set state
    #' @param state (`list`)\cr
    #'  contains fields relevant for a specific class
    #' \itemize{
    #' \item{`selected`}{ defines initial selection}
    #' \item{`keep_na` (`logical`)}{ defines whether to keep or remove `NA` values}
    #' }
    set_state = function(state) {
      if (!is.null(state$selected)) {
        state$selected <- as.character(state$selected)
      }
      super$set_state(state)
      invisible(NULL)
    },

    #' @description
    #' Sets the selected values of this `ChoicesFilterState`.
    #'
    #' @param selection (`character`) the array of the selected choices.
    #'   Must not contain NA values.
    #'
    #' @return invisibly `NULL`
    #'
    #' @note Casts the passed object to `character` before validating the input
    #' making it possible to pass any object coercible to `character` to this method.
    #'
    #' @examples
    #' filter <- teal:::ChoicesFilterState$new(c("a", "b", "c"), varname = "name")
    #' filter$set_selected(c("c", "a"))
    set_selected = function(selection) {
      super$set_selected(selection)
    }
  ),
  private = list(
    histogram_data = data.frame(),
    log_state = function() {
      .log(
        "State for", self$get_varname(deparse = TRUE),
        "set to:",
        if (length(self$get_selected()) > 5) {
          paste0(toString(self$get_selected()[1:5]), ", ...")
        } else {
          toString(self$get_selected())
        },
        "NA:", toString(self$get_keep_na())
      )
    },

    validate_selection = function(value) {
      if (!is.character(value)) {
        stop(
          sprintf(
            "Values of the selection for `%s` in `%s` should be an array of character.",
            self$get_varname(deparse = TRUE),
            self$get_dataname(deparse = TRUE)
          )
        )
      }
      pre_msg <- sprintf(
        "data '%s', variable '%s': ",
        self$get_dataname(deparse = TRUE),
        self$get_varname(deparse = TRUE)
      )
      check_in_subset(value, private$choices, pre_msg = pre_msg)
    },

    cast_and_validate = function(values) {
      tryCatch({
        values <- as.character(values)
        if (any(is.na(values))) stop()
      },
        error = function(error) stop("The array of set values must contain values coercible to character.")
      )
      values
    },

    remove_out_of_bound_values = function(values) {
      in_choices_mask <- values %in% private$choices
      if (length(values[!in_choices_mask]) > 0) {
        warning(paste("Values:", paste(values[!in_choices_mask], collapse = ", "), "are not in choices."))
      }
      values[in_choices_mask]
    }
  )
)

# DateFilterState ---------
#' @name DateFilterState
#' @title `FilterState` object for Date variable
#' @docType class
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @examples
#' filter_state <- teal:::DateFilterState$new(
#'   c(Sys.Date() + seq(1:10), NA),
#'   varname = "x",
#'   input_dataname = as.name("data"),
#'   extract_type = character(0)
#' )
#' isolate(filter_state$get_call())
#'
#' isolate(filter_state$set_selected(c(Sys.Date() + 3L, Sys.Date() + 8L)))
#' isolate(filter_state$set_keep_na(TRUE))
#' isolate(filter_state$get_call())
DateFilterState <- R6::R6Class( # nolint
  "DateFilterState",
  inherit = FilterState,
  public = list(

    #' @description
    #' Initialize a `FilterState` object
    #' @param x (`Date`)\cr
    #'   values of the variable used in filter
    #' @param varname (`character`, `name`)\cr
    #'   name of the variable
    #' @param varlabel (`character(1)`)\cr
    #'   label of the variable (optional).
    #' @param input_dataname (`name` or `call`)\cr
    #'   name of dataset where `x` is taken from
    #' @param extract_type (`character(0)`, `character(1)`)\cr
    #' whether condition calls should be prefixed by dataname. Possible values:
    #' \itemize{
    #' \item{`character(0)` (default)}{ `varname` in the condition call will not be prefixed}
    #' \item{`"list"`}{ `varname` in the condition call will be returned as `<input_dataname>$<varname>`}
    #' \item{`"matrix"`}{ `varname` in the condition call will be returned as `<input_dataname>[, <varname>]`}
    #' }
    initialize = function(x,
                          varname,
                          varlabel = character(0),
                          input_dataname = NULL,
                          extract_type = character(0)) {
      stopifnot(is(x, "Date"))
      super$initialize(x, varname, varlabel, input_dataname, extract_type)

      var_range <- range(x, na.rm = TRUE)
      private$set_choices(var_range)
      self$set_selected(var_range)

      return(invisible(self))
    },

    #' @description
    #' Returns reproducible condition call for current selection.
    #' For this class returned call looks like
    #' `<varname> >= <min value> & <varname> <= <max value>` with
    #' optional `is.na(<varname>)`.
    #' @return (`call`)
    get_call = function() {
      filter_call <- utils.nest::call_condition_range_date(
        varname = private$get_varname_prefixed(),
        range = self$get_selected()
      )

      filter_call <- private$add_keep_na_call(filter_call)

      filter_call
    },

    #' @description
    #' UI Module for `EmptyFilterState`.
    #' This UI element contains two date selections for `min` and `max`
    #' of the range and a checkbox whether to keep the `NA` values.
    #' @param id (`character(1)`)\cr
    #'  id of shiny element
    ui = function(id) {
      ns <- NS(id)
      fluidRow(
        div(
          actionButton(
            inputId = ns("start_date_reset"),
            label = NULL,
            icon = icon("undo fa-xs"),
            style = "float: left; padding: 0; padding-top: 4px; padding-bottom: 5px; width: 10%;"
          ),
          actionButton(
            inputId = ns("end_date_reset"),
            label = NULL,
            icon = icon("undo fa-xs"),
            style = "float: right; padding: 0; padding-top: 4px; padding-bottom: 5px; width: 10%;"
          ),
          div(
            style = "margin: auto; width: 80%;",
            dateRangeInput(
              inputId = ns("selection"),
              label = NULL,
              start = private$choices[1],
              end = private$choices[2],
              min = private$choices[1],
              max = private$choices[2]
            )
          )
        ),
        if (private$na_count > 0) {
          checkboxInput(
            ns("keep_na"),
            label_keep_na_count(private$na_count),
            value = isolate(self$get_keep_na())
          )
        } else {
          NULL
        }
      )
    },

    #' @description
    #' Server module
    #' @param input (`Shiny`)\cr input object
    #' @param output (`Shiny`)\cr output object
    #' @param session (`Shiny`)\cr session object
    #' @return nothing
    server = function(input, output, session) {
      private$observers$selection <- observeEvent(
        ignoreNULL = FALSE, # ignoreNULL: we don't want to ignore NULL when nothing is selected in the `selectInput`,
        ignoreInit = TRUE, # ignoreInit: should not matter because we set the UI with the desired initial state
        eventExpr = input$selection,
        handlerExpr = {
          start_date <- input$selection[1]
          end_date <- input$selection[2]

          self$set_selected(c(start_date, end_date))
          private$log_state()
        }
      )

      private$observe_keep_na(input)

      private$observers$reset1 <- observeEvent(input$start_date_reset, {
        updateDateRangeInput(
          session = session,
          inputId = "selection",
          start = private$choices[1]
        )
      })

      private$observers$reset2 <- observeEvent(input$end_date_reset, {
        updateDateRangeInput(
          session = session,
          inputId = "selection",
          end = private$choices[2]
        )
      })

      return(NULL)
    },

    #' @description
    #' Sets the selected time frame of this `DateFilterState`.
    #'
    #' @param bounds (`Date(2)`) the lower and the upper bound of the selected
    #'   time frame. Must not contain NA values.
    #'
    #' @return invisibly `NULL`.
    #'
    #' @note Casts the passed object to `Date` before validating the input
    #' making it possible to pass any object coercible to `Date` to this method.
    #'
    #' @examples
    #' date <- as.Date("13/09/2021")
    #' filter <- teal:::DateFilterState$new(
    #'   c(date, date + 1, date + 2, date + 3),
    #'   varname = "name"
    #' )
    #' filter$set_selected(c(date + 1, date + 2))
    set_selected = function(bounds) {
      super$set_selected(bounds)
    }
  ),
  private = list(
    log_state = function() {
      .log(
        "State for", self$get_varname(deparse = TRUE),
        "set to:", paste(self$get_selected(), collapse = " - "),
        "NA:", toString(self$get_keep_na())
      )
    },

    validate_selection = function(value) {
      if (!is(value, "Date")) {
        stop(
          sprintf(
            "value of the selection for `%s` in `%s` should be a Date",
            self$get_varname(deparse = TRUE),
            self$get_dataname(deparse = TRUE)
          )
        )
      }
      pre_msg <- sprintf(
        "dataset '%s', variable '%s': ",
        self$get_dataname(deparse = TRUE),
        self$get_varname(deparse = TRUE)
      )
      check_in_range(value, private$choices, pre_msg = pre_msg)
    },

    cast_and_validate = function(values) {
      tryCatch({
        values <- as.Date(values)
        if (any(is.na(values))) stop()
      },
        error = function(error) stop("The array of set values must contain values coercible to Date.")
      )
      if (length(values) != 2) stop("The array of set values must have length two.")
      values
    },

    remove_out_of_bound_values = function(values) {
      if (values[1] < private$choices[1]) {
        warning(paste("Value: ", values[1], "is outside of the possible range."))
        values[1] <- private$choices[1]
      }

      if (values[length(values)] > private$choices[2]) {
        warning(paste("Value: ", values[length(values)], "is outside of the possible range."))
        values[length(values)] <- private$choices[2]
      }
      values
    }
  )
)

# DatetimeFilterState ---------
#' @rdname DatetimeFilterState
#' @title `FilterState` object for `POSIXct` variable
#' @docType class
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @examples
#' filter_state <- teal:::init_filter_state(
#'   c(Sys.time() + seq(0, by = 3600, length.out = 10), NA),
#'   varname = "x",
#'   input_dataname = as.name("data"),
#'   extract_type = character(0)
#' )
#'
#' isolate(filter_state$get_call())
#' isolate(filter_state$set_selected(c(Sys.time() + 3L, Sys.time() + 8L)))
#' isolate(filter_state$set_keep_na(TRUE))
#' isolate(filter_state$get_call())
DatetimeFilterState <- R6::R6Class( # nolint
  "DatetimeFilterState",
  inherit = FilterState,
  public = list(

    #' @description
    #' Initialize a `FilterState` object. This class
    #' has an extra field, `private$timezone`, which is set to `Sys.timezone()` by
    #' default. However, in case when using this module in `teal` app, one needs
    #' timezone of the app user. App user timezone is taken from `session$userData$timezone`
    #' and is set only if object is initialized in `shiny`.
    #' @param x (`POSIXct` or `POSIXlt`)\cr
    #'   values of the variable used in filter
    #' @param varname (`character`, `name`)\cr
    #'   name of the variable
    #' @param varlabel (`character(1)`)\cr
    #'   label of the variable (optional).
    #' @param input_dataname (`name` or `call`)\cr
    #'   name of dataset where `x` is taken from
    #' @param extract_type (`character(0)`, `character(1)`)\cr
    #' whether condition calls should be prefixed by dataname. Possible values:
    #' \itemize{
    #' \item{`character(0)` (default)}{ `varname` in the condition call will not be prefixed}
    #' \item{`"list"`}{ `varname` in the condition call will be returned as `<input_dataname>$<varname>`}
    #' \item{`"matrix"`}{ `varname` in the condition call will be returned as `<input_dataname>[, <varname>]`}
    #' }
    initialize = function(x,
                          varname,
                          varlabel = character(0),
                          input_dataname = NULL,
                          extract_type = character(0)) {
      stopifnot(is(x, "POSIXct") || is(x, "POSIXlt"))
      super$initialize(x, varname, varlabel, input_dataname, extract_type)

      var_range <- range(x, na.rm = TRUE)
      private$set_choices(var_range)
      self$set_selected(var_range)

      if (shiny::isRunning()) {
        session <- getDefaultReactiveDomain()
        if (!is.null(session$userData$timezone)) {
          private$timezone <- session$userData$timezone
        }
      } else if (isTRUE(attr(x, "tz") != "")) {
        private$timezone <- attr(x, "tz")
      }

      return(invisible(self))
    },

    #' @description
    #' Returns reproducible condition call for current selection.
    #' For this class returned call looks like
    #' `<varname> >= as.POSIXct(<min>, tz = <timezone>) & <varname> <= <max>, tz = <timezone>)`
    #' with optional `is.na(<varname>)`.
    get_call = function() {
      filter_call <- utils.nest::call_condition_range_posixct(
        varname = private$get_varname_prefixed(),
        range = self$get_selected(),
        timezone = private$timezone
      )

      filter_call <- private$add_keep_na_call(filter_call)

      filter_call
    },

    #' @description
    #' UI Module for `EmptyFilterState`.
    #' This UI element contains two date-time selections for `min` and `max`
    #' of the range and a checkbox whether to keep the `NA` values.
    #' @param id (`character(1)`)\cr
    #'  id of shiny element
    ui = function(id) {
      ns <- NS(id)
      fluidRow(
        div(
          actionButton(
            inputId = ns("start_date_reset"),
            label = NULL,
            icon = icon("undo fa-xs"),
            style = "float: left; padding: 0; padding-top: 4px; padding-bottom: 5px; width: 10%;"
          ),
          actionButton(
            inputId = ns("end_date_reset"),
            label = NULL,
            icon = icon("undo fa-xs"),
            style = "float: right; padding: 0; padding-top: 4px; padding-bottom: 5px; width: 10%;"
          ),
          div(
            class = "input-daterange input-group",
            style = "margin: auto; width: 80%;",
            div(
              style = "float: left; width: 100%;",
              {
                x <- shinyWidgets::airDatepickerInput(
                  inputId = ns("selection_start"),
                  value = private$choices[1],
                  startView = private$choices[1],
                  timepicker = TRUE,
                  minDate = private$choices[1],
                  maxDate = private$choices[2],
                  update_on = "close",
                  addon = "none",
                  position = "bottom right"
                )
                x$children[[2]]$attribs <- c(x$children[[2]]$attribs, list(class = " input-sm"))
                x
              }
            ),
            span(
              class = "input-group-addon",
              "to",
              title = "Times are displayed in the local timezone and are converted to UTC in the analysis"
            ),
            div(
              style = "float: right; width: 100%;",
              {
                x <- shinyWidgets::airDatepickerInput(
                  inputId = ns("selection_end"),
                  value = private$choices[2],
                  startView = private$choices[2],
                  timepicker = TRUE,
                  minDate = private$choices[1],
                  maxDate = private$choices[2],
                  update_on = "close",
                  addon = "none",
                  position = "bottom right"
                )
                x$children[[2]]$attribs <- c(x$children[[2]]$attribs, list(class = " input-sm"))
                x
              }
            )
          )
        ),
        if (private$na_count > 0) {
          checkboxInput(
            ns("keep_na"),
            label_keep_na_count(private$na_count),
            value = isolate(self$get_keep_na())
          )
        } else {
          NULL
        }
      )
    },

    #' @description
    #' Server module
    #' @param input (`Shiny`)\cr input object
    #' @param output (`Shiny`)\cr output object
    #' @param session (`Shiny`)\cr session object
    #' @return nothing
    server = function(input, output, session) {
      private$observers$selection <- observeEvent(
        ignoreNULL = FALSE, # ignoreNULL: we don't want to ignore NULL when nothing is selected in the `selectInput`,
        ignoreInit = TRUE, # ignoreInit: should not matter because we set the UI with the desired initial state
        eventExpr = {
          input$selection_start
          input$selection_end
        },
        handlerExpr = {
          start_date <- input$selection_start
          end_date <- input$selection_end

          if (start_date < private$choices[1]) {
            start_date <- private$choices[1]
          }

          if (end_date > private$choices[2]) {
            end_date <- private$choices[2]
          }


          self$set_selected(c(start_date, end_date))
          private$log_state()
        }
      )


      private$observe_keep_na(input)

      private$observers$reset1 <- observeEvent(input$start_date_reset, {
        shinyWidgets::updateAirDateInput(
          session = session,
          inputId = "selection_start",
          value = private$choices[1]
        )
      })
      private$observers$reset2 <- observeEvent(input$end_date_reset, {
        shinyWidgets::updateAirDateInput(
          session = session,
          inputId = "selection_end",
          value = private$choices[2]
        )
      })

      return(NULL)
    },

    #' @description
    #' Sets the selected time frame of this `DatetimeFilterState`.
    #'
    #' @param bounds (`POSIX(2)`) the lower and the upper bound of the selected
    #'   time frame. Must not contain NA values.
    #'
    #' @return invisibly `NULL`.
    #'
    #' @note Casts the passed object to `POSIXct` before validating the input
    #' making it possible to pass any object coercible to `POSIXct` to this method.
    #'
    #' @examples
    #' date <- as.POSIXct(1, origin = "01/01/1970")
    #' filter <- teal:::DatetimeFilterState$new(
    #'   c(date, date + 1, date + 2, date + 3),
    #'   varname = "name"
    #' )
    #' filter$set_selected(c(date + 1, date + 2))
    set_selected = function(bounds) {
      super$set_selected(bounds)
    }
  ),
  private = list(
    timezone = Sys.timezone(),

    log_state = function() {
      .log(
        "State for", self$get_varname(deparse = TRUE),
        "set to:", paste(self$get_selected(), collapse = " - "),
        "NA:", toString(self$get_keep_na())
      )
    },

    validate_selection = function(value) {
      if (!(is(value, "POSIXct") || is(value, "POSIXlt"))) {
        stop(
          sprintf(
            "value of the selection for `%s` in `%s` should be a POSIXct or POSIXlt",
            self$get_varname(deparse = TRUE),
            self$get_dataname(deparse = TRUE)
          )
        )
      }

      pre_msg <- sprintf(
        "dataset '%s', variable '%s': ",
        self$get_dataname(deparse = TRUE),
        self$get_varname(deparse = TRUE)
      )
      check_in_range(value, private$choices, pre_msg = pre_msg)
    },

    cast_and_validate = function(values) {
      tryCatch({
        values <- as.POSIXct(values)
        if (any(is.na(values))) stop()
      },
        error = function(error) stop("The array of set values must contain values coercible to POSIX.")
      )
      if (length(values) != 2) stop("The array of set values must have length two.")
      values
    },

    remove_out_of_bound_values = function(values) {
      if (values[1] < private$choices[1]) {
        warning(paste("Value: ", values[1], "is outside of the possible range."))
        values[1] <- private$choices[1]
      }

      if (values[2] > private$choices[2]) {
        warning(paste("Value: ", values[2], "is outside of the possible range."))
        values[2] <- private$choices[2]
      }
      values
    }
  )
)
