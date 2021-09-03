#' Initialize `FilterStates` object
#'
#' Initialize `FilterStates` object
#' @param data (`data.frame`, `MultiAssayExperiment`, `SummarizedExperiment`, `matrix`)\cr
#'   R object which `subset` function is applied on.
#'
#' @param input_dataname (`character(1)` or `name` or `call`)\cr
#'   name of the data used on lhs of the expression
#'   specified to the function argument attached to this `FilterStates`.
#'
#' @param output_dataname (`character(1)` or `name` or `call`)\cr
#'   name of the output data on the lhs of the assignment expression.
#'
#' @param datalabel (`character(0)` or `character(1)`)\cr
#'   text label value.
#'
#' @param ... (optional)
#'   additional arguments for specific classes: keys
#'
#' @examples
#' df <- data.frame(
#'   character = letters,
#'   numeric = seq_along(letters),
#'   date = seq(Sys.Date(), length.out = length(letters), by = "1 day"),
#'   datetime = seq(Sys.time(), length.out = length(letters), by = "33.33 hours")
#' )
#' rf <- teal:::init_filter_states(
#'   data = df,
#'   input_dataname = "DF",
#'   output_dataname = "DF_FILTERED",
#'   varlabels = c(
#'     "character variable", "numeric variable", "date variable", "datetime variable"
#'   )
#' )
#' \dontrun{
#' shinyApp(
#'   ui = fluidPage(
#'     actionButton("clear", span(icon("times"), "Remove all filters")),
#'     rf$ui_add_filter_state(id = "add", data = df),
#'     rf$ui("active"),
#'     verbatimTextOutput("expr"),
#'   ),
#'   server = function(input, output, session) {
#'     callModule(
#'       module = rf$srv_add_filter_state,
#'       id = "add",
#'       data = df
#'     )
#'     output$expr <- renderText({
#'       pdeparse(rf$get_call())
#'     })
#'     observeEvent(
#'       input$clear,
#'       rf$queue_empty()
#'     )
#'   }
#' )
#' }
init_filter_states <- function(data,
                               input_dataname,
                               output_dataname = input_dataname,
                               datalabel = character(0),
                               ...) {
  UseMethod("init_filter_states")
}

#' @export
init_filter_states.data.frame <- function(data, #nolint #nousage
                                          input_dataname,
                                          output_dataname = input_dataname,
                                          datalabel = character(0),
                                          varlabels = character(0),
                                          keys = character(0)) {
  DFFilterStates$new(input_dataname = input_dataname,
                     output_dataname = output_dataname,
                     datalabel = datalabel,
                     varlabels = varlabels,
                     keys = keys)
}

#' @export
init_filter_states.matrix <- function(data, #nolint #nousage
                                      input_dataname,
                                      output_dataname = input_dataname,
                                      datalabel = character(0)) {
  MatrixFilterStates$new(input_dataname = input_dataname,
                         output_dataname = output_dataname,
                         datalabel = datalabel)
}

#' @export
init_filter_states.MultiAssayExperiment <- function(data, #nolint #nousage
                                                    input_dataname,
                                                    output_dataname = input_dataname,
                                                    datalabel = character(0),
                                                    varlabels,
                                                    keys = character(0)) {
  MAEFilterStates$new(input_dataname = input_dataname,
                      output_dataname = output_dataname,
                      datalabel = datalabel,
                      varlabels = varlabels,
                      keys = keys)
}

#' @export
init_filter_states.SummarizedExperiment <- function(data, #nolint #nousage
                                                    input_dataname,
                                                    output_dataname = input_dataname,
                                                    datalabel = character(0)) {
  SEFilterStates$new(input_dataname = input_dataname,
                     output_dataname = output_dataname,
                     datalabel = datalabel)
}


# FilterStates -----
#'
#' @title `FilterStates` R6 class
#'
#' @description
#' Implements \code{\link{ReactiveQueue}} to the `teal` filters.
#' Class manages adding and removing `FilterState` to the reactive
#' queue and returns reproducible R expression relevant to specific
#' `FilterStates` subclass.
#' Currently `data.frame`, `MultiAssayExperiment`,
#' `SummarizedExperiment` and `matrix` are available.
#'
#'
#' @examples
#' library(shiny)
#' filter_states <- teal:::DFFilterStates$new(
#'   input_dataname = "data",
#'   output_dataname = "data_filtered",
#'   varlabels = c(x = "x variable", SEX = "Sex"),
#'   datalabel = character(0),
#'   keys = character(0)
#' )
#' filter_state <- teal:::RangeFilterState$new(
#'   c(NA, Inf, seq(1:10)),
#'   varname = "x",
#'   varlabel = "x variable",
#'   input_dataname = as.name("data"),
#'   extract_type = "list"
#' )
#' isolate(filter_state$set_selected(c(3L, 8L)))
#'
#' filter_states$queue_push(
#'   x = filter_state,
#'   queue_index = 1L,
#'   element_id = "x"
#' )
#' isolate(filter_states$get_call())
FilterStates <- R6::R6Class( # nolint
  classname = "FilterStates",
  public = list(
    #' @description
    #' Initializes this `FilterStates` object.
    #' @param data (`data.frame`, `MultiAssayExperiment`, `SummarizedExperiment`, `matrix`)\cr
    #'   R object which `subset` function is applied on.
    #'
    #' @param input_dataname (`character(1)` or `name` or `call`)\cr
    #'   name of the data used on lhs of the expression
    #'   specified to the function argument attached to this `FilterStates`.
    #'
    #' @param output_dataname (`character(1)` or `name` or `call`)\cr
    #'   name of the output data on the lhs of the assignment expression.
    #'
    #' @param datalabel (`character(0)` or `character(1)`)\cr
    #'   text label value.
    #'
    initialize = function(input_dataname, output_dataname, datalabel) {
      stopifnot(
        is.call(input_dataname) || is.name(input_dataname) || is_character_single(input_dataname)
      )
      stopifnot(
        is.call(output_dataname) || is.name(output_dataname) || is_character_single(output_dataname)
      )
      stopifnot(is_character_vector(datalabel, min = 0, max = 1))

      char_to_name <- function(x) {
        if (is.character(x)) {
          as.name(x)
        } else {
          x
        }
      }

      private$input_dataname <- char_to_name(input_dataname)
      private$output_dataname <- char_to_name(output_dataname)
      private$datalabel <- datalabel
      invisible(self)
    },
    #' @description
    #' Filter call
    #'
    #' Makes a subset function call based on condition calls from `FilterState`
    #' objects selection.
    #' `lhs` of the call is `private$output_dataname` and in `rhs`
    #' `self$get_fun()` with `private$input_dataname` as argument and list of
    #' condition calls from `FilterState`. If input and output data-names
    #' are the same and no filters applied, method returns `NULL` to avoid
    #' no-op call such as `x <- x`.
    #'
    #' @return `call` or `NULL`
    get_call = function() {
      # queue (list) names must be the same as argument of the function
      # for ... list should be unnamed
      queue_list <- private$queue
      filter_items <- lapply(
        X = queue_list,
        function(queue) {
          items <- queue$get()
          calls <- lapply(
            items,
            function(state) {
              state$get_call()
            }
          )
          if (length(calls) > 0) {
            utils.nest::calls_combine_by(
              operator = "&",
              calls = calls
            )
          }
        }
      )
      filter_items <- Filter(
        x = filter_items,
        f = Negate(is.null)
      )

      if (length(filter_items) > 0) {
        # below code translates to call by the names of filter_items
        rhs <- utils.nest::call_with_colon(
          self$get_fun(),
          private$input_dataname,
          unlist_args = filter_items
        )

        substitute(
          env = list(
            lhs = private$output_dataname,
            rhs = rhs
          ),
          expr = lhs <- rhs
        )
      } else if (!identical(private$output_dataname, private$input_dataname)) {
        substitute(
          env = list(
            lhs = private$output_dataname,
            rhs = private$input_dataname
          ),
          expr = lhs <- rhs
        )
      } else {
        # avoid no-op call
        NULL
      }
    },

    #' @description
    #' Gets the name of the function used to filter the data in this FilterStates.
    #'
    #' Get function name used to create filter call. By default it's a
    #' "subset" but can be overridden by child class method.
    #' @return `character(1)` the name of the function
    get_fun = function() {
      "subset"
    },

    #' @description
    #' Remove all FilterState objects from all queues in this FilterStates.
    #' @return NULL
    queue_empty = function() {
      queue_indices <- if (is.null(names(private$queue))) {
        seq_along(private$queue)
      } else {
        names(private$queue)
      }

      lapply(queue_indices, function(queue_index) {
        queue_elements <- names(self$queue_get(queue_index = queue_index))
        lapply(queue_elements, function(element_id) {
          self$queue_remove(queue_index = queue_index, element_id = element_id)
          if (shiny::isRunning()) {
            private$remove_filter_state(queue_index, element_id)
          }
        })
      })

      invisible(NULL)
    },

    #' @description
    #' Returns a list of FilterState objects stored in this FilterStates.
    #' @param queue_index (`character(1)`, `integer(1)`)\cr
    #'   index of the `private$queue` list where `ReactiveQueue` are kept.
    #' @param element_id (`character(1)`)\cr
    #'   name of `ReactiveQueue` element.
    #' @return `list` of `FilterState` objects
    queue_get = function(queue_index, element_id = character(0)) {
      private$validate_queue_exists(queue_index)
      stopifnot(is_empty(element_id) || is_character_single(element_id))

      if (is_empty(element_id)) {
        private$queue[[queue_index]]$get()
      } else {
        private$queue[[queue_index]]$get()[element_id]
      }
    },

    #' @description
    #' Sets `ReactiveQueue` objects.
    #' @param x (`list` of `ReactiveQueue`)\cr
    #'  Must be a list even if single `ReactiveQueue` is set.
    queue_initialize = function(x) {
      stopifnot(is_class_list("ReactiveQueue")(x))
      private$queue <- x
      invisible(NULL)
    },

    #' @description
    #' Adds a new `FilterState` object to this FilterStates
    #' @param x (`FilterState`)
    #' @param queue_index (`character(1)`, `integer(1)`)\cr
    #'   index of the `private$queue` list where `ReactiveQueue` are kept.
    #' @param element_id (`character(1)`)\cr
    #'   name of the `ReactiveQueue` element.
    #' @note throws an exception if the length of `x` does not match the length of
    #'   `element_id`
    queue_push = function(x, queue_index, element_id) {
      private$validate_queue_exists(queue_index)
      stopifnot(is_character_single(element_id))

      states <- if (is.list(x)) {
        x
      } else {
        list(x)
      }
      state <- setNames(states, element_id)
      private$queue[[queue_index]]$push(state)
      invisible(NULL)
    },

    #' @description
    #' Removes a single filter state
    #'
    #' Removes a single filter state with all shiny elements associated
    #' with this state. It removes:\cr
    #' * particular `FilterState` from `private$queue`
    #' * UI card created for this filter
    #' * observers listening selection and remove button
    #' @param queue_index (`character(1)`, `logical(1)`)\cr
    #'   index of the `private$queue` list where `ReactiveQueue` are kept.
    #' @param element_id (`character(1)`)\cr
    #'   name of `ReactiveQueue` element.
    queue_remove = function(queue_index, element_id) {
      private$validate_queue_exists(queue_index)
      stopifnot(is_character_single(element_id))
      .log("removing filter item", element_id, "from queue", queue_index)
      stopifnot(is_character_single(queue_index) || is_numeric_single(queue_index))
      stopifnot(is_character_single(element_id))

      filters <- self$queue_get(queue_index = queue_index, element_id = element_id)
      lapply(filters, function(filter) filter$destroy_observers())
      private$queue[[queue_index]]$remove(filters)
    },

    #' @description
    #' Shiny UI module
    #'
    #' Shiny UI element being a container for `FilterState` elements.
    #' Content of this container is created using `renderUI` in
    #' `server` module
    #' @param id (`character(1)`)\cr
    #'   id of the shiny element
    #' @return shiny.tag
    ui = function(id) {
      ns <- NS(id)
      private$card_id <- ns("cards")
      tags$div(
        id = private$card_id,
        class = "list-group hideable-list-group",
        `data-label` = ifelse(private$datalabel == "", "", (paste0("> ", private$datalabel)))
      )
    },

    #' @description
    #' Set bookmark state
    #'
    #' @param data (`data.frame`)\cr
    #'   data which are supposed to be filtered
    #' @param state (`named list`)\cr
    #'   should contain values which are initial selection in the `FilterState`.
    #'   Names of the `list` element should correspond to the name of the
    #'   column in `data`.
    set_bookmark_state = function(data, state) {
      stop("Pure virtual method.")
    },

    #' @description
    #' Shiny UI module to add filter variable.
    #' @param id (`character(1)`)\cr
    #'  id of shiny module
    #' @param data (`data.frame`, `MultiAssayExperiment`, `SummarizedExperiment`, `matrix`)\cr
    #'  object containing columns to be used as filter variables.
    #' @return shiny.tag
    ui_add_filter_state = function(id, data) {
      div("This object cannot be filtered")
    },

    #' @description
    #' Shiny server module to add filter variable
    #'
    #' @param input (`Shiny`)\cr input object
    #' @param output (`Shiny`)\cr output object
    #' @param session (`Shiny`)\cr session object
    #' @param data (`data.frame`, `MultiAssayExperiment`, `SummarizedExperiment`, `matrix`)\cr
    #'  object containing columns to be used as filter variables.
    #' @return `NULL`
    srv_add_filter_state = function(input, output, session, data) {
      NULL
    }
  ),
  private = list(
    card_id = character(0),
    card_ids = character(0),
    datalabel = character(0),
    input_dataname = NULL,  # because it holds object of class name
    output_dataname = NULL,  # because it holds object of class name,
    ns = NULL, # shiny ns()
    observers = list(), # observers
    queue = NULL, # list of ReactiveQueue(s) initialized by self$queue_initialize

    # Module to add `FilterState` to queue
    #'
    #' This module adds `FilterState` object to queue, inserts
    #' shiny UI to the Active Filter Variables, calls `FilterState` modules and
    #' create observer to remove state
    #' parameter filter_state (`FilterState`)
    #' parameter queue_index (`character(1)`, `logical(1)`)\cr
    #'   index of the `private$queue` list where `ReactiveQueue` are kept.
    #' parameter element_id (`character(1)`)\cr
    #'   name of `ReactiveQueue` element.
    add_filter_state = function(input, output, session, filter_state, queue_index, element_id) {
      stopifnot(is(filter_state, "FilterState"))
      stopifnot(is_character_single(queue_index) || is_integer_single(queue_index))
      stopifnot(is_character_single(element_id))

      self$queue_push(
        x = filter_state,
        queue_index = queue_index,
        element_id = element_id
      )

      card_id <- session$ns("card")
      queue_id <- sprintf("%s-%s", queue_index, element_id)
      private$card_ids[queue_id] <- card_id

      insertUI(
        selector = sprintf("#%s", private$card_id),
        where = "beforeEnd",
        # add span with id to be removable
        ui = div(
          id = card_id,
          class = "list-group-item",
          fluidPage(
            fluidRow(
              column(
                width = 10,
                class = "no-left-right-padding",
                tags$div(
                  tags$span(filter_state$get_varname(),
                    class = "filter_panel_varname"
                  ),
                  if_not_character_empty(
                    filter_state$get_varlabel(),
                    if (tolower(filter_state$get_varname()) != tolower(filter_state$get_varlabel())) {
                      tags$span(filter_state$get_varlabel(),
                        class = "filter_panel_varlabel"
                      )
                    }
                  )
                )
              ),
              column(
                width = 2,
                class = "no-left-right-padding",
                actionLink(
                  session$ns("remove"),
                  label = "",
                  icon = icon("times-circle", lib = "font-awesome"),
                  class = "remove pull-right"
                )
              )
            ),
            filter_state$ui(id = session$ns("content"))
          )
        )
      )

      moduleServer(id = "content", filter_state$server)

      private$observers[[queue_id]] <- observeEvent(
        ignoreInit = TRUE,
        ignoreNULL = TRUE,
        eventExpr = input$remove,
        handlerExpr = {
          self$queue_remove(queue_index, element_id)
          private$remove_filter_state(queue_index, element_id)
        }
      )

      return(invisible(NULL))
    },

    # Remove shiny element. Method can be called from reactive session where
    #' `observeEvent` for remove-filter-state is set and also from `FilteredDataset`
    #' level, where shiny-session-namespace is different. That is why it's important
    #' to remove shiny elements from anywhere. In `add_filter_state` `session$ns(NULL)`
    #' is equivalent to `private$ns(queue_index)`. This means that
    #'
    remove_filter_state = function(queue_index, element_id) {
      queue_id <- sprintf("%s-%s", queue_index, element_id)

      removeUI(
        selector = sprintf("#%s", private$card_ids[queue_id])
      )
      private$card_ids <- private$card_ids[names(private$card_ids) != queue_id]

      private$observers[[queue_id]]$destroy()
      private$observers[[queue_id]] <- NULL
    },

    # Checks if the queue of the given index was initialized in this FilterStates
    # @param queue_index (character or integer)
    validate_queue_exists = function(queue_index) {
      stopifnot(is_character_single(queue_index) || is_numeric_single(queue_index))
      if (
        !(
          is.numeric(queue_index) && all(queue_index <= length(private$queue) && queue_index > 0) ||
          is.character(queue_index) && all(queue_index %in% names(private$queue))
        )
      ) {
        stop(
          paste(
            "ReactiveQueue",
            queue_index,
            "has not been initialized in FilterStates object belonging to the dataset",
            private$datalabel
          )
        )
      }
    },

    # Maps the array of strings to sanitized unique HTML ids.
    # @param keys `character` the array of strings
    # @return `list` the mapping
    map_vars_to_html_ids = function(keys) {
      sanitized_values <- make.unique(gsub("[^[:alnum:]]", perl = TRUE, replacement = "", x = keys))
      sanitized_values <- paste0("var_", sanitized_values)
      stats::setNames(object = sanitized_values, nm = keys)
    }
  )
)

# DFFilterStates -----
DFFilterStates <- R6::R6Class( # nolint
  classname = "DFFilterStates",
  inherit = FilterStates,
  public = list(

    #' Initializes `DFFilterStates` object
    #'
    #' Initializes `DFFilterStates` object by setting `input_dataname`,
    #' `output_dataname` and initializing `ReactiveQueue`. This class contains a
    #' single `ReactiveQueue` with no specified name which means that
    #' when calling the function associated to this class (`dplyr::filter`), a list of
    #' conditions are passed to unnamed arguments (`...`).
    #'
    #' @param input_dataname (`character(1)` or `name` or `call`)\cr
    #'   name of the data used on lhs of the expression
    #'   specified to the function argument attached to this `FilterStates`.
    #'
    #' @param output_dataname (`character(1)` or `name` or `call`)\cr
    #'   name of the output data on the lhs of the assignment expression.
    #'
    #' @param datalabel (`character(0)` or `character(1)`)\cr
    #'   text label value.
    #'
    #' @param varlabels (`character`)\cr
    #'   labels of the variables used in this object
    #'
    #' @param keys (`character`)\cr
    #'   key columns names
    initialize = function(input_dataname, output_dataname, datalabel, varlabels, keys) {
      super$initialize(input_dataname, output_dataname, datalabel)
      private$varlabels <- varlabels
      private$keys <- keys

      self$queue_initialize(
        list(
          ReactiveQueue$new()
        )
      )
    },

    #' Get function name
    #'
    #' Get function name used to create filter call.
    #' For `DFFilterStates` `dplyr::filter` is used
    #' @return `character(1)`
    get_fun = function() {
      return("dplyr::filter")
    },

    #' @description
    #' Set bookmark state
    #'
    #' @param data (`data.frame`)\cr
    #'   data which are supposed to be filtered
    #' @param state (`named list`)\cr
    #'   should contain values which are initial selection in the `FilterState`.
    #'   Names of the `list` element should correspond to the name of the
    #'   column in `data`.
    set_bookmark_state = function(data, state) {
      stopifnot(is.data.frame(data))
      stopifnot(all(names(state) %in% names(data)))

      html_id_mapping <- private$map_vars_to_html_ids(get_filterable_varnames(colnames(data)))
      for (varname in names(state)) {
        value <- state[[varname]]
        fstate <- init_filter_state(
          data[[varname]],
          varname = as.name(varname),
          varlabel = private$get_varlabels(varname),
          input_dataname = private$input_dataname
        )
        fstate$set_selected(value = value)

        if (shiny::isRunning()) {
          id <- html_id_mapping[[varname]]
          callModule(
            module = private$add_filter_state,
            id = id,
            filter_state = fstate,
            queue_index = 1L,
            element_id = varname
          )
        } else {
          self$queue_push(
            x = fstate,
            queue_index = 1L,
            element_id = varname
          )
        }
      }
    },


    #' @description
    #' Shiny UI module to add filter variable
    #' @param id (`character(1)`)\cr
    #'  id of shiny module
    #' @param data (`data.frame`)\cr
    #'  object which columns are used to choose filter variables.
    #' @return shiny.tag
    ui_add_filter_state = function(id, data) {
      stopifnot(is_character_single(id))
      stopifnot(is.data.frame(data))

      ns <- NS(id)

      if (nrow(data) == 0) {
        div(sprintf("data '%s' has zero rows", deparse(private$input_dataname)))
      } else {
        div(
          optionalSelectInput(
            ns("var_to_add"),
            choices = NULL,
            options = shinyWidgets::pickerOptions(
              liveSearch = TRUE,
              noneSelectedText = "Select variable to filter"
            )
          )
        )
      }
    },

    #' @description
    #' Shiny server module to add filter variable
    #'
    #' Module controls available choices to select as a filter variable.
    #' Selected filter variable is being removed from available choices.
    #' Removed filter variable gets back to available choices.
    #'
    #' @param input (`Shiny`)\cr input object
    #' @param output (`Shiny`)\cr output object
    #' @param session (`Shiny`)\cr session object
    #' @param data (`data.frame`)\cr
    #'  object which columns are used to choose filter variables.
    #' @return `NULL`
    srv_add_filter_state = function(input, output, session, data) {
      stopifnot(is.data.frame(data))

      active_filter_vars <- reactive({
        vapply(
          X = self$queue_get(queue_index = 1L),
          FUN.VALUE = character(1),
          function(x) x$get_varname(deparse = TRUE)
        )
      })

      # available choices to display
      avail_column_choices <- reactive({
        choices <- setdiff(
          get_filterable_varnames(data = data),
          active_filter_vars()
        )

        data_choices_labeled(
          data = data,
          choices = choices,
          varlabels = private$get_varlabels(choices),
          keys = private$keys
        )
      })
      observeEvent(
        avail_column_choices(),
        ignoreNULL = TRUE,
        handlerExpr = {
          .log("updating column choices to add filter variables for", deparse(private$input_dataname))
          if (is.null(avail_column_choices())) {
            shinyjs::hide("var_to_add")
          } else {
            shinyjs::show("var_to_add")
          }
          updateOptionalSelectInput(
            session,
            "var_to_add",
            choices = avail_column_choices()
          )
        }
      )

      html_id_mapping <- private$map_vars_to_html_ids(get_filterable_varnames(colnames(data)))
      observeEvent(
        eventExpr = input$var_to_add,
        handlerExpr = {
          id <- html_id_mapping[[input$var_to_add]]
          callModule(
            private$add_filter_state,
            id = id,
            filter_state = init_filter_state(
              data[[input$var_to_add]],
              varname = as.name(input$var_to_add),
              varlabel = private$get_varlabels(input$var_to_add),
              input_dataname = private$input_dataname
            ),
            queue_index = 1L,
            element_id = input$var_to_add
          )

        }
      )

      NULL
    }
  ),
  private = list(
    varlabels = character(0),
    keys = character(0),
    #' description
    #' Get label of specific variable. In case when variable label is missing
    #' name of the variable is returned.
    #' parameter variable (`character(1)`)\cr
    #'  name of the variable for which label should be returned
    #' return `character(1)`
    get_varlabels = function(variables = character(0)) {
      stopifnot(is.character(variables))
      if (identical(variables, character(0))) {
        private$varlabels

      } else {
        varlabels <- private$varlabels[variables]
        varlabels[is.na(varlabels) || varlabels == ""] <- variables[
          is.na(varlabels) || varlabels == ""
          ]
        varlabels
      }
    }
  )
)


# MAEFilterStates -----
MAEFilterStates <- R6::R6Class( # nolint
  classname = "MAEFilterStates",
  inherit = FilterStates,
  public = list(

    #' Initialize `MAEFilterStates` object
    #'
    #' Initialize `MAEFilterStates` object
    #' @param data (`MultiAssayExperiment`)\cr
    #'   R object which `SummarizedExperiment::summarizeByColData` function is applied on.
    #'
    #' @param input_dataname (`character(1)` or `name` or `call`)\cr
    #'   name of the data used on lhs of the expression
    #'   specified to the function argument attached to this `FilterStates`.
    #'
    #' @param output_dataname (`character(1)` or `name` or `call`)\cr
    #'   name of the output data on the lhs of the assignment expression.
    #'
    #' @param datalabel (`character(0)` or `character(1)`)\cr
    #'   text label value.
    #'
    #' @param varlabels (`character`)\cr
    #'   labels of the variables used in this object
    #'
    #' @param keys (`character`)\cr
    #'   key columns names
    initialize = function(input_dataname, output_dataname, datalabel, varlabels, keys) {
      super$initialize(input_dataname, output_dataname, datalabel)
      private$keys <- keys
      private$varlabels <- varlabels

      self$queue_initialize(
        list(
          y = ReactiveQueue$new()
        )
      )
      return(invisible(self))
    },

    #' Get function name
    #'
    #' Get function name used to create filter call.
    #' For `MAEFilterStates`
    #' `MultiAssayExperiment::subsetByColData` is used.
    #' @return `character(1)`
    get_fun = function() {
      return("MultiAssayExperiment::subsetByColData")
    },

    #' @description
    #' Set bookmark state
    #'
    #' @param data (`MultiAssayExperiment`)\cr
    #'   data which are supposed to be filtered
    #' @param state (`named list`)\cr
    #'   should contain values which are initial selection in the `FilterState`.
    #'   Names of the `list` element should correspond to the name of the
    #'   column in `colData(data)`
    set_bookmark_state = function(data, state) {
      stopifnot(is(data, "MultiAssayExperiment"))
      stopifnot(
        all(names(state) %in% names(colData(data)))
      )

      html_id_mapping <- private$map_vars_to_html_ids(get_filterable_varnames(SummarizedExperiment::colData(data)))
      for (varname in names(state)) {
        value <- state[[varname]]
        fstate <- init_filter_state(
          SummarizedExperiment::colData(data)[[varname]],
          varname = as.name(varname),
          varlabel = private$get_varlabels(varname),
          input_dataname = private$input_dataname,
          extract_type = "list"
        )
        fstate$set_selected(value = value)

        id <- html_id_mapping[[varname]]
        callModule(
          private$add_filter_state,
          id = id,
          filter_state = fstate,
          queue_index = "y",
          element_id = varname
        )

      }
    },

    #' @description
    #' Shiny UI module to add filter variable
    #' @param id (`character(1)`)\cr
    #'  id of shiny module
    #' @param data (`MultiAssayExperiment`)\cr
    #'  object containing `colData` which columns are used to be used
    #'  to choose filter variables.
    #' @return shiny.tag
    ui_add_filter_state = function(id, data) {
      stopifnot(is_character_single(id))
      stopifnot(is(data, "MultiAssayExperiment"))

      ns <- NS(id)

      if (nrow(SummarizedExperiment::colData(data)) == 0) {
        div(sprintf("colData of '%s' has zero rows", deparse(private$input_dataname)))
      } else {
        optionalSelectInput(
          ns("var_to_add"),
          choices = NULL,
          options = shinyWidgets::pickerOptions(
            liveSearch = TRUE,
            noneSelectedText = "Select subject variable"
          )
        )
      }
    },

    #' @description
    #' Shiny server module to add filter variable.
    #'
    #' Module controls available choices to select as a filter variable.
    #' Selected filter variable is being removed from available choices.
    #' Removed filter variable gets back to available choices.
    #'
    #' @param input (`Shiny`)\cr input object
    #' @param output (`Shiny`)\cr output object
    #' @param session (`Shiny`)\cr session object
    #' @param data (`MultiAssayExperiment`)\cr
    #'  object containing `colData` which columns are used to be used
    #'  to choose filter variables.
    #'  in `optionalSelectInput`
    #' @return `NULL`
    srv_add_filter_state = function(input, output, session, data) {
      stopifnot(is(data, "MultiAssayExperiment"))

      active_filter_vars <- reactive({
        vapply(
          X = self$queue_get(queue_index = "y"),
          FUN.VALUE = character(1),
          function(x) x$get_varname(deparse = TRUE)
        )
      })

      # available choices to display
      avail_column_choices <- reactive({
        choices <- setdiff(
          get_filterable_varnames(data = SummarizedExperiment::colData(data)),
          active_filter_vars()
        )
        data_choices_labeled(data = SummarizedExperiment::colData(data),
                             choices = choices,
                             varlabels = private$get_varlabels(choices),
                             keys = private$keys)
      })
      observeEvent(
        avail_column_choices(),
        ignoreNULL = TRUE,
        handlerExpr = {
          .log("updating column choices to add filter variables for", deparse(private$input_dataname))
          if (is.null(avail_column_choices())) {
            shinyjs::hide("var_to_add")
          } else {
            shinyjs::show("var_to_add")
          }
          updateOptionalSelectInput(
            session,
            "var_to_add",
            choices = avail_column_choices()
          )
        }
      )

      html_id_mapping <- private$map_vars_to_html_ids(get_filterable_varnames(SummarizedExperiment::colData(data)))
      observeEvent(
        eventExpr = input$var_to_add,
        handlerExpr = {
          id <- html_id_mapping[[input$var_to_add]]
          callModule(
            private$add_filter_state,
            id = id,
            filter_state = init_filter_state(
              SummarizedExperiment::colData(data)[[input$var_to_add]],
              varname = as.name(input$var_to_add),
              varlabel = private$get_varlabels(input$var_to_add),
              input_dataname = private$input_dataname,
              extract_type = "list"
            ),
            queue_index = "y",
            element_id = input$var_to_add
          )

        }
      )

      return(NULL)
    }
  ),
  private = list(
    varlabels = character(0),
    keys = character(0),
    #' description
    #' Get label of specific variable. In case when variable label is missing
    #' name of the variable is returned.
    #' parameter variable (`character(1)`)\cr
    #'  name of the variable for which label should be returned
    #' return `character(1)`
    get_varlabels = function(variables = character(0)) {
      stopifnot(is.character(variables))
      if (identical(variables, character(0))) {
        private$varlabels

      } else {
        varlabels <- private$varlabels[variables]
        varlabels[is.na(varlabels) || varlabels == ""] <- variables[
          is.na(varlabels) || varlabels == ""
          ]
        varlabels
      }

    }
  )
)

# SEFilterStates -----
SEFilterStates <- R6::R6Class( # nolint
  classname = "SEFilterStates",
  inherit = FilterStates,
  public = list(

    #' Initialize `SEFilterStates` object
    #'
    #' Initialize `SEFilterStates` object
    #' @param data (`SummarizedExperiment`)\cr
    #'   R object which `subset` function is applied on.
    #'
    #' @param input_dataname (`character(1)` or `name` or `call`)\cr
    #'   name of the data used on lhs of the expression
    #'   specified to the function argument attached to this `FilterStates`.
    #'
    #' @param output_dataname (`character(1)` or `name` or `call`)\cr
    #'   name of the output data on the lhs of the assignment expression.
    #'
    #' @param datalabel (`character(0)` or `character(1)`)\cr
    #'   text label value.
    initialize = function(input_dataname, output_dataname, datalabel) {
      super$initialize(input_dataname, output_dataname, datalabel)
      self$queue_initialize(
        list(
          subset = ReactiveQueue$new(),
          select = ReactiveQueue$new()
        )
      )
    },

    #' @description
    #' Set bookmark state
    #'
    #' @param data (`SummarizedExperiment`)\cr
    #'   data which are supposed to be filtered
    #' @param state (`named list`)\cr
    #'   This list should contain `subset` and `select` element where
    #'   each should be a named list containing values as a selection in the `FilterState`.
    #'   Names of each the `list` element in `subset` and `select` should correspond to
    #'   the name of the column in `rowData(data)` and `colData(data)`.
    set_bookmark_state = function(data, state) {
      stopifnot(is(data, "SummarizedExperiment"))
      stopifnot(
        all(names(state) %in% c("subset", "select")),
        is.null(state$subset) || all(names(state$subset) %in% names(rowData(data))),
        is.null(state$select) || all(names(state$select) %in% names(colData(data)))
      )

      row_html_mapping <- private$map_vars_to_html_ids(get_filterable_varnames(row_data))
      row_html_mapping <- setNames(object = paste0("rowData_", row_html_mapping), nm = names(row_html_mapping))
      for (varname in names(state$subset)) {
        value <- state$subset[[varname]]
        fstate <- init_filter_state(
          SummarizedExperiment::rowData(data)[[varname]],
          varname = as.name(varname),
          input_dataname = private$input_dataname
        )
        fstate$set_selected(value = value)

        id <- row_html_mapping[[varname]]
        callModule(
          private$add_filter_state,
          id = id,
          filter_state = fstate,
          queue_index = "subset",
          element_id = varname
        )
      }


      col_html_mapping <- private$map_vars_to_html_ids(get_filterable_varnames(col_data))
      col_html_mapping <- setNames(object = paste0("colData_", col_html_mapping), nm = names(col_html_mapping))
      for (varname in names(state$select)) {
        value <- state$select[[varname]]
        fstate <- init_filter_state(
          SummarizedExperiment::colData(data)[[varname]],
          varname = as.name(varname)
        )
        fstate$set_selected(value = value)

        id <- col_html_mapping[[varname]]
        callModule(
          private$add_filter_state,
          id = id,
          filter_state = fstate,
          queue_index = "select",
          element_id = varname
        )
      }

      return(NULL)
    },

    #' @description
    #' Shiny UI module to add filter variable
    #' @param id (`character(1)`)\cr
    #'  id of shiny module
    #' @param data (`SummarizedExperiment`)\cr
    #'  object containing `colData` and `rowData` which columns
    #'  are used to choose filter variables. Column selection from `colData`
    #'  and `rowData` are separate shiny entities.
    #' @return shiny.tag
    ui_add_filter_state = function(id, data) {
      stopifnot(is_character_single(id))
      stopifnot(is(data, "SummarizedExperiment"))

      ns <- NS(id)

      row_input <- if (nrow(SummarizedExperiment::rowData(data)) == 0) {
        div(sprintf("rowData of '%s' has zero rows", deparse(private$input_dataname)))
      } else {
        optionalSelectInput(
          ns("row_to_add"),
          choices = NULL,
          options = shinyWidgets::pickerOptions(
            liveSearch = TRUE,
            noneSelectedText = "Select gene variable"
          )
        )
      }

      col_input <- if (nrow(SummarizedExperiment::colData(data)) == 0) {
        span(sprintf("colData of '%s' has zero rows", deparse(private$input_dataname)))
      } else {
        optionalSelectInput(
          ns("col_to_add"),
          choices = NULL,
          options = shinyWidgets::pickerOptions(
            liveSearch = TRUE,
            noneSelectedText = "Select sample variable"
          )
        )

      }

      div(
        row_input,
        col_input
      )
    },

    #' @description
    #' Shiny server module to add filter variable
    #'
    #' Module controls available choices to select as a filter variable.
    #' Selected filter variable is being removed from available choices.
    #' Removed filter variable gets back to available choices.
    #' This module unlike other `FilterStates` classes manages two
    #' sets of filter variables - one for `colData` and another for
    #' `rowData`.
    #'
    #' @param input (`Shiny`)\cr input object
    #' @param output (`Shiny`)\cr output object
    #' @param session (`Shiny`)\cr session object
    #' @param data (`SummarizedExperiment`)\cr
    #'  object containing `colData` and `rowData` which columns
    #'  are used to choose filter variables. Column selection from `colData`
    #'  and `rowData` are separate shiny entities.
    #' @return `NULL`
    srv_add_filter_state = function(input, output, session, data) {
      stopifnot(is(data, "SummarizedExperiment"))

      active_filter_col_vars <- reactive({
        vapply(
          X = self$queue_get(queue_index = "select"),
          FUN.VALUE = character(1),
          function(x) x$get_varname(deparse = TRUE)
        )
      })
      active_filter_row_vars <- reactive({
        vapply(
          X = self$queue_get(queue_index = "subset"),
          FUN.VALUE = character(1),
          function(x) x$get_varname(deparse = TRUE)
        )
      })

      row_data <- SummarizedExperiment::rowData(data)
      col_data <- SummarizedExperiment::colData(data)

      # available choices to display
      avail_row_data_choices <- reactive({
        choices <- setdiff(
          get_filterable_varnames(data = row_data),
          active_filter_row_vars()
        )

        data_choices_labeled(data = row_data,
                             choices = choices,
                             varlabels = character(0),
                             keys = NULL)

      })
      avail_col_data_choices <- reactive({
        choices <- setdiff(
          get_filterable_varnames(data = col_data),
          active_filter_col_vars()
        )

        data_choices_labeled(data = col_data,
                             choices = choices,
                             varlabels = character(0),
                             keys = NULL)
      })


      observeEvent(
        avail_row_data_choices(),
        ignoreNULL = TRUE,
        handlerExpr = {
          .log("updating rowData choices to add filter variables for", deparse(private$input_dataname))
          if (is.null(avail_row_data_choices())) {
            shinyjs::hide("row_to_add")
          } else {
            shinyjs::show("row_to_add")
          }
          updateOptionalSelectInput(
            session,
            "row_to_add",
            choices = avail_row_data_choices()
          )
        }
      )

      observeEvent(
        avail_col_data_choices(),
        ignoreNULL = TRUE,
        handlerExpr = {
          .log("updating colData choices to add filter variables for", deparse(private$input_dataname))
          if (is.null(avail_col_data_choices())) {
            shinyjs::hide("col_to_add")
          } else {
            shinyjs::show("col_to_add")
          }
          updateOptionalSelectInput(
            session,
            "col_to_add",
            choices = avail_col_data_choices()
          )
        }
      )

      col_html_mapping <- private$map_vars_to_html_ids(get_filterable_varnames(col_data))
      col_html_mapping <- setNames(object = paste0("colData_", col_html_mapping), nm = names(col_html_mapping))
      observeEvent(
        eventExpr = input$col_to_add,
        handlerExpr = {
          id <- col_html_mapping[[input$col_to_add]]
          callModule(
            private$add_filter_state,
            id = id,
            filter_state = init_filter_state(
              SummarizedExperiment::colData(data)[[input$col_to_add]],
              varname = as.name(input$col_to_add),
              input_dataname = private$input_dataname
            ),
            queue_index = "select",
            element_id = input$col_to_add
          )
        }
      )

      row_html_mapping <- private$map_vars_to_html_ids(get_filterable_varnames(row_data))
      row_html_mapping <- setNames(object = paste0("rowData_", row_html_mapping), nm = names(row_html_mapping))
      observeEvent(
        eventExpr = input$row_to_add,
        handlerExpr = {
          id <- row_html_mapping[[input$row_to_add]]
          callModule(
            private$add_filter_state,
            id = id,
            filter_state = init_filter_state(
              SummarizedExperiment::rowData(data)[[input$row_to_add]],
              varname = as.name(input$row_to_add),
              input_dataname = private$input_dataname
            ),
            queue_index = "subset",
            element_id = input$row_to_add
          )
        }
      )

      NULL
    }
  )
)

# MatrixFilterStates -----
MatrixFilterStates <- R6::R6Class( # nolint
  classname = "MatrixFilterStates",
  inherit = FilterStates,
  public = list(

    #' Initialize `MatrixFilterStates` object
    #'
    #' Initialize `MatrixFilterStates` object
    #' @param data (`matrix`)\cr
    #'   R object which `subset` function is applied on.
    #'
    #' @param input_dataname (`character(1)` or `name` or `call`)\cr
    #'   name of the data used on lhs of the expression
    #'   specified to the function argument attached to this `FilterStates`.
    #'
    #' @param output_dataname (`character(1)` or `name` or `call`)\cr
    #'   name of the output data on the lhs of the assignment expression.
    #'
    #' @param datalabel (`character(0)` or `character(1)`)\cr
    #'   text label value.
    initialize = function(input_dataname, output_dataname, datalabel) {
      super$initialize(input_dataname, output_dataname, datalabel)
      self$queue_initialize(
        list(
          subset = ReactiveQueue$new()
        )
      )
    },

    #' @description
    #' Set bookmark state
    #'
    #' @param data (`matrix`)\cr
    #'   data which are supposed to be filtered
    #' @param state (`named list`)\cr
    #'   should contain values which are initial selection in the `FilterState`.
    #'   Names of the `list` element should correspond to the name of the
    #'   column in `data`.
    set_bookmark_state = function(data, state) {
      stopifnot(is(data, "matrix"))
      stopifnot(
        all(names(state) %in% names(colData(data)))
      )

      for (varname in names(state)) {
        value <- state[[varname]]
        fstate <- init_filter_state(
          data[[varname]],
          varname = as.name(varname),
          varlabel = private$get_varlabels(varname),
          input_dataname = private$input_dataname,
          extract_type = "matrix"
        )
        fstate$set_selected(value = value)

        id <- digest::digest(sprintf("%s_%s", "subset", varname), algo = "md5")
        callModule(
          private$add_filter_state,
          id = id,
          filter_state = fstate,
          queue_index = "subset",
          element_id = varname
        )
      }
    },

    #' @description
    #' Shiny UI module to add filter variable
    #' @param id (`character(1)`)\cr
    #'  id of shiny module
    #' @param data (`matrix`)\cr
    #'  object which columns are used to choose filter variables.
    #' @return shiny.tag
    ui_add_filter_state = function(id, data) {
      stopifnot(is_character_single(id))
      stopifnot(is.matrix(data))

      ns <- NS(id)

      if (nrow(data) == 0) {
        div(sprintf("data '%s' has zero rows", deparse(private$input_dataname)))
      } else {
        optionalSelectInput(
          ns("var_to_add"),
          choices = NULL,
          options = shinyWidgets::pickerOptions(
            liveSearch = TRUE,
            noneSelectedText = "Select variable to filter"
          )
        )
      }
    },

    #' @description
    #' Shiny server module to add filter variable
    #'
    #' Module controls available choices to select as a filter variable.
    #' Selected filter variable is being removed from available choices.
    #' Removed filter variable gets back to available choices.
    #'
    #' @param input (`Shiny`)\cr input object
    #' @param output (`Shiny`)\cr output object
    #' @param session (`Shiny`)\cr session object
    #' @param data (`matrix`)\cr
    #'  object which columns are used to choose filter variables.
    #' @param varnames (`character`)\cr
    #' @param varlabels (`character`)\cr
    #'  labels of the variables displayed under name of the variable in UI element
    #' @param keys (`character`)\cr
    #'  names of the primary keys to distinguish from other variables
    #'  in `optionalSelectInput`
    #' @return `NULL`
    srv_add_filter_state = function(input, output, session, data) {
      stopifnot(is.matrix(data))

      active_filter_vars <- reactive({
        vapply(
          X = self$queue_get(queue_index = "subset"),
          FUN.VALUE = character(1),
          function(x) x$get_varname(deparse = TRUE)
        )
      })

      # available choices to display
      avail_column_choices <- reactive({
        choices <- setdiff(
          get_filterable_varnames(data = data),
          active_filter_vars()
        )
        data_choices_labeled(data = data,
                             choices = choices,
                             varlabels = character(0),
                             keys = NULL)
      })
      observeEvent(
        avail_column_choices(),
        ignoreNULL = TRUE,
        handlerExpr = {
          .log("updating column choices to add filter variables for", deparse(private$input_dataname))
          if (length(avail_column_choices()) < 0) {
            shinyjs::hide("var_to_add")
          } else {
            shinyjs::show("var_to_add")
          }
          updateOptionalSelectInput(
            session,
            "var_to_add",
            choices = avail_column_choices()
          )
        }
      )

      html_id_mapping <- private$map_vars_to_html_ids(get_filterable_varnames(data))
      observeEvent(
        eventExpr = input$var_to_add,
        handlerExpr = {
          id <- html_id_mapping[[input$var_to_add]]
          callModule(
            private$add_filter_state,
            id = id,
            filter_state = init_filter_state(
              subset(data, select = input$var_to_add),
              varname = as.name(input$var_to_add),
              varlabel = private$get_varlabel(input$var_to_add),
              input_dataname = private$input_dataname,
              extract_type = "matrix"
            ),
            queue_index = "subset",
            element_id = input$var_to_add
          )
        }
      )

      NULL
    }
  )
)

# utils -----
.filterable_class <- c("logical", "integer", "numeric", "factor", "character", "Date", "POSIXct", "POSIXlt")

#' Gets filterable variable names
#'
#' Gets filterable variable names from a given object. The names match variables
#' of classes in the `teal:::.filterable_class` enum.
#' @param data (`object`)\cr
#'   the R object containing elements which class can be checked through `vapply` or `apply`.
#'
#' @examples
#' df <- data.frame(
#'   a = letters[1:3],
#'   b = 1:3,
#'   c = Sys.Date() + 1:3,
#'   d = Sys.time() + 1:3,
#'   z = complex(3)
#' )
#' teal:::get_filterable_varnames(df)
#' @return `character` the array of the matched element names
get_filterable_varnames <- function(data) {
  UseMethod("get_filterable_varnames")
}

#' @export
get_filterable_varnames.default <- function(data) { #nolint #nousage
  is_expected_class <- vapply(
    X = data,
    FUN = function(x) any(class(x) %in% .filterable_class),
    FUN.VALUE = logical(1)
  )
  names(is_expected_class[is_expected_class])
}

#' @export
get_filterable_varnames.matrix <- function(data) { #nolint #nousage
  # all columns are the same type in matrix
  is_expected_class <- class(data[, 1]) %in% .filterable_class
  if (is_expected_class && !is.null(names(data))) {
    names(data)
  } else {
    character(0)
  }
}

#' @title Returns a `choices_labeled` object
#'
#' @param data `data.frame`
#' @param choices `character` the array of chosen variables
#' @param varlabels `character` the labels of variables in data
#' @param keys `character` the names of the key columns in data
#' @return `character(0)` if choices are empty; a `choices_labeled` object otherwise
#' @noRd
data_choices_labeled <- function(data, choices, varlabels = character(0), keys = character(0)) {
  if (is_empty(choices)) {
    return(character(0))
  }

  choice_labels <- if (identical(varlabels, character(0))) {
    vapply(
      X = data,
      FUN = function(x) {
        label <- attr(x, "label")
        if (length(label) != 1) {
          ""
        } else {
          label
        }
      },
      FUN.VALUE = character(1)
    )[choices]
  } else {
    varlabels
  }

  if (!identical(choice_labels, character(0))) {
    choice_labels[is.na(choice_labels) | choice_labels == ""] <- names(
      choice_labels[is.na(choice_labels) | choice_labels == ""]
    )
    choice_types <- setNames(
      variable_types(data = data, columns = choices),
      choices
    )
    choice_types[keys] <- "primary_key"

    choices_labeled(
      choices = choices,
      labels = unname(choice_labels[choices]),
      types = choice_types[choices]
    )
  } else {
    choices
  }
}
