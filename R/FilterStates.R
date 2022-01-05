#' Initialize `FilterStates` object
#'
#' Initialize `FilterStates` object
#' @param data (`data.frame`, `MultiAssayExperiment`, `SummarizedExperiment`, `matrix`)\cr
#'   the R object which `subset` function is applied on.
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
#'     rf$ui("states"),
#'     verbatimTextOutput("expr"),
#'   ),
#'   server = function(input, output, session) {
#'     rf$srv_add_filter_state(id = "add", data = df)
#'     rf$server(id = "states")
#'     output$expr <- renderText({
#'       deparse1(rf$get_call(), collapse = "\n")
#'     })
#'     observeEvent(input$clear, rf$queue_empty())
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
init_filter_states.data.frame <- function(data, # nolint #nousage
                                          input_dataname,
                                          output_dataname = input_dataname,
                                          datalabel = character(0),
                                          varlabels = character(0),
                                          keys = character(0)) {
  DFFilterStates$new(
    input_dataname = input_dataname,
    output_dataname = output_dataname,
    datalabel = datalabel,
    varlabels = varlabels,
    keys = keys
  )
}

#' @export
init_filter_states.matrix <- function(data, # nolint #nousage
                                      input_dataname,
                                      output_dataname = input_dataname,
                                      datalabel = character(0)) {
  MatrixFilterStates$new(
    input_dataname = input_dataname,
    output_dataname = output_dataname,
    datalabel = datalabel
  )
}

#' @export
init_filter_states.MultiAssayExperiment <- function(data, # nolint #nousage
                                                    input_dataname,
                                                    output_dataname = input_dataname,
                                                    datalabel = character(0),
                                                    varlabels,
                                                    keys = character(0)) {
  MAEFilterStates$new(
    input_dataname = input_dataname,
    output_dataname = output_dataname,
    datalabel = datalabel,
    varlabels = varlabels,
    keys = keys
  )
}

#' @export
init_filter_states.SummarizedExperiment <- function(data, # nolint #nousage
                                                    input_dataname,
                                                    output_dataname = input_dataname,
                                                    datalabel = character(0)) {
  SEFilterStates$new(
    input_dataname = input_dataname,
    output_dataname = output_dataname,
    datalabel = datalabel
  )
}


# FilterStates -----
#'
#' @title `FilterStates` R6 class
#'
#' @description
#' Implements [ReactiveQueue] to the `teal` filters.
#' Class manages adding and removing `FilterState` to the reactive
#' queue and returns reproducible R expression relevant to specific
#' `FilterStates` subclass.
#' Currently `data.frame`, `MultiAssayExperiment`,
#' `SummarizedExperiment` and `matrix` are available.
#'
#' @keywords internal
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
      logger::log_trace("Instantiated { class(self)[1] }, dataname: { deparse1(private$input_dataname) }")
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
      filter_items <- sapply(
        X = queue_list,
        USE.NAMES = TRUE,
        simplify = FALSE,
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
    #' Gets the name of the function used to filter the data in this `FilterStates`.
    #'
    #' Get function name used to create filter call. By default it's a
    #' "subset" but can be overridden by child class method.
    #' @return `character(1)` the name of the function
    get_fun = function() {
      "subset"
    },

    #' @description
    #' Remove all `FilterState` objects from all queues in this `FilterStates`.
    #' @return NULL
    queue_empty = function() {
      logger::log_trace("{ class(self)[1] } emptying queue, dataname: { deparse1(private$input_dataname) }")
      queue_indices <- if (is.null(names(private$queue))) {
        seq_along(private$queue)
      } else {
        names(private$queue)
      }

      lapply(queue_indices, function(queue_index) {
        queue_elements <- names(self$queue_get(queue_index = queue_index))
        lapply(queue_elements, function(element_id) {
          self$queue_remove(queue_index = queue_index, element_id = element_id)
        })
      })

      logger::log_trace("{ class(self)[1] } emptied queue, dataname: { deparse1(private$input_dataname) }")
      invisible(NULL)
    },

    #' @description
    #' Returns a list of `FilterState` objects stored in this `FilterStates.`
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
    #'  must be a list even if single `ReactiveQueue` is set.
    queue_initialize = function(x) {
      stopifnot(is_class_list("ReactiveQueue")(x))
      private$queue <- x
      invisible(NULL)
    },

    #' @description
    #' Adds a new `FilterState` object to this `FilterStates`
    #' @param x (`FilterState`)\cr
    #'   object to be added to the queue
    #' @param queue_index (`character(1)`, `integer(1)`)\cr
    #'   index of the `private$queue` list where `ReactiveQueue` are kept.
    #' @param element_id (`character(1)`)\cr
    #'   name of the `ReactiveQueue` element.
    #' @note throws an exception if the length of `x` does not match the length of
    #'   `element_id`
    queue_push = function(x, queue_index, element_id) {
      logger::log_trace("{ class(self)[1] } pushing into queue, dataname: { deparse1(private$input_dataname) }")
      private$validate_queue_exists(queue_index)
      stopifnot(is_character_single(element_id))

      states <- if (is.list(x)) {
        x
      } else {
        list(x)
      }
      state <- setNames(states, element_id)
      private$queue[[queue_index]]$push(state)
      logger::log_trace("{ class(self)[1] } pushed into queue, dataname: { deparse1(private$input_dataname) }")
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
      logger::log_trace(paste(
        "{ class(self)[1] } removing a filter from queue { queue_index },",
        "dataname: { deparse1(private$input_dataname) }"
      ))
      private$validate_queue_exists(queue_index)
      stopifnot(is_character_single(element_id))
      stopifnot(is_character_single(queue_index) || is_numeric_single(queue_index))
      stopifnot(is_character_single(element_id))

      filters <- self$queue_get(queue_index = queue_index, element_id = element_id)
      private$queue[[queue_index]]$remove(filters)
      logger::log_trace(
        "{ class(self)[1] } removed from queue { queue_index }, dataname: { deparse1(private$input_dataname) }"
      )
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
    #' Gets the reactive values from the active `FilterState` objects.
    #'
    #' Get active filter state from the `FilterState` objects kept in `ReactiveQueue`(s).
    #' The output list is a compatible input to `self$set_filter_state`.
    #'
    #' @return `list` containing `list` per each `FilterState` in the `ReactiveQueue`
    get_filter_state = function() {
      stop("Pure virtual method.")
    },

    #' @description
    #' Sets active `FilterState` objects.
    #'
    #' @param data (`data.frame`)\cr
    #'   data which are supposed to be filtered
    #' @param state (`named list`)\cr
    #'   should contain values which are initial selection in the `FilterState`.
    #'   Names of the `list` element should correspond to the name of the
    #'   column in `data`.
    #' @return function which throws an error
    set_filter_state = function(data, state) {
      stop("Pure virtual method.")
    },

    #' @description Remove a single `FilterState` from the `ReactiveQueue`.
    #'
    #' @param element_id (`character`)\cr
    #'  Name of variable to remove its `FilterState`.
    #'
    #' @return `NULL`
    remove_filter_state = function(element_id) {
      stop("This variable can not be removed from the filter.")
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
    #' @param id (`character(1)`)\cr
    #'   an ID string that corresponds with the ID used to call the module's UI function.
    #' @param data (`data.frame`, `MultiAssayExperiment`, `SummarizedExperiment`, `matrix`)\cr
    #'  object containing columns to be used as filter variables.
    #' @param ... ignored
    #' @return `moduleServer` function which returns `NULL`
    srv_add_filter_state = function(id, data, ...) {
      check_ellipsis(..., stop = FALSE)
      moduleServer(
        id = id,
        function(input, output, session) {
          NULL
        }
      )
    }
  ),
  private = list(
    card_id = character(0),
    card_ids = character(0),
    datalabel = character(0),
    input_dataname = NULL, # because it holds object of class name
    output_dataname = NULL, # because it holds object of class name,
    ns = NULL, # shiny ns()
    observers = list(), # observers
    queue = NULL, # list of ReactiveQueue(s) initialized by self$queue_initialize

    #' Module to insert/remove `FilterState` UI
    #'
    #' This module adds the shiny UI of the newly added `FilterState` object to queue to the Active Filter
    #' Variables, calls `FilterState` modules and creates observer to remove state
    #' parameter filter_state (`FilterState`)
    #'
    #' parameter queue_index (`character(1)`, `logical(1)`)\cr
    #'   index of the `private$queue` list where `ReactiveQueue` are kept.
    #' parameter element_id (`character(1)`)\cr
    #'   name of `ReactiveQueue` element.
    #' return `moduleServer` function which returns `NULL`
    insert_filter_state_ui = function(id, filter_state, queue_index, element_id) {
      checkmate::assert_class(filter_state, "FilterState")
      checkmate::assert(
        checkmate::check_int(queue_index),
        checkmate::check_character(queue_index, len = 1),
        combine = "or"
      )
      checkmate::assert_character(element_id, len = 1)
      moduleServer(
        id = id,
        function(input, output, session) {
          logger::log_trace(paste(
            "{ class(self)[1] }$insert_filter_state_ui, adding FilterState UI,",
            "dataname: { deparse1(private$input_dataname) }"
          ))
          shiny::setBookmarkExclude("remove")
          card_id <- session$ns("card")
          queue_id <- sprintf("%s-%s", queue_index, element_id)
          private$card_ids[queue_id] <- card_id

          insertUI(
            selector = sprintf("#%s", private$card_id),
            where = "beforeEnd",
            # add span with id to be removable
            ui = {
              div(
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
            }
          )

          filter_state$server(id = "content")
          private$observers[[queue_id]] <- observeEvent(
            ignoreInit = TRUE,
            ignoreNULL = TRUE,
            eventExpr = input$remove,
            handlerExpr = {
              logger::log_trace(paste(
                "{ class(self)[1] }$insert_filter_state_ui@1 removing FilterState from queue '{ queue_index }',",
                "dataname: { deparse1(private$input_dataname) }"
              ))
              self$queue_remove(queue_index, element_id)
              logger::log_trace(paste(
                "{ class(self)[1] }$insert_filter_state_ui@1 removed FilterState from queue '{ queue_index }',",
                "dataname: { deparse1(private$input_dataname) }"
              ))
            }
          )

          logger::log_trace(paste(
            "{ class(self)[1] }$insert_filter_state_ui, added FilterState UI,",
            "dataname: { deparse1(private$input_dataname) }"
          ))
          NULL
        }
      )
    },

    # Remove shiny element. Method can be called from reactive session where
    #' `observeEvent` for remove-filter-state is set and also from `FilteredDataset`
    #' level, where shiny-session-namespace is different. That is why it's important
    #' to remove shiny elements from anywhere. In `add_filter_state` `session$ns(NULL)`
    #' is equivalent to `private$ns(queue_index)`. This means that
    #'
    remove_filter_state_ui = function(queue_index, element_id) {
      queue_id <- sprintf("%s-%s", queue_index, element_id)
      removeUI(selector = sprintf("#%s", private$card_ids[queue_id]))
      private$card_ids <- private$card_ids[names(private$card_ids) != queue_id]
      if (length(private$observers[[queue_id]]) > 0) {
        private$observers[[queue_id]]$destroy()
        private$observers[[queue_id]] <- NULL
      }
    },

    # Checks if the queue of the given index was initialized in this `FilterStates`
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
    # @param prefix `character(1)` text to prefix id. Needed in case of multiple
    #  queue objects where keys (variables) might be duplicated across queues
    # @return `list` the mapping
    map_vars_to_html_ids = function(keys, prefix = "") {
      checkmate::assert_character(keys, null.ok = TRUE)
      checkmate::assert_character(prefix, len = 1)
      sanitized_values <- make.unique(gsub("[^[:alnum:]]", perl = TRUE, replacement = "", x = keys))
      sanitized_values <- sprintf("%s_var_%s", prefix, sanitized_values)
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
    #' Server module
    #'
    #' @param id (`character(1)`)\cr
    #'   an ID string that corresponds with the ID used to call the module's UI function.
    #' @return `moduleServer` function which returns `NULL`
    server = function(id) {
      moduleServer(
        id = id,
        function(input, output, session) {
          previous_state <- reactiveVal(isolate(self$queue_get(1L)))
          added_state_name <- reactiveVal(character(0))
          removed_state_name <- reactiveVal(character(0))

          observeEvent(self$queue_get(1L), {
            added_state_name(setdiff(names(self$queue_get(1L)), names(previous_state())))
            removed_state_name(setdiff(names(previous_state()), names(self$queue_get(1L))))

            previous_state(self$queue_get(1L))
          })

          observeEvent(added_state_name(), ignoreNULL = TRUE, {
            fstates <- self$queue_get(1L)
            html_ids <- private$map_vars_to_html_ids(names(fstates))
            for (fname in added_state_name()) {
              private$insert_filter_state_ui(
                id = html_ids[fname],
                filter_state = fstates[[fname]],
                queue_index = 1L,
                element_id = fname
              )
            }
            added_state_name(character(0))
          })

          observeEvent(removed_state_name(), {
            req(removed_state_name())
            for (fname in removed_state_name()) {
              private$remove_filter_state_ui(1L, fname)
            }
            removed_state_name(character(0))
          })
          NULL
        }
      )
    },

    #' @description
    #' Gets the reactive values from the active `FilterState` objects.
    #'
    #' Get active filter state from the `FilterState` objects kept in `ReactiveQueue`.
    #' The output list is a compatible input to `self$set_filter_state`.
    #' @return `list` with named elements corresponding to `FilterState` in the `ReactiveQueue`.
    get_filter_state = function() {
      lapply(self$queue_get(1L), function(x) x$get_state())
    },

    #' @description
    #' Set filter state
    #'
    #' @param data (`data.frame`)\cr
    #'   data which are supposed to be filtered.
    #' @param state (`named list`)\cr
    #'   should contain values which are initial selection in the `FilterState`.
    #'   Names of the `list` element should correspond to the name of the
    #'   column in `data`.
    #' @param vars_include (`character(n)`)\cr
    #'  optional, vector of column names to be included.
    #' @param ... ignored.
    #' @examples
    #' dffs <- teal:::DFFilterStates$new(
    #'   input_dataname = "iris",
    #'   output_dataname = "iris_filtered",
    #'   datalabel = character(0),
    #'   varlabels = character(0),
    #'   keys = character(0)
    #' )
    #' fs <- list(
    #'   Sepal.Length = list(selected = c(5.1, 6.4), keep_na = TRUE, keep_inf = TRUE),
    #'   Species = list(selected = c("setosa", "versicolor"), keep_na = FALSE)
    #' )
    #' dffs$set_filter_state(state = fs, data = iris)
    #' shiny::isolate(dffs$get_filter_state())
    #'
    #' @return `NULL`
    set_filter_state = function(data, state, vars_include = get_filterable_varnames(data = data), ...) {
      checkmate::assert_data_frame(data)
      checkmate::assert(
        checkmate::check_subset(names(state), names(data)),
        checkmate::check_class(state, "default_filter"),
        combine = "or"
      )
      logger::log_trace(
        "{ class(self)[1] }$set_filter_state initializing, dataname: { deparse1(private$input_dataname) }"
      )

      filter_states <- self$queue_get(1L)
      state_names <- names(state)
      excluded_vars <- setdiff(state_names, vars_include)
      if (length(excluded_vars) > 0) {
        warning(
          paste(
            "These columns filters were excluded:",
            paste(excluded_vars, collapse = ", "),
            "from dataset",
            private$input_dataname
          )
        )
        logger::log_warn(
          paste(
            "Columns filters { paste(excluded_vars, collapse = ', ') } were excluded",
            "from { deparse1(private$input_dataname) }"
          )
        )
      }

      filters_to_apply <- state_names[state_names %in% vars_include]

      for (varname in filters_to_apply) {
        value <- state[[varname]]
        if (varname %in% names(filter_states)) {
          fstate <- filter_states[[varname]]
          set_filter_state(x = value, fstate)
        } else {
          fstate <- init_filter_state(
            data[[varname]],
            varname = as.name(varname),
            varlabel = private$get_varlabels(varname),
            input_dataname = private$input_dataname
          )
          set_filter_state(x = value, fstate)
          self$queue_push(x = fstate, queue_index = 1L, element_id = varname)
        }
      }
      logger::log_trace(
        "{ class(self)[1] }$set_filter_state initialized, dataname: { deparse1(private$input_dataname) }"
      )
      NULL
    },

    #' @description Remove a `FilterState` from the `ReactiveQueue`.
    #'
    #' @param element_id (`character(1)`)\cr name of `ReactiveQueue` element.
    #'
    #' @return `NULL`
    #'
    remove_filter_state = function(element_id) {
      logger::log_trace("{ class(self)[1] }$remove_filter_state called, dataname: { deparse1(private$input_dataname) }")

      if (!element_id %in% names(self$queue_get(1L))) {
        warning(paste(
          "Variable:", element_id, "is not present in the actual active filters of dataset: { private$input_dataname }",
          "therefore no changes are applied."
        ))
        logger::log_warn(
          paste(
            "Variable:", element_id, "is not present in the actual active filters of dataset:",
            "{ private$input_dataname } therefore no changes are applied."
          )
        )
      } else {
        self$queue_remove(queue_index = 1L, element_id = element_id)
        logger::log_trace("{ class(self)[1] }$remove_filter_state done, dataname: { deparse1(private$input_dataname) }")
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
    #' @param id (`character(1)`)\cr
    #'   an ID string that corresponds with the ID used to call the module's UI function.
    #' @param data (`data.frame`)\cr
    #'  object which columns are used to choose filter variables.
    #' @param vars_include (`character(n)`)\cr
    #'  optional, vector of column names to be included.
    #' @param ... ignored
    #' @return `moduleServer` function which returns `NULL`
    srv_add_filter_state = function(id, data, vars_include = get_filterable_varnames(data = data), ...) {
      stopifnot(is.data.frame(data))
      check_ellipsis(..., stop = FALSE)
      moduleServer(
        id = id,
        function(input, output, session) {
          logger::log_trace(
            "DFFilterStates$srv_add_filter_state initializing, dataname: { deparse1(private$input_dataname) }"
          )
          shiny::setBookmarkExclude(c("var_to_add"))
          active_filter_vars <- reactive({
            vapply(
              X = self$queue_get(queue_index = 1L),
              FUN.VALUE = character(1),
              function(x) x$get_varname(deparse = TRUE)
            )
          })

          # available choices to display
          avail_column_choices <- reactive({
            choices <- setdiff(vars_include, active_filter_vars())

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
              logger::log_trace(paste(
                "DFFilterStates$srv_add_filter_state@1 updating available column choices,",
                "dataname: { deparse1(private$input_dataname) }"
              ))
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
              logger::log_trace(paste(
                "DFFilterStates$srv_add_filter_state@1 updated available column choices,",
                "dataname: { deparse1(private$input_dataname) }"
              ))
            }
          )

          observeEvent(
            eventExpr = input$var_to_add,
            handlerExpr = {
              logger::log_trace(paste(
                "DFFilterStates$srv_add_filter_state@2 adding FilterState,",
                "dataname: { deparse1(private$input_dataname) }"
              ))
              self$queue_push(
                x = init_filter_state(
                  data[[input$var_to_add]],
                  varname = as.name(input$var_to_add),
                  varlabel = private$get_varlabels(input$var_to_add),
                  input_dataname = private$input_dataname
                ),
                queue_index = 1L,
                element_id = input$var_to_add
              )
              logger::log_trace(paste(
                "DFFilterStates$srv_add_filter_state@2 added FilterState,",
                "dataname: { deparse1(private$input_dataname) }"
              ))
            }
          )

          logger::log_trace(
            "DFFilterStates$srv_add_filter_state initialized, dataname: { deparse1(private$input_dataname) }"
          )
          NULL
        }
      )
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
        varlabels[is.na(varlabels) | varlabels == ""] <- variables[
          is.na(varlabels) | varlabels == ""
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
    #' Server module
    #' @param id (`character(1)`)\cr
    #'   an ID string that corresponds with the ID used to call the module's UI function.
    #' @return `moduleServer` function which returns `NULL`
    server = function(id) {
      moduleServer(
        id = id,
        function(input, output, session) {
          previous_state <- reactiveVal(isolate(self$queue_get("y")))
          added_state_name <- reactiveVal(character(0))
          removed_state_name <- reactiveVal(character(0))

          observeEvent(self$queue_get("y"), {
            added_state_name(setdiff(names(self$queue_get("y")), names(previous_state())))
            removed_state_name(setdiff(names(previous_state()), names(self$queue_get("y"))))

            previous_state(self$queue_get("y"))
          })

          observeEvent(added_state_name(), ignoreNULL = TRUE, {
            fstates <- self$queue_get("y")
            html_ids <- private$map_vars_to_html_ids(names(fstates))
            for (fname in added_state_name()) {
              private$insert_filter_state_ui(
                id = html_ids[fname],
                filter_state = fstates[[fname]],
                queue_index = "y",
                element_id = fname
              )
            }
            added_state_name(character(0))
          })

          observeEvent(removed_state_name(), {
            req(removed_state_name())
            for (fname in removed_state_name()) {
              private$remove_filter_state_ui("y", fname)
            }
            removed_state_name(character(0))
          })
          NULL
        }
      )
    },

    #' @description
    #' Returns active `FilterState` objects.
    #'
    #' Gets all active filters from this dataset in form of the nested list.
    #' The output list can be used as input to `self$set_filter_state`.
    #'
    #' @return `list` with elements number equal number of `FilterStates`.
    get_filter_state = function() {
      lapply(self$queue_get(queue_index = "y"), function(x) x$get_state())
    },

    #' @description
    #' Set filter state
    #'
    #' @param data (`MultiAssayExperiment`)\cr
    #'   data which are supposed to be filtered.
    #' @param state (`named list`)\cr
    #'   should contain values which are initial selection in the `FilterState`.
    #'   Names of the `list` element should correspond to the name of the
    #'   column in `colData(data)`.
    #' @param ... ignored.
    #' @return `NULL`
    set_filter_state = function(data, state, ...) {
      checkmate::assert_class(data, "MultiAssayExperiment")
      checkmate::assert(
        checkmate::check_subset(names(state), names(SummarizedExperiment::colData(data))),
        checkmate::check_class(state, "default_filter"),
        combine = "or"
      )
      logger::log_trace(paste(
        "MAEFilterState$set_filter_state initializing,",
        "dataname: { deparse1(private$input_dataname) }"
      ))
      filter_states <- self$queue_get("y")
      for (varname in names(state)) {
        value <- state[[varname]]
        if (varname %in% names(filter_states)) {
          fstate <- filter_states[[varname]]
          set_filter_state(x = value, fstate)
        } else {
          fstate <- init_filter_state(
            SummarizedExperiment::colData(data)[[varname]],
            varname = as.name(varname),
            varlabel = private$get_varlabels(varname),
            input_dataname = private$input_dataname,
            extract_type = "list"
          )
          set_filter_state(x = value, fstate)
          fstate$set_na_rm(TRUE)
          self$queue_push(
            x = fstate,
            queue_index = "y",
            element_id = varname
          )
        }
      }
      logger::log_trace(paste(
        "MAEFilterState$set_filter_state initialized,",
        "dataname: { deparse1(private$input_dataname) }"
      ))
      NULL
    },

    #' @description Remove a variable from the `ReactiveQueue` and its corresponding UI element.
    #'
    #' @param element_id (`character(1)`)\cr name of `ReactiveQueue` element.
    #'
    #' @return `NULL`
    #'
    remove_filter_state = function(element_id) {
      logger::log_trace(
        "{ class(self)[1] }$remove_filter_state called, dataname: { deparse1(private$input_dataname) }"
      )

      if (!element_id %in% names(self$queue_get("y"))) {
        warning(paste(
          "Variable:", element_id,
          "is not present in the actual active filters of dataset: { private$input_dataname }",
          "therefore no changes are applied."
        ))
        logger::log_warn(
          paste(
            "Variable:", element_id, "is not present in the actual active filters of dataset:",
            "{ private$input_dataname } therefore no changes are applied."
          )
        )
      } else {
        self$queue_remove(queue_index = "y", element_id = element_id)
        logger::log_trace(
          "{ class(self)[1] }$remove_filter_state done, dataname: { deparse1(private$input_dataname) }"
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
    #' @param id (`character(1)`)\cr
    #'   an ID string that corresponds with the ID used to call the module's UI function.
    #' @param data (`MultiAssayExperiment`)\cr
    #'  object containing `colData` which columns are used to be used
    #'  to choose filter variables.
    #'  in `optionalSelectInput`
    #' @param ... ignored
    #' @return `moduleServer` function which returns `NULL`
    srv_add_filter_state = function(id, data, ...) {
      stopifnot(is(data, "MultiAssayExperiment"))
      check_ellipsis(..., stop = FALSE)
      moduleServer(
        id = id,
        function(input, output, session) {
          logger::log_trace(
            "MAEFilterState$srv_add_filter_state initializing, dataname: { deparse1(private$input_dataname) }"
          )
          shiny::setBookmarkExclude("var_to_add")
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
            data_choices_labeled(
              data = SummarizedExperiment::colData(data),
              choices = choices,
              varlabels = private$get_varlabels(choices),
              keys = private$keys
            )
          })
          observeEvent(
            avail_column_choices(),
            ignoreNULL = TRUE,
            handlerExpr = {
              logger::log_trace(paste(
                "MAEFilterStates$srv_add_filter_state@1 updating available column choices,",
                "dataname: { deparse1(private$input_dataname) }"
              ))
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
              logger::log_trace(paste(
                "MAEFilterStates$srv_add_filter_state@1 updated available column choices,",
                "dataname: { deparse1(private$input_dataname) }"
              ))
            }
          )

          observeEvent(
            eventExpr = input$var_to_add,
            handlerExpr = {
              logger::log_trace(paste(
                "MAEFilterStates$srv_add_filter_state@2 adding FilterState,",
                "dataname: { deparse1(private$input_dataname) }"
              ))

              fstate <- init_filter_state(
                SummarizedExperiment::colData(data)[[input$var_to_add]],
                varname = as.name(input$var_to_add),
                varlabel = private$get_varlabels(input$var_to_add),
                input_dataname = private$input_dataname,
                extract_type = "list"
              )
              fstate$set_na_rm(TRUE)

              self$queue_push(
                x = fstate,
                queue_index = "y",
                element_id = input$var_to_add
              )
              logger::log_trace(paste(
                "MAEFilterStates$srv_add_filter_state@2 added FilterState,",
                "dataname: { deparse1(private$input_dataname) }"
              ))
            }
          )

          logger::log_trace(
            "MAEFilterState$srv_add_filter_state initialized, dataname: { deparse1(private$input_dataname) }"
          )
          NULL
        }
      )
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
    #' Server module
    #' @param id (`character(1)`)\cr
    #'   an ID string that corresponds with the ID used to call the module's UI function.
    #' @return `moduleServer` function which returns `NULL`
    server = function(id) {
      moduleServer(
        id = id,
        function(input, output, session) {
          previous_state_subset <- reactiveVal(isolate(self$queue_get("subset")))
          added_state_name_subset <- reactiveVal(character(0))
          removed_state_name_subset <- reactiveVal(character(0))

          observeEvent(self$queue_get("subset"), {
            added_state_name_subset(
              setdiff(names(self$queue_get("subset")), names(previous_state_subset()))
            )
            removed_state_name_subset(
              setdiff(names(previous_state_subset()), names(self$queue_get("subset")))
            )
            previous_state_subset(self$queue_get("subset"))
          })

          observeEvent(added_state_name_subset(), ignoreNULL = TRUE, {
            fstates <- self$queue_get("subset")
            html_ids <- private$map_vars_to_html_ids(keys = names(fstates), prefix = "rowData")
            for (fname in added_state_name_subset()) {
              private$insert_filter_state_ui(
                id = html_ids[fname],
                filter_state = fstates[[fname]],
                queue_index = "subset",
                element_id = fname
              )
            }
            added_state_name_subset(character(0))
          })

          observeEvent(removed_state_name_subset(), {
            req(removed_state_name_subset())
            for (fname in removed_state_name_subset()) {
              private$remove_filter_state_ui("subset", fname)
            }
            removed_state_name_subset(character(0))
          })

          # select
          previous_state_select <- reactiveVal(isolate(self$queue_get("select")))
          added_state_name_select <- reactiveVal(character(0))
          removed_state_name_select <- reactiveVal(character(0))

          observeEvent(self$queue_get("select"), {
            # find what has been added or removed
            added_state_name_select(
              setdiff(names(self$queue_get("select")), names(previous_state_select()))
            )
            removed_state_name_select(
              setdiff(names(previous_state_select()), names(self$queue_get("select")))
            )
            previous_state_select(self$queue_get("select"))
          })

          observeEvent(added_state_name_select(), ignoreNULL = TRUE, {
            fstates <- self$queue_get("select")
            html_ids <- private$map_vars_to_html_ids(keys = names(fstates), prefix = "colData")
            for (fname in added_state_name_select()) {
              private$insert_filter_state_ui(
                id = html_ids[fname],
                filter_state = fstates[[fname]],
                queue_index = "select",
                element_id = fname
              )
            }
            added_state_name_select(character(0))
          })

          observeEvent(removed_state_name_select(), {
            req(removed_state_name_select())
            for (fname in removed_state_name_select()) {
              private$remove_filter_state_ui("select", fname)
            }
            removed_state_name_select(character(0))
          })
          NULL
        }
      )
    },

    #' @description
    #' Gets the reactive values from the active `FilterState` objects.
    #'
    #' Gets all active filters from this dataset in form of the nested list.
    #' The output list is a compatible input to `self$set_filter_state`.
    #'
    #' @return `list` containing one or two lists  depending on the number of
    #' `ReactiveQueue` object (I.e. if `rowData` and `colData` exist). Each
    #' `list` contains elements number equal to number of active filter variables.
    get_filter_state = function() {
      states <- sapply(
        X = names(private$queue),
        simplify = FALSE,
        function(x) {
          lapply(self$queue_get(queue_index = x), function(xx) xx$get_state())
        }
      )
      Filter(function(x) length(x) > 0, states)
    },

    #' @description
    #' Set filter state
    #'
    #' @param data (`SummarizedExperiment`)\cr
    #'   data which are supposed to be filtered.
    #' @param state (`named list`)\cr
    #'   this list should contain `subset` and `select` element where
    #'   each should be a named list containing values as a selection in the `FilterState`.
    #'   Names of each the `list` element in `subset` and `select` should correspond to
    #'   the name of the column in `rowData(data)` and `colData(data)`.
    #' @param ... ignored.
    #' @return `NULL`
    set_filter_state = function(data, state, ...) {
      checkmate::assert_class(data, "SummarizedExperiment")
      checkmate::assert_class(state, "list")

      checkmate::assert(
        checkmate::check_subset(names(state), c("subset", "select")),
        checkmate::check_class(state, "default_filter"),
        combine = "or"
      )
      checkmate::assert(
        checkmate::test_null(state$subset),
        checkmate::assert(
          checkmate::check_class(state$subset, "list"),
          checkmate::check_subset(names(state$subset), names(SummarizedExperiment::rowData(data))),
          combine = "and"
        ),
        combine = "or"
      )
      checkmate::assert(
        checkmate::test_null(state$select),
        checkmate::assert(
          checkmate::check_class(state$select, "list"),
          checkmate::check_subset(names(state$select), names(SummarizedExperiment::colData(data))),
          combine = "and"
        ),
        combine = "or"
      )

      filter_states <- self$queue_get("subset")
      for (varname in names(state$subset)) {
        value <- state$subset[[varname]]
        if (varname %in% names(filter_states)) {
          fstate <- filter_states[[varname]]
          set_filter_state(x = value, fstate)
        } else {
          fstate <- init_filter_state(
            SummarizedExperiment::rowData(data)[[varname]],
            varname = as.name(varname),
            input_dataname = private$input_dataname
          )
          set_filter_state(x = value, fstate)
          self$queue_push(
            x = fstate,
            queue_index = "subset",
            element_id = varname
          )
        }
      }

      filter_states <- self$queue_get("select")
      for (varname in names(state$select)) {
        value <- state$select[[varname]]
        if (varname %in% names(filter_states)) {
          fstate <- filter_states[[varname]]
          set_filter_state(x = value, fstate)
        } else {
          fstate <- init_filter_state(
            SummarizedExperiment::colData(data)[[varname]],
            varname = as.name(varname),
            input_dataname = private$input_dataname
          )
          set_filter_state(x = value, fstate)
          self$queue_push(
            x = fstate,
            queue_index = "select",
            element_id = varname
          )
        }

      }
      logger::log_trace(paste(
        "SEFilterState$set_filter_state initialized,",
        "dataname: { deparse1(private$input_dataname) }"
      ))
      NULL
    },

    #' @description Remove a variable from the `ReactiveQueue` and its corresponding UI element.
    #'
    #' @param element_id (`character(1)`)\cr name of `ReactiveQueue` element.
    #'
    #' @return `NULL`
    remove_filter_state = function(element_id) {
      logger::log_trace(
        "{ class(self)[1] }$remove_filter_state called, dataname: { deparse1(private$input_dataname) }"
      )

      checkmate::assert(
        !checkmate::test_null(names(element_id)),
        checkmate::check_subset(names(element_id), c("subset", "select")),
        combine = "and"
      )
      for (varname in element_id$subset) {
        if (!all(unlist(element_id$subset) %in% names(self$queue_get("subset")))) {
          warning(paste(
            "Variable:", element_id, "is not present in the actual active subset filters of dataset:",
            "{ deparse(private$input_dataname) } therefore no changes are applied."
          ))
          logger::log_warn(
            paste(
              "Variable:", element_id, "is not present in the actual active subset filters of dataset:",
              "{ deparse(private$input_dataname) } therefore no changes are applied."
            )
          )
        } else {
          self$queue_remove(queue_index = "subset", element_id = varname)
        }
      }

      for (varname in element_id$select) {
        if (!all(unlist(element_id$select) %in% names(self$queue_get("select")))) {
          warning(paste(
            "Variable:", element_id, "is not present in the actual active select filters of dataset:",
            "{ private$input_dataname } therefore no changes are applied."
          ))
          logger::log_warn(
            paste(
              "Variable:", element_id, "is not present in the actual active select filters of dataset:",
              "{ private$input_dataname } therefore no changes are applied."
            )
          )
        } else {
          self$queue_remove(queue_index = "select", element_id = varname)
          logger::log_trace(
            "{ class(self)[1] }$remove_filter_state done, dataname: { deparse1(private$input_dataname) }"
          )
        }
      }
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
    #' @param id (`character(1)`)\cr
    #'   an ID string that corresponds with the ID used to call the module's UI function.
    #' @param data (`SummarizedExperiment`)\cr
    #'  object containing `colData` and `rowData` which columns
    #'  are used to choose filter variables. Column selection from `colData`
    #'  and `rowData` are separate shiny entities.
    #' @param ... ignored
    #' @return `moduleServer` function which returns `NULL`
    srv_add_filter_state = function(id, data, ...) {
      stopifnot(is(data, "SummarizedExperiment"))
      check_ellipsis(..., stop = FALSE)
      moduleServer(
        id = id,
        function(input, output, session) {
          logger::log_trace(paste(
            "SEFilterState$srv_add_filter_state initializing,",
            "dataname: { deparse1(private$input_dataname) }"
          ))
          shiny::setBookmarkExclude(c("row_to_add", "col_to_add"))
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

            data_choices_labeled(
              data = row_data,
              choices = choices,
              varlabels = character(0),
              keys = NULL
            )
          })
          avail_col_data_choices <- reactive({
            choices <- setdiff(
              get_filterable_varnames(data = col_data),
              active_filter_col_vars()
            )

            data_choices_labeled(
              data = col_data,
              choices = choices,
              varlabels = character(0),
              keys = NULL
            )
          })


          observeEvent(
            avail_row_data_choices(),
            ignoreNULL = TRUE,
            handlerExpr = {
              logger::log_trace(paste(
                "SEFilterStates$srv_add_filter_state@1 updating available row data choices,",
                "dataname: { deparse1(private$input_dataname) }"
              ))
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
              logger::log_trace(paste(
                "SEFilterStates$srv_add_filter_state@1 updated available row data choices,",
                "dataname: { deparse1(private$input_dataname) }"
              ))
            }
          )

          observeEvent(
            avail_col_data_choices(),
            ignoreNULL = TRUE,
            handlerExpr = {
              logger::log_trace(paste(
                "SEFilterStates$srv_add_filter_state@2 updating available col data choices,",
                "dataname: { deparse1(private$input_dataname) }"
              ))
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
              logger::log_trace(paste(
                "SEFilterStates$srv_add_filter_state@2 updated available col data choices,",
                "dataname: { deparse1(private$input_dataname) }"
              ))
            }
          )

          observeEvent(
            eventExpr = input$col_to_add,
            handlerExpr = {
              logger::log_trace(paste(
                "SEFilterStates$srv_add_filter_state@3 adding FilterState to col data,",
                "dataname: { deparse1(private$input_dataname) }"
              ))
              self$queue_push(
                x = init_filter_state(
                  SummarizedExperiment::colData(data)[[input$col_to_add]],
                  varname = as.name(input$col_to_add),
                  input_dataname = private$input_dataname
                ),
                queue_index = "select",
                element_id = input$col_to_add
              )
              logger::log_trace(paste(
                "SEFilterStates$srv_add_filter_state@3 added FilterState to col data,",
                "dataname: { deparse1(private$input_dataname) }"
              ))
            }
          )

          observeEvent(
            eventExpr = input$row_to_add,
            handlerExpr = {
              logger::log_trace(paste(
                "SEFilterStates$srv_add_filter_state@4 adding FilterState to row data,",
                "dataname: { deparse1(private$input_dataname) }"
              ))
              self$queue_push(
                x = init_filter_state(
                  SummarizedExperiment::rowData(data)[[input$row_to_add]],
                  varname = as.name(input$row_to_add),
                  input_dataname = private$input_dataname
                ),
                queue_index = "subset",
                element_id = input$row_to_add
              )
              logger::log_trace(paste(
                "SEFilterStates$srv_add_filter_state@4 added FilterState to row data,",
                "dataname: { deparse1(private$input_dataname) }"
              ))
            }
          )

          logger::log_trace(
            "SEFilterState$srv_add_filter_state initialized, dataname: { deparse1(private$input_dataname) }"
          )
          NULL
        }
      )
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
    #' Server module
    #' @param id (`character(1)`)\cr
    #'   an ID string that corresponds with the ID used to call the module's UI function.
    #' @return `moduleServer` function which returns `NULL`
    server = function(id) {
      moduleServer(
        id = id,
        function(input, output, session) {
          previous_state <- reactiveVal(isolate(self$queue_get("subset")))
          added_state_name <- reactiveVal(character(0))
          removed_state_name <- reactiveVal(character(0))

          observeEvent(self$queue_get("subset"), {
            added_state_name(
              setdiff(names(self$queue_get("subset")), names(previous_state()))
            )
            removed_state_name(
              setdiff(names(previous_state()), names(self$queue_get("subset")))
            )
            previous_state(self$queue_get("subset"))
          })

          observeEvent(added_state_name(), ignoreNULL = TRUE,  {
            fstates <- self$queue_get("subset")
            html_ids <- private$map_vars_to_html_ids(keys = names(fstates))
            for (fname in added_state_name()) {
              private$insert_filter_state_ui(
                id = html_ids[fname],
                filter_state = fstates[[fname]],
                queue_index = "subset",
                element_id = fname
              )
            }
            added_state_name(character(0))
          })

          observeEvent(removed_state_name(), {
            req(removed_state_name())

            for (fname in removed_state_name()) {
              private$remove_filter_state_ui("subset", fname)
            }
            removed_state_name(character(0))
          })
          NULL
        }
      )
    },

    #' @description
    #' Returns active `FilterState` objects.
    #'
    #' Gets all active filters from this dataset in form of the nested list.
    #' The output list can be used as input to `self$set_filter_state`.
    #'
    #' @return `list` containing `list` with selected values for each `FilterState`.
    get_filter_state = function() {
      lapply(self$queue_get(queue_index = "subset"), function(x) x$get_state())
    },

    #' @description
    #' Sets a filter state
    #'
    #' @param data (`matrix`)\cr
    #'   data which are supposed to be filtered.
    #' @param state (`named list`)\cr
    #'   should contain values which are initial selection in the `FilterState`.
    #'   Names of the `list` element should correspond to the name of the
    #'   column in `data`.
    #' @param ... ignored.
    #' @return `NULL`
    set_filter_state = function(data, state, ...) {
      checkmate::assert_class(data, "matrix")
      checkmate::assert(
        checkmate::assert(
          !checkmate::test_null(names(state)),
          checkmate::check_subset(names(state), colnames(data)),
          combine = "and"
        ),
        checkmate::check_class(state, "default_filter"),
        combine = "or"
      )
      logger::log_trace(paste(
        "MatrixFilterState$set_filter_state initializing,",
        "dataname: { deparse1(private$input_dataname) }"
      ))
      filter_states <- self$queue_get("subset")
      for (varname in names(state)) {
        value <- state[[varname]]
        if (varname %in% names(filter_states)) {
          fstate <- filter_states[[varname]]
          set_filter_state(x = value, fstate)
        } else {
          fstate <- init_filter_state(
            data[, varname],
            varname = as.name(varname),
            varlabel = varname,
            input_dataname = private$input_dataname,
            extract_type = "matrix"
          )
          set_filter_state(x = value, fstate)
          self$queue_push(
            x = fstate,
            queue_index = "subset",
            element_id = varname
          )
        }
      }
      logger::log_trace(paste(
        "MatrixFilterState$set_filter_state initialized,",
        "dataname: { deparse1(private$input_dataname) }"
      ))
      NULL
    },

    #' @description Remove a variable from the `ReactiveQueue` and its corresponding UI element.
    #'
    #' @param element_id (`character(1)`)\cr name of `ReactiveQueue` element.
    #'
    #' @return `NULL`
    remove_filter_state = function(element_id) {
      logger::log_trace("{ class(self)[1] }$remove_filter_state called, dataname: { deparse1(private$input_dataname) }")

      if (!element_id %in% names(self$queue_get("subset"))) {
        warning(paste(
          "Variable:", element_id, "is not present in the actual active filters of dataset:",
          "{ private$input_dataname } therefore no changes are applied."
        ))
        logger::log_warn(
          paste(
            "Variable:", element_id, "is not present in the actual active filters of dataset:",
            "{ deparse(private$input_dataname) } therefore no changes are applied."
          )
        )
      } else {
        self$queue_remove(queue_index = "subset", element_id = element_id)
        logger::log_trace("{ class(self)[1] }$remove_filter_state done, dataname: { deparse1(private$input_dataname) }")
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
    #' @param id (`character(1)`)\cr
    #'   an ID string that corresponds with the ID used to call the module's UI function.
    #' @param data (`matrix`)\cr
    #'  object which columns are used to choose filter variables.
    #' @param ... ignored
    #' @return `moduleServer` function which returns `NULL`
    srv_add_filter_state = function(id, data, ...) {
      stopifnot(is.matrix(data))
      check_ellipsis(..., stop = FALSE)
      moduleServer(
        id = id,
        function(input, output, session) {
          logger::log_trace(
            "MatrixFilterStates$srv_add_filter_state initializing, dataname: { deparse1(private$input_dataname) }"
          )
          shiny::setBookmarkExclude("var_to_add")
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
            data_choices_labeled(
              data = data,
              choices = choices,
              varlabels = character(0),
              keys = NULL
            )
          })
          observeEvent(
            avail_column_choices(),
            ignoreNULL = TRUE,
            handlerExpr = {
              logger::log_trace(paste(
                "MatrixFilterStates$srv_add_filter_state@1 updating column choices,",
                "dataname: { deparse1(private$input_dataname) }"
              ))
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
              logger::log_trace(paste(
                "MatrixFilterStates$srv_add_filter_state@1 updated column choices,",
                "dataname: { deparse1(private$input_dataname) }"
              ))
            }
          )



          observeEvent(
            eventExpr = input$var_to_add,
            handlerExpr = {
              logger::log_trace(paste(
                "MatrixFilterState$srv_add_filter_state@2 adding FilterState,",
                "dataname: { deparse1(private$input_dataname) }"
              ))
              self$queue_push(
                x = init_filter_state(
                  subset(data, select = input$var_to_add),
                  varname = as.name(input$var_to_add),
                  varlabel = private$get_varlabel(input$var_to_add),
                  input_dataname = private$input_dataname,
                  extract_type = "matrix"
                ),
                queue_index = "subset",
                element_id = input$var_to_add
              )
              logger::log_trace(paste(
                "MatrixFilterState$srv_add_filter_state@2 added FilterState,",
                "dataname: { deparse1(private$input_dataname) }"
              ))
            }
          )

          logger::log_trace(
            "MatrixFilterStates$srv_add_filter_state initialized, dataname: { deparse1(private$input_dataname) }"
          )
          NULL
        }
      )
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
get_filterable_varnames.default <- function(data) { # nolint #nousage
  is_expected_class <- vapply(
    X = data,
    FUN = function(x) any(class(x) %in% .filterable_class),
    FUN.VALUE = logical(1)
  )
  names(is_expected_class[is_expected_class])
}

#' @export
get_filterable_varnames.matrix <- function(data) { # nolint #nousage
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
#' @param data (`data.frame`, `DFrame`, `list`)\cr
#'   where labels can be taken from in case when `varlabels` is not specified.
#'   `data` must be specified if `varlabels` is not specified.
#' @param choices (`character`)\cr
#'  the array of chosen variables
#' @param varlabels (`character`)\cr
#'  the labels of variables in data
#' @param keys (`character`)\cr
#'  the names of the key columns in data
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
