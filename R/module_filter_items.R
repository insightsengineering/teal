#' Creates UI to show currently selected filters that can be applied to
#' the dataset, i.e. all columns selected for filtering are shown.
#'
#' @param id module id
#' @param dataname `character` name of dataset that is filtered
#' @md
#'
ui_filter_items <- function(id, dataname) {
  ns <- NS(id)
  uiOutput(ns("filters"))
}

#' @importFrom shinyWidgets pickerInput pickerOptions
srv_filter_items <- function(input, output, session, datasets, dataname, container = div) {
  # This whole function is complicated because the UI must be created dynamically.
  # Any observers previously registered during dynamic creation must be deleted on the
  # next dynamic registration of observers.
  # When a UI element with an id is added and removed, its input element still stays there
  # When it is readded, the input elements still are the old ones before it was removed,
  # e.g. the button input starts at the number of clicks on it before it was removed.

  # UI for a single filter item for a filter variable ----
  ui_single_filter_item <- function(id_prefix, filter_char, filter_state, prelabel) {
    stopifnot(is_character_single(id_prefix))
    stopifnot(is_character_single(prelabel))

    remove_filter_id <- paste0(id_prefix, "_remove_filter")
    selection_id <- paste0(id_prefix, "_selection")
    keep_na_id <- paste0(id_prefix, "_keepNA")

    # label before select input and button to remove filter
    varlabel <- tagList(
      tags$span(
        prelabel,
        # todo1: remove most of these checks
        if (!(is.null(filter_char$label) || is.na(filter_char$label) || is_empty(filter_char$label) || filter_char$label == "")) {
          tags$small(filter_char$label, style = "font-weight:normal; margin-left:3px")
        }
      ),
      actionLink(
        remove_filter_id, "", icon("trash-alt", lib = "font-awesome"),
        class = "remove"
      ),
      if (!is.null(filter_char$na_count) && filter_char$na_count > 0) {
        checkboxInput(keep_na_id, paste0("Keep NA (", filter_char$na_count, ")"), value = filter_state$keep_na)
      }
    )

    if (filter_char$type == "choices") {
      if (length(filter_char$choices) > 5) {
        pickerInput(
          selection_id,
          varlabel,
          choices =  filter_char$choices,
          selected = filter_state$selection,
          multiple = TRUE,
          options = pickerOptions(
            actionsBox = TRUE,
            liveSearch = (length(filter_char$choices) > 10),
            noneSelectedText = "Select a value"
          ),
          width = "100%"
        )
      } else {
        checkboxGroupInput(
          selection_id,
          varlabel,
          choices =  filter_char$choices,
          selected = filter_state$selection,
          width = "100%"
        )
      }
    } else if (filter_char$type == "range") {
      sliderInput(
        selection_id,
        varlabel,
        min = floor(filter_char$range[1] * 100) / 100,
        max = ceiling(filter_char$range[2] * 100) / 100,
        value = filter_state$selection,
        width = "100%"
      )
    } else if (filter_char$type == "logical") {
      radioButtons(
        selection_id,
        varlabel,
        choices = filter_char$choices,
        selected = filter_state$selection,
        width = "100%"
      )
    } else {
      # fail gracefully although this should have been caught before already
      tags$p(paste(varname, "in data", dataname, "has unknown type:", filter_char$type))
    }
  }

  # we only trigger re-rendering of the UI when the vars in the filter state change,
  # not when their state changes; otherwise, changing the filter for a var would trigger
  # regeneration of the UI which results in delays
  filtervarnames <- reactiveVal(NULL)
  observe({
    current_filtervarnames <- names(datasets$get_filter_state(dataname))
    if (!identical(filtervarnames(), current_filtervarnames)) {
      filtervarnames(current_filtervarnames)
    }
  })

  # dynamic ui part ----
  output$filters <- renderUI({
    filtervarnames() # dependence when names of filter vars change

    .log("generating ui filters for data", dataname)
    filter_chars <- isolate(datasets$get_filter_chars(dataname))
    var_states <- isolate(datasets$get_filter_state(dataname))

    ns <- session$ns
    return(do.call(container, unname(Map(
      function(varname, filter_char, filter_state) ui_single_filter_item(
        id_prefix = ns(varname), filter_char = filter_char, filter_state = filter_state,
        prelabel = paste0(dataname, ".", varname)
      ),
      varname = names(filter_chars), # also used as id
      filter_char = filter_chars,
      filter_state = var_states
    ))))
  })

  # dynamic server part for UI ----

  # we need to keep a list of current observers so we can destroy them when the UI is regenerated dynamically
  # must destroy the observer so that it does not keep deleting when the var is added again (which
  # would then immediately be deleted again)
  active_observers <- list()
  observeEvent(filtervarnames(), {
    .log("generating server part for filters for data", dataname)

    lapply(active_observers, function(o) o$destroy())
    active_observers <<- list()

    lapply(filtervarnames(), function(varname) {
      # change filter for variable
      id_selection <- paste0(varname, "_selection")
      id_keepna <- paste0(varname, "_keepNA")
      o1 <- observeEvent(
        {
          input[[id_selection]]
          input[[id_keepna]]
        },
        {
          selection_state <- input[[id_selection]]
          type <- datasets$get_filter_type(dataname, varname)
          if (type == "choices") {
            # unfortunately, NULL is also returned for a select when nothing is selected
            # in a multiple checkbox, so we need to set it manually to character(0)
            selection_state <- if (is.null(selection_state)) character(0) else selection_state
          }
          if (is.null(selection_state)) {
            # NULL initially before UI is initialized, but we cannot ignore NULLs for
            # selectInputs as they mean all unselected
            return()
          }
          keep_na_state <- if_null(input[[id_keepna]], FALSE) # may be NULL when var contains no NA
          state <- list(selection = selection_state, keep_na = keep_na_state)
          .log("State:", state)
          datasets$set_filter_state(dataname, varname, state)
        },
        ignoreNULL = FALSE, # ignoreNULL: we don't want to ignore NULL when nothing is selected,
        ignoreInit = TRUE # ignoreInit: should not matter because we set the UI with the desired initial state
      )

      # remove variable
      id_remove <- paste0(varname, "_remove_filter")
      o2 <- observeEvent(
        input[[id_remove]],
        {
          datasets$remove_filter(dataname, varname)
        },
        # the button is created dynamically afterwards, so this will trigger although the user has not clicked, see the doc
        ignoreInit = TRUE
      )
      active_observers <<- c(active_observers, list(o1, o2))
    })
  })

  return(NULL)
}
