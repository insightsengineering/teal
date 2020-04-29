#' Creates UI to show currently selected filters that can be applied to
#' the dataset, i.e. all columns selected for filtering are shown.
#'
#' @param id module id
#' @param dataname `character` name of dataset that is filtered
#' @md
#'
ui_filter_items <- function(id, dataname) {
  ns <- NS(id)
  div(
    id = ns(character(0)), # needed to assign an id, so filter can be shown / hidden
    uiOutput(ns("filters"))
  )
}

#' @importFrom shinyWidgets pickerInput pickerOptions
srv_filter_items <- function(input, output, session, datasets, dataname, container = div) {
  # This whole function is complicated because the UI must be created dynamically from the
  # selection of filters.
  # Any observers previously registered during dynamic creation must be deleted on the
  # next dynamic registration of observers.
  # When a UI element with an id is added and removed, its input element still stays there
  # When it is readded, the input elements still are the old ones before it was removed,
  # e.g. the button input starts at the number of clicks on it before it was removed.
  # When the filter is changed from the UI, this sets the state in the FilteredData.
  # This in turns triggers the UI regeneration which reflects the new state (and should
  # leave the UI in the same state because nothing changed, unless the state does not capture
  # the full UI state). This triggers the `set_filter_state` function again, but stops there
  # because the underlying reactiveVal only triggers when its value changes (unlike reactive).

  # UI for a single filter item for a filter variable ----
  ui_single_filter_item <- function(id_prefix, filter_info, filter_state, prelabel) {
    stopifnot(is_character_single(id_prefix))
    stopifnot(is_character_single(prelabel))

    remove_filter_id <- paste0(id_prefix, "_remove_filter")
    selection_id <- paste0(id_prefix, "_selection")
    keep_na_id <- paste0(id_prefix, "_keepNA")

    # label before select input and button to remove filter
    varlabel <- tagList(
      tags$span(
        prelabel,
        if (!is.null(filter_info$label) || (filter_info$label != "")) {
          tags$small(filter_info$label, style = "font-weight:normal; margin-left:3px")
        }
      ),
      actionLink(
        remove_filter_id, "", icon("trash-alt", lib = "font-awesome"),
        class = "remove"
      ),
      if (!is.null(filter_info$na_count) && filter_info$na_count > 0) {
        checkboxInput(keep_na_id, paste0("Keep NA (", filter_info$na_count, ")"), value = filter_state$keep_na)
      }
    )

    if (filter_info$type == "choices") {
      if (length(filter_info$choices) > 5) {
        pickerInput(
          selection_id,
          varlabel,
          choices = filter_info$choices,
          selected = filter_state$selection,
          multiple = TRUE,
          options = pickerOptions(
            actionsBox = TRUE,
            liveSearch = (length(filter_info$choices) > 10),
            noneSelectedText = "Select a value"
          ),
          width = "100%"
        )
      } else {
        checkboxGroupInput(
          selection_id,
          varlabel,
          choices =  filter_info$choices,
          selected = filter_state$selection,
          width = "100%"
        )
      }
    } else if (filter_info$type == "range") {
      sliderInput(
        selection_id,
        varlabel,
        # when rounding, we must make sure that we don't set the slider to an invalid state
        min = floor(filter_info$range[1] * 100) / 100,
        max = ceiling(filter_info$range[2] * 100) / 100,
        value = filter_state$selection,
        width = "100%"
      )
    } else if (filter_info$type == "logical") {
      radioButtons(
        selection_id,
        varlabel,
        choices = filter_info$choices,
        selected = filter_state$selection,
        width = "100%"
      )
    } else {
      # fail gracefully although this should have been caught before already
      tags$p(paste("For varlabel in", varlabel, "in data", dataname, "has unknown type:", filter_info$type))
    }
  }

  # dynamic ui part ----
  output$filters <- renderUI({
    .log("generating ui filters for data", dataname)
    filter_infos <- datasets$get_filter_info(dataname)
    var_states <- datasets$get_filter_state(dataname)

    ns <- session$ns
    return(do.call(container, unname(Map(
      function(varname, filter_info, filter_state) {
        ui_single_filter_item(
          id_prefix = ns(varname), filter_info = filter_info, filter_state = filter_state,
          prelabel = paste0(dataname, ".", varname)
        )
      },
      varname = names(filter_infos), # also used as id
      filter_info = filter_infos,
      filter_state = var_states
    ))))
  })

  # dynamic server part for UI ----

  # we need to keep a list of current observers so we can destroy them when the UI is regenerated dynamically
  # must destroy the observer so that it does not keep deleting when the var is added again (which
  # would then immediately be deleted again)
  active_observers <- list()
  observe({
    .log("generating server part for filters for data", dataname)

    lapply(active_observers, function(o) o$destroy())
    active_observers <<- list()

    # over all filtered variables
    lapply(names(datasets$get_filter_state(dataname)), function(varname) {
      # change filter for variable
      id_selection <- paste0(varname, "_selection")
      id_keepna <- paste0(varname, "_keepNA")
      o1 <- observeEvent({
          input[[id_selection]]
          input[[id_keepna]]
        }, {
          selection_state <- input[[id_selection]]
          type <- datasets$get_filter_type(dataname, varname)
          if (type == "choices") {
            # unfortunately, NULL is also returned for a select when nothing is selected
            # in a multiple checkbox, so we need to set it manually to character(0)
            selection_state <- if (is.null(selection_state)) character(0) else selection_state
          } else if (type == "range") {
            # we must make sure to truncate the state because the slider range is similar to
            # [round(min(range)), round(max(range))]. Therefore, it may be outside the range
            if (!is.null(selection_state)) {
              real_range <- datasets$get_filter_info(dataname, varname)$range
              selection_state <- c(
                max(selection_state[[1]], real_range[[1]]),
                min(selection_state[[2]], real_range[[2]])
              )
            }
          }
          if (is.null(selection_state)) {
            # NULL initially before UI is initialized, but we cannot ignore NULLs for
            # selectInputs as they mean all unselected
            return()
          }
          keep_na_state <- if_null(input[[id_keepna]], FALSE) # input field may not exist if var contains no NA
          state <- list(selection = selection_state, keep_na = keep_na_state)
          .log("State:", state$selection[1:min(length(state$selection), 8)]) # truncate the output
          datasets$set_filter_state(dataname, varname, state)
        },
        ignoreNULL = FALSE, # ignoreNULL: we don't want to ignore NULL when nothing is selected,
        ignoreInit = TRUE # ignoreInit: should not matter because we set the UI with the desired initial state
      )

      # remove variable
      id_remove <- paste0(varname, "_remove_filter")
      o2 <- observeEvent(
        input[[id_remove]], {
          datasets$set_filter_state(dataname, varname, state = NULL)
        },
        # the button is created dynamically afterwards, so this will trigger although
        # the user has not clicked, see the doc
        ignoreInit = TRUE
      )
      active_observers <<- c(active_observers, list(o1, o2))
    })
  })

  # returning observers so you can cancel them in a similar fashion as in this module when integrating this module
  # into another dynamic module
  return(active_observers)
}
