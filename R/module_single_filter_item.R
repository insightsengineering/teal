# UI for a single filter item for a filter variable ----
ui_single_filter_item <- function(id, filter_info, filter_state, prelabel) {
  ns <- NS(id)
  stopifnot(is_character_single(prelabel))

  remove_filter_id <- ns("remove_filter")
  selection_id <- ns("selection")
  keep_na_id <- ns("keepNA")

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
        selected = filter_state$choices,
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
        selected = filter_state$choices,
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
      value = filter_state$range,
      width = "100%"
    )
  } else if (filter_info$type == "logical") {
    radioButtons(
      selection_id,
      varlabel,
      choices = filter_info$choices,
      selected = filter_state$status,
      width = "100%"
    )
  } else {
    # fail gracefully although this should have been caught before already
    tags$p(paste("For varlabel in", varlabel, "in data", dataname, "has unknown type:", filter_info$type))
  }
}


srv_single_filter_item <- function(input, output, session, datasets, dataname, varname) {
  stopifnot(
    is(datasets, "FilteredData"),
    is_character_single(dataname),
    is_character_single(varname)
  )

  id_selection <- "selection"
  id_keepna <- "keepNA"
  o1 <- observeEvent({
    input[[id_selection]]
    input[[id_keepna]]
  }, {
    selection_state <- input[[id_selection]]
    type <- datasets$get_filter_type(dataname, varname)
    state <- if (type == "choices") {
      # unfortunately, NULL is also returned for a select when nothing is selected
      # in a multiple checkbox, so we need to set it manually to character(0)
      list(
        choices = if (is.null(selection_state)) character(0) else selection_state
      )
    } else if (type == "range") {
      # we must make sure to truncate the state because the slider range is similar to
      # [round(min(range)), round(max(range))]. Therefore, it may be outside the range
      stopifnot(is_numeric_vector(selection_state), length(selection_state) == 2)
      real_range <- datasets$get_filter_info(dataname, varname)$range
      list(
        range = c(
          max(selection_state[[1]], real_range[[1]]),
          min(selection_state[[2]], real_range[[2]])
        )
      )
    } else if (type == "logical") {
      list(status = selection_state)
    } else {
      stop("Unknown filter type ", type, " for var ", varname)
    }
    keep_na_state <- if_null(input[[id_keepna]], FALSE) # input field may not exist if var contains no NA
    state <- c(state, list(keep_na = keep_na_state))
    .log("State for ", varname, ":", filter_state_to_str(type, state)) # truncate the output
    datasets$set_filter_state(dataname, varname, state)
  },
  ignoreNULL = FALSE, # ignoreNULL: we don't want to ignore NULL when nothing is selected,
  ignoreInit = TRUE # ignoreInit: should not matter because we set the UI with the desired initial state
  )

  # remove variable
  id_remove <- "remove_filter"
  o2 <- observeEvent(
    input[[id_remove]], {
      datasets$set_filter_state(dataname, varname, state = NULL)
    },
    # the button is created dynamically afterwards, so this will trigger although
    # the user has not clicked, see the doc
    ignoreInit = TRUE
  )

  return(list(o1, o2)) # so we can cancel them
}
