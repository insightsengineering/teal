# Module to change the filter for a single variable

.threshold_slider_vs_checkboxgroup <- 5 #nolint


# label of checkbox to keep / remove NAs
get_keep_na_label <- function(na_count) {
  paste0("Keep NA (", na_count, ")")
}

#' UI to filter a single filter variable
#'
#' We pass in the initial state to avoid filtering twice that otherwise comes
#' with the `updateInput` functions.
#'
#' @md
#' @param id module id
#' @param filter_info `filter_info` returned by datasets class
#' @param filter_state `filter_state` returned by datasets class
#' @param prelabel `character` label to append before computed label of input
#'
#' @examples
#' library(random.cdisc.data)
#' library(dplyr)
#'
#' ADSL <- radsl(cached = TRUE)
#' attr(ADSL, "keys") <- get_cdisc_keys("ADSL")
#'
#' datasets <- teal:::FilteredData$new()
#' isolate({
#'   datasets$set_data("ADSL", ADSL)
#'   datasets$set_filter_state("ADSL", list(
#'     AGE = list(range = c(33, 44), keep_na = FALSE)
#'   ))
#' })
#'
#' app <- shinyApp(ui = function() {
#'   tagList(
#'     teal:::include_teal_css_js(),
#'     isolate(teal:::ui_single_filter_item(
#'       "var_AGE",
#'       datasets$get_filter_info("ADSL", "AGE"),
#'       datasets$get_filter_state("ADSL", "AGE"),
#'       prelabel = "ADSL.AGE"
#'     ))
#'   )
#' }, server = function(input, output, session) {
#'   callModule(teal:::srv_single_filter_item, "var_AGE", datasets, "ADSL", "AGE")
#' })
#'
#' \dontrun{
#' runApp(app)
#' }
#'
ui_single_filter_item <- function(id, filter_info, filter_state, prelabel) {
  stopifnot(
    is.list(filter_info),
    is.list(filter_state),
    is_character_single(prelabel)
  )

  ns <- NS(id)
  stopifnot(is_character_single(prelabel))

  id_remove_filter <- ns("remove_filter")
  id_selection <- ns("selection")
  id_keep_na <- ns("keepNA")

  # we set label to NULL everywhere, so we can set the label column ourselves
  select_input <- if (filter_info$type == "choices" || filter_info$type == "integer_flag") {
    if (length(filter_info$choices) <= .threshold_slider_vs_checkboxgroup) {
      div(
        style = "position: relative;",
        div(
          class = "filterPlotOverlayBoxes",
          plotOutput(ns("plot"), height = "100%")
        ),
        checkboxGroupInput(
          id_selection,
          label = NULL,
          choices =  filter_info$choices,
          selected = filter_state$choices,
          width = "100%"
        )
      )
    } else {
      pickerInput(
        id_selection,
        label = NULL,
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
    }
  } else if (filter_info$type == "range") {
    div(
      div(
        class = "filterPlotOverlayRange",
        plotOutput(ns("plot"), height = "100%")
      ),
      sliderInput(
        id_selection,
        label = NULL,
        # `round()` may round to a slightly smaller interval (with negative numbers), so
        # we avoid this and then truncate it when we read the input in the server function
        min = floor(filter_info$range[[1]] * 100) / 100,
        max = ceiling(filter_info$range[[2]] * 100) / 100,
        value = filter_state$range,
        width = "100%"
      )
    )
  } else if (filter_info$type == "logical") {
    div(
      style = "position: relative;",
      # same overlay as for choices with no more than (default: 5) elements
      div(
        class = "filterPlotOverlayBoxes",
        plotOutput(ns("plot"), height = "100%")
      ),
      radioButtons(
        id_selection,
        label = NULL,
        choices = filter_info$choices,
        selected = filter_state$status,
        width = "100%"
      )
    )
  } else {
    # fail gracefully although this should have been caught before already
    tags$p(paste("Variable with id", id, "has unknown type:", filter_info$type))
  }

  # label before select input and button to remove filter
  res <- fluidPage(
    fluidRow(
      column(8, class = "no-left-right-padding", tags$span(
        tags$span(prelabel, class = "filter_panel_varname"),
        if (!is.null(filter_info$label) || (filter_info$label != "")) {
          tags$span(filter_info$label, class = "filter_panel_varlabel")
        }
      )),
      column(4, class = "no-left-right-padding", actionLink(
        id_remove_filter, "", icon("trash-alt", lib = "font-awesome"),
        class = "remove pull-right"
      ))
    ),
    fluidRow(select_input),
    fluidRow(
      checkboxInput(id_keep_na, get_keep_na_label(filter_info$na_count), value = filter_state$keep_na)
    )
  )
  return(res)
}

#' Server function to filter for a single variable
#'
#' Regarding the return value: The `observers` are returned so they can be canceled
#' when the module is removed.
#'
#' @md
#' @inheritParams srv_shiny_module_arguments
#' @param dataname `character` dataname
#' @param varname `character` variable within `dataname` to filter
#'
#' @return `reactive` which returns `list(observers = ...)` with registered observers.
#'
#' @importFrom grDevices rgb
#' @importFrom ggplot2 ggplot aes_string geom_area theme_void scale_y_continuous scale_x_continuous geom_col
#' @importFrom ggplot2 coord_flip scale_x_discrete
srv_single_filter_item <- function(input, output, session, datasets, dataname, varname) {
  stopifnot(
    is(datasets, "FilteredData"),
    is_character_single(dataname),
    is_character_single(varname)
  )

  # compute plot that might overlay along with filtering (e.g. histogram) ----

  # we have to make this outside the if because plot options may be different per variable type
  var_type <- isolate(datasets$get_filter_type(dataname, varname))

  output$plot <- if (var_type == "choices" || var_type == "logical" || var_type == "integer_flag") {
    renderPlot(bg = "transparent", {
      filter_info <- datasets$get_filter_info(dataname, varname)
      if ((length(filter_info$choices) <= .threshold_slider_vs_checkboxgroup) || (var_type == "logical")) {
        # Proportional
        data <- filter_info$histogram_data
        data$y <- rev(data$y / sum(data$y)) # we have to reverse because the histogram is turned by 90 degrees
        ggplot(data) +
          # sort factor so that it reflects checkbox order
          aes_string(x = "x", y = "y") +
          geom_col(
            width = 0.95,
            fill = rgb(66 / 255, 139 / 255, 202 / 255),
            color = NA,
            alpha = 0.2
          ) +
          coord_flip() +
          theme_void() +
          scale_x_discrete(expand = c(0, 0)) +
          scale_y_continuous(expand = c(0, 0), limits = c(0, 1))
      }
    })
  } else if (var_type == "range") {
    renderPlot(
      bg = "transparent",
      height = 25, {
        filter_info <- datasets$get_filter_info(dataname, varname)
        ggplot(filter_info$histogram_data) +
          aes_string(x = "x", y = "y") +
          geom_area(
            fill = rgb(66 / 255, 139 / 255, 202 / 255),
            color = NA,
            alpha = 0.2) +
          theme_void() +
          scale_y_continuous(expand = c(0, 0)) +
          scale_x_continuous(expand = c(0, 0))
      })
  } else {
    # no plot generated
    NULL
  }

  # define observers ----
  id_selection <- "selection"
  id_keep_na <- "keepNA"
  id_remove_filter <- "remove_filter"

  # observers for Browser UI state -> FilteredData filter_state ----
  o1 <- observeEvent({
    input[[id_selection]]
    input[[id_keep_na]]
  }, {
    selection_state <- input[[id_selection]]
    type <- datasets$get_filter_type(dataname, varname)
    state <- if (type == "choices" || type == "integer_flag") {
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
    keep_na_state <- if_null(input[[id_keep_na]], FALSE) # input field may not exist if variable contains no `NA`
    state <- c(state, list(keep_na = keep_na_state))
    .log("State for ", varname, ":", filter_state_to_str(type, state)) # truncates the output if too much
    set_single_filter_state(datasets, dataname = dataname, varname = varname, state = state)
  },
  ignoreNULL = FALSE, # ignoreNULL: we don't want to ignore NULL when nothing is selected in the `selectInput`,
  ignoreInit = TRUE # ignoreInit: should not matter because we set the UI with the desired initial state
  )

  # remove variable
  o2 <- observeEvent(input[[id_remove_filter]], {
      set_single_filter_state(datasets, dataname = dataname, varname = varname, state = NULL)
    },
    # the button is created dynamically afterwards, so this will trigger although
    # the user has not clicked, see the doc
    ignoreInit = TRUE
  )

  return(list(observers = list(o1, o2))) # so we can cancel them
}
