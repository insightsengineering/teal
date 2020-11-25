# Module to change the filter for a single variable

.threshold_slider_vs_checkboxgroup <- 5 #nolint


# label of checkbox to keep / remove NAs
get_keep_na_label <- function(na_count) {
  paste0("Keep NA (", na_count, ")")
}

# label of checkbox to keep / remove NAs
get_keep_inf_label <- function(inf_count) {
  paste0("Keep Inf (", inf_count, ")")
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
#' @importFrom shinyWidgets airDatepickerInput
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
  id_end_date <- ns("end_date")
  id_keep_na <- ns("keepNA")
  id_keep_inf <- ns("keepInf")

  # we set label to NULL everywhere, so we can set the label column ourselves
  select_input <- if (filter_info$type == "choices") {
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
      optionalSelectInput(
        inputId = id_selection,
        label = NULL,
        choices = filter_info$choices,
        selected = filter_state$choices,
        multiple = TRUE,
        options = pickerOptions(
          actionsBox = TRUE,
          liveSearch = (length(filter_info$choices) > 10),
          noneSelectedText = "Select a value"
        )
      )
    }
  } else if (filter_info$type == "range") {
    div(
      div(
        class = "filterPlotOverlayRange",
        plotOutput(ns("plot"), height = "100%")
      ),
      optionalSliderInput(
        inputId = id_selection,
        label = NULL,
        # `round()` may return a slightly smaller interval e.g. round(c(-0.3, 1.4)) for doubles
        min = filter_info$range[[1]],
        max = filter_info$range[[2]],
        value = filter_state$range,
        width = "100%",
        step = if (filter_info$is_integer) 1L
      ),
      if (filter_info$inf_count > 0) {
        checkboxInput(id_keep_inf,
                      get_keep_inf_label(filter_info$inf_count),
                      value = filter_state$keep_inf)
      } else {
        NULL
      }
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
  } else if (filter_info$type == "date") {
    daterange <- filter_info$daterange

    if (filter_info$is_datetime) {
      div(
        airDatepickerInput(
          inputId = id_selection,
          label = div(
            "From:",
            title = "Times are displayed in the local timezone and are converted to UTC in the analysis",
            icon("info-circle")
          ),
          value = daterange[[1]],
          timepicker = TRUE,
          minDate = daterange[[1]],
          maxDate = daterange[[2]],
          update_on = "close"
        ),
        airDatepickerInput(
          inputId = id_end_date,
          label = div(
            "To:",
            title = "Times are displayed in the local timezone and are converted to UTC in the analysis",
            icon("info-circle")
          ),
          value = daterange[[2]],
          timepicker = TRUE,
          minDate = daterange[[1]],
          maxDate = daterange[[2]],
          update_on = "close"
        )
      )
    } else {
      dateRangeInput(
        inputId = id_selection,
        label = NULL,
        start = daterange[[1]],
        end = daterange[[2]],
        min = daterange[[1]],
        max = daterange[[2]]
        )
    }
  } else if ("all_na" %in% names(filter_info)) {
    "All values missing - no filtering possible"
  } else {
    paste0("Filtering variable of class ", filter_info$class[1], " is not yet supported")
  }

  # label before select input and button to remove filter
  res <- fluidPage(
    fluidRow(
      column(10, class = "no-left-right-padding",
        tags$div(
        tags$span(prelabel, class = "filter_panel_varname"),
        if (!is.null(filter_info$label) || (filter_info$label != "")) {
          tagList(
            tags$br(),
            tags$span(filter_info$label, class = "filter_panel_varlabel"))
        }
        )
      ),
      column(2, class = "no-left-right-padding", actionLink(
        id_remove_filter, "", icon("trash-alt", lib = "font-awesome"),
        class = "remove pull-right"
      ))
    ),
    fluidRow(select_input),
    if (filter_info$na_count > 0 && filter_info$type != "unknown") {
      fluidRow(
        checkboxInput(id_keep_na, get_keep_na_label(filter_info$na_count), value = filter_state$keep_na)
      )
    } else {
      NULL
    }
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

  get_client_timezone(session$ns)

  # compute plot that might overlay along with filtering (e.g. histogram) ----

  # we have to make this outside the if because plot options may be different per variable type
  var_type <- isolate(datasets$get_filter_type(dataname, varname))

  output$plot <- if (var_type == "choices" || var_type == "logical") {
    renderPlot(bg = "transparent", {
      filter_info <- datasets$get_filter_info(dataname, varname)
      if ((length(filter_info$choices) <= .threshold_slider_vs_checkboxgroup) || (var_type == "logical")) {
        # Proportional
        data <- filter_info$histogram_data
        data$y <- rev(data$y / sum(data$y)) # we have to reverse because the histogram is turned by 90 degrees
        data$x <- seq_len(nrow(data)) # to prevent ggplot reordering columns using the characters in x column
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
  id_end_date <- "end_date"
  id_keep_na <- "keepNA"
  id_keep_inf <- "keepInf"
  id_remove_filter <- "remove_filter"

  # observers for Browser UI state -> FilteredData filter_state ----
  o1 <- observeEvent({
    input[[id_selection]]
    input[[id_end_date]]
    input[[id_keep_inf]]
    input[[id_keep_na]]
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
    } else if (type == "date") {
      start_date <- selection_state[[1]]
      end_date <- if (datasets$get_filter_info(dataname, varname)$is_datetime) {
        input[[id_end_date]]
      } else {
        selection_state[[2]]
      }

      validate(
        need(start_date, "Please select a valid start date."),
        need(end_date, "Please select a valid end date.")
        )

      stopifnot(
        inherits(start_date, "Date") || inherits(start_date, "POSIXct"),
        inherits(end_date, "Date") || inherits(end_date, "POSIXct")
        )

      list(daterange = c(start_date, end_date), timezone = input$tz)

    } else if (type == "unknown") {
      list(NULL)
    } else {
      stop("Unknown filter type ", type, " for var ", varname)
    }

    keep_na_state <- if_null(input[[id_keep_na]], FALSE) # input field may not exist if variable contains no `NA`
    keep_inf_state <- if_null(input[[id_keep_inf]], FALSE)

    state <- c(state, list(keep_na = keep_na_state, keep_inf = keep_inf_state))
    .log("State for ", varname, ":", filter_state_to_str(type, state)) # truncates the output if too much

    set_single_filter_state(datasets,
                            dataname = dataname,
                            varname = varname,
                            state = state)
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
