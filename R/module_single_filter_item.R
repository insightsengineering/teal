# label of checkbox to keep / remove NAs
get_keep_na_label <- function(na_count) paste0("Keep NA (", na_count, ")")

#' UI for a single filter item for a filter variable
#'
#' We pass in the initial state to avoid filtering twice that otherwise comes
#' with the `updateInput` functions.
#'
#' @md
#' @param filter_info `filter_info` returned by datasets class
#' @param filter_state `filter_state` returned by datasets class
#' @param prelabel `character` label to append before computed label of input
#'
#' @examples
#' library(random.cdisc.data)
#'
#' ADSL <- radsl(cached = TRUE)
#' attr(ADSL, "keys") <- get_cdisc_keys("ADSL")
#'
#' datasets <- teal:::FilteredData$new()
#' isolate({
#'   datasets$set_data("ADSL", ADSL)
#'   datasets$set_filter_state("ADSL", varname = NULL, list(
#'     AGE = list(range = c(33, 44), keep_na = FALSE)
#'   ))
#' })
#'
#' shinyApp(ui = function() {
#'   tagList(
#'     include_teal_css_js(),
#'     isolate(ui_single_filter_item(
#'       "var_AGE",
#'       datasets$get_filter_info("ADSL", "AGE"),
#'       datasets$get_filter_state("ADSL", "AGE"),
#'       prelabel = "ADSL.AGE"
#'     ))
#'   )
#' }, server = function(input, output, session) {
#'   callModule(srv_single_filter_item, "var_AGE", datasets, "ADSL", "AGE")
#' }) %>% invisible() # invisible so it does not run
#'
#'
#' # more complicated example which updates the dataset state from the server
#' # state to the browser overwriting anything done from the user in the meantime
#' library(random.cdisc.data)
#'
#' ADSL <- radsl(cached = TRUE)
#' attr(ADSL, "keys") <- get_cdisc_keys("ADSL")
#'
#' datasets <- teal:::FilteredData$new()
#' isolate({
#'   datasets$set_data("ADSL", ADSL)
#'   datasets$set_filter_state("ADSL", varname = NULL, list(
#'     AGE = list(range = c(33, 44), keep_na = FALSE)
#'   ))
#' })
#'
#' # note that the remove button does not do anything because the UI is not
#' # dynamically updating with the filter state for this single module
#' # This logic is implemented in `module_filter_items`.
#' # sodo3: there is a bug with ITTFL where there is a single choice and it
#' # is converted to a character vector instead of staying a named list
#' filter_var <- "AGE"
#' shinyApp(ui = function() {
#'   # we use isolate because the app is initialized with these values
#'   # and they are dynamically changed by the server afterwards
#'   div(
#'     include_teal_css_js(),
#'     isolate(ui_single_filter_item(
#'       "var_filter", datasets$get_filter_info("ADSL", filter_var),
#'       datasets$get_filter_state("ADSL", filter_var), prelabel = paste0("ADSL.", filter_var)
#'     )),
#'     actionButton("reset_min", "Regenerate slider value")
#'   )
#' }, server = function(input, output, session) {
#'   update_ui_trigger <- callModule(
#'     srv_single_filter_item, "var_filter", datasets, "ADSL", filter_var
#'   )$update_ui_trigger
#'   observeEvent(input$reset_min, {
#'     # wait to show that changes by the user while this is running are discarded
#'     Sys.sleep(2)
#'     # for this to work, filter_var should be a numeric range variable
#'     new_state <- list(
#'       range = sort(sample(30:70, 2)), keep_na = sample(c(TRUE, FALSE), 1)
#'     )
#'     print(paste0("Setting new random state ", toString(new_state)))
#'     datasets$set_filter_state("ADSL", filter_var, new_state)
#'     update_ui_trigger()
#'   }, ignoreInit = TRUE)
#' }) %>% invisible() # invisible so it does not run
ui_single_filter_item <- function(id, filter_info, filter_state, prelabel) {
  ns <- NS(id)
  stopifnot(is_character_single(prelabel))

  id_remove_filter <- ns("remove_filter")
  id_selection <- ns("selection")
  id_keep_na <- ns("keepNA")

  # we set label to NULL everywhere, so we can set the label column ourselves
  select_input <- if (filter_info$type == "choices") {
    # this if should ideally be implemented in Javascript to also work when choices are updates from the server side
    if (length(filter_info$choices) <= 5) {
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
        shiny::plotOutput(ns("plot"), height = "100%")
      ),
      sliderInput(
        id_selection,
        label = NULL,
        # when rounding, we must make sure that we don't set the slider to an invalid state
        min = floor(filter_info$range[1] * 100) / 100,
        max = ceiling(filter_info$range[2] * 100) / 100,
        value = filter_state$range,
        width = "100%"
      )
    )
  } else if (filter_info$type == "logical") {
    div(
      style = "position: relative;",
      # same overlay as for choices with no more than 5 elements
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

  na_is_displayed <- !is.null(filter_info$na_count) && (filter_info$na_count > 0)
  # label before select input and button to remove filter
  return(fluidPage(
    fluidRow(
      column(1, shiny::icon("grip-vertical", class = "sortableJS-handle")), #todo
      column(if (na_is_displayed) 4 else 8, tags$span(
        prelabel,
        if (!is.null(filter_info$label) || (filter_info$label != "")) {
          tags$small(filter_info$label, style = "font-weight:normal; margin-left:3px")
        }
      )),
      if (na_is_displayed) {
        column(4, div(
          class = "nopadding", # removes padding around checkbox
          checkboxInput(id_keep_na, get_keep_na_label(filter_info$na_count), value = filter_state$keep_na)
        ))
      },
      column(4, actionLink(
        id_remove_filter, "", icon("trash-alt", lib = "font-awesome"),
        class = "remove"
      ))
    ),
    fluidRow(select_input)
  ))
}

srv_single_filter_item <- function(input, output, session, datasets, dataname, varname) {
  stopifnot(
    is(datasets, "FilteredData"),
    is_character_single(dataname),
    is_character_single(varname)
  )

  # compute plot that might overlay along with filtering (e.g. histogram) ----

  # we have to make this outside the if because plot options may be different per variable type
  var_type <- isolate(datasets$get_filter_type(dataname, varname))
  output$plot <- if ((var_type == "choices") || (var_type == "logical")) {
    renderPlot(bg = "transparent", {
      filter_info <- datasets$get_filter_info(dataname, varname)
      if ((length(filter_info$choices) <= 5) || (var_type == "logical")) {
        # Proportional
        data <- filter_info$histogram_data
        data$y <- rev(data$y / sum(data$y)) # we have to reverse because the histogram is turned by 90 degrees
        ggplot(data) +
          # sort factor so that it reflects checkbox order
          aes_string(x = "x", y = "y") +
          geom_col(width = 0.95,
                   fill = grDevices::rgb(66 / 255, 139 / 255, 202 / 255),
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
    shiny::renderPlot(
      bg = "transparent",
      height = 25, {
        filter_info <- datasets$get_filter_info(dataname, varname)
        ggplot2::ggplot(filter_info$histogram_data) +
          ggplot2::aes_string(x = "x", y = "y") +
          ggplot2::geom_area(
            fill = grDevices::rgb(66 / 255, 139 / 255, 202 / 255),
            color = NA,
            alpha = 0.2) +
          ggplot2::theme_void() +
          ggplot2::scale_y_continuous(expand = c(0, 0)) +
          ggplot2::scale_x_continuous(expand = c(0, 0))
      })
  } else {
    # no plot generated
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
    keep_na_state <- if_null(input[[id_keep_na]], FALSE) # input field may not exist if var contains no NA
    state <- c(state, list(keep_na = keep_na_state))
    .log("State for ", varname, ":", filter_state_to_str(type, state)) # truncate the output
    datasets$set_filter_state(dataname, varname, state)
  },
  ignoreNULL = FALSE, # ignoreNULL: we don't want to ignore NULL when nothing is selected,
  ignoreInit = TRUE # ignoreInit: should not matter because we set the UI with the desired initial state
  )

  # remove variable
  o2 <- observeEvent(
    input[[id_remove_filter]], {
      datasets$set_filter_state(dataname, varname, state = NULL)
    },
    # the button is created dynamically afterwards, so this will trigger although
    # the user has not clicked, see the doc
    ignoreInit = TRUE
  )

  # observers for FilteredData filter_state -> Browser UI state ----
  # to avoid bad user experience, e.g. when the user clicks a button, the server is calculating and
  # the user clicks again in the meantime, calling `updateInput` would destroy the user selection, so
  # we only update when it is explicitly requested by calling a reactive returned by this module:
  # `update_ui_trigger`
  update_ui_val <- reactiveVal(0)
  update_ui_trigger <- function() update_ui_val(update_ui_val() + 1)
  o3 <- observeEvent(update_ui_val(), {
    req(update_ui_val() > 0) # ignore init

    # does not react to changes of type and choices because the type of UI element is already rendered
    # this would require Javascript, similar to use `radioGroup` instead of `selectInput` for less than
    # five items
    filter_info <- datasets$get_filter_info(dataname, varname)
    filter_state <- datasets$get_filter_state(dataname, varname)
    type <- filter_info$type
    if (type == "choices") {
      if (length(filter_info$choices) <= 5) {
        updateCheckboxGroupInput(session, id_selection, choices = filter_info$choices, selected = filter_state$choices)
      } else {
        updateSelectInput(session, id_selection, choices = filter_info$choices, selected = filter_state$choices)
      }
    } else if (type == "range") {
      updateSliderInput(
        session, id_selection, min = filter_info$range[[1]],
        max = filter_info$range[[2]], value = filter_state$range
      )
    } else if (type == "logical") {
      updateRadioButtons(session, id_selection, choices = filter_info$status, selected = filter_state$status)
    } else {
      stop("Unknown filter type ", type, " for var ", varname)
    }

    updateCheckboxInput(
      session, id_keep_na, label = get_keep_na_label(filter_state$na_count), value = filter_state$keep_na
    )
  })

  return(list(observers = list(o1, o2, o3), update_ui_trigger = update_ui_trigger)) # so we can cancel them
}
