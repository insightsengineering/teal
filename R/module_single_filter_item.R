#todo: document
# UI for a single filter item for a filter variable ----
ui_single_filter_item <- function(id, filter_info, filter_state, prelabel) {
  ns <- NS(id)
  stopifnot(is_character_single(prelabel))

  remove_filter_id <- ns("remove_filter")
  selection_id <- ns("selection")
  keep_na_id <- ns("keepNA")

  # we set label to NULL everywhere, so we can set the label column ourselves
  select_input <- if (filter_info$type == "choices") {
    if (length(filter_info$choices) <= 5) {
      div(
        style = "position: relative;",
        div(
          style = "
        position: absolute;
        top: -2px; right: 16px; bottom: -2px; left: 16px;
        animation:
          0.75s ease-out 0s 1 shinyDataFilterEnlargeX,
          0.5s ease-in  0s 1 shinyDataFilterFadeIn;
        transform-origin: left;",
          plotOutput(ns("plot"), height = "100%")
        ),
        checkboxGroupInput(
          selection_id,
          label = NULL,
          choices =  filter_info$choices,
          selected = filter_state$choices,
          width = "100%"
        )
      )
    } else {
      pickerInput(
        selection_id,
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
        style = "
          margin: 0px 11px -25px 11px;
          height: 25px;
          animation:
            0.75s ease-out 0s 1 shinyDataFilterEnlargeY,
            0.5s ease-in  0s 1 shinyDataFilterFadeIn;
          transform-origin: bottom;",
        shiny::plotOutput(ns("plot"), height = "100%")),
      sliderInput(
        selection_id,
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
        style = "
        position: absolute;
        top: -2px; right: 16px; bottom: -2px; left: 16px;
        animation:
          0.75s ease-out 0s 1 shinyDataFilterEnlargeX,
          0.5s ease-in  0s 1 shinyDataFilterFadeIn;
        transform-origin: left;",
        plotOutput(ns("plot"), height = "100%")
      ),
      radioButtons(
        selection_id,
        label = NULL,
        choices = filter_info$choices,
        selected = filter_state$status,
        width = "100%"
      )
    )
  } else {
    # fail gracefully although this should have been caught before already
    tags$p(paste("Variable with label", varlabel, "in data", dataname, "has unknown type:", filter_info$type))
  }

  # label before select input and button to remove filter
  return(div(
    fluidRow(
      #height = "40px",
      column(1, shiny::icon("grip-vertical", class = "sortableJS-handle")), #todo
      column(1, tags$span(
        prelabel,
        if (!is.null(filter_info$label) || (filter_info$label != "")) {
          tags$small(filter_info$label, style = "font-weight:normal; margin-left:3px")
        }
      )),
      if (!is.null(filter_info$na_count) && filter_info$na_count > 0) {
        make_inline <- function(x) {
          x$name <- "span"
          x$attribs$class <- "form-group shiny-input-container-inline"
          stopifnot(length(x$children) == 1)
          x$children[[1]]$name <- "span"
          return(x)
        } # todo: not working
        column(2, make_inline(checkboxInput(keep_na_id, paste0("Keep NA (", filter_info$na_count, ")"), value = filter_state$keep_na)))
      },
      column(4, actionLink(
        remove_filter_id, "", icon("trash-alt", lib = "font-awesome"),
        class = "remove"
      ))
    ),
    fluidRow(select_input)
  ))
}


# todo: give other arguments, only those really needed, possibly transformed, e.g. only for varname
srv_single_filter_item <- function(input, output, session, datasets, dataname, varname) {
  stopifnot(
    is(datasets, "FilteredData"),
    is_character_single(dataname),
    is_character_single(varname)
  )

  # define plot that might overlay along with filtering (e.g. histogram)

  # we have to make this outside the if because plot options may be different per variable type
  var_type <- isolate(datasets$get_filter_type(dataname, varname))
  output$plot <- if ((var_type == "choices") || (var_type == "logical")) {
    renderPlot(bg = "transparent", {
      filter_info <- datasets$get_filter_info(dataname, varname)
      if ((length(filter_info$choices) <= 5) || (var_type == "logical")) {
        # Proportional
        # todo: replace by real data
        nb_vars <- length(datasets$get_filter_info(dataname, varname)$choices)
        fake_data <- data.frame(x = letters[1:nb_vars], y = sample(1:10 / 10, nb_vars, replace = TRUE)) # in [0,1] range for plot
        ggplot(fake_data) +
          # sort factor so that it reflects checkbox order
          aes_string(x = "x", y = "y") +
          geom_col(width = 0.95,
                            fill = grDevices::rgb(66 / 255, 139 / 255, 202 / 255),
                            color = NA,
                            alpha = 0.2) +
          coord_flip() +
          theme_void() +
          scale_x_discrete(expand = c(0, 0)) +
          scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) # todo: adapt limits
      }
    })
  } else if (var_type == "range") {
    shiny::renderPlot(
      bg = "transparent",
      height = 25, {
        density <- stats::density(mtcars$cyl, na.rm = TRUE)
        fake_data <- data.frame(x = density$x, y = density$y)
        ggplot2::ggplot(fake_data) +
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


  # define observers for buttons ----

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
