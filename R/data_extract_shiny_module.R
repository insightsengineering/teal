#------------------------------------------------------------------------------------------------------
#----------------------------------- UI Part ------------------------------------------------------
#------------------------------------------------------------------------------------------------------

#' teal Data Column Extraction module ui
#'
#' @param id (\code{character}) Shiny input ID
#' @param label (\code{character}) Label above the Data extract input
#' @param value (\code{data_extract}) This is the outcome of a \link{data_extract}
#'  function. It shall be used
#'
#' @return shiny Input that allows to define a filtering of key variables of
#'  a data set and the selection of non-key variables. This input is the UI function
#'  of the \link{data_extractor} server function.
#'
#' @import shiny
#'
data_extract_input <- function(id, label, value = data_extract()) {
  ns <- NS(id)

  # List of elements
  if (is.null(value$dataname)) {
    output_panel <- lapply(value, function(x) {
      dataname <- x$dataname
      conditionalPanel(
        condition = paste0("input['", ns("ds"), "'] == '", x$dataname, "'"),
        data_extract_input_single(id = ns(dataname), value = x)
      )
    }) # lapply
    datanames <- unlist(lapply(value, function(x) x$dataname))
  } else {
    output_panel <- list(data_extract_input_single(ns(value$dataname), value))

    datanames <- value$dataname
  }

  datanames %<>% setNames(datanames)
  if (!is.null(value)) {
    div(
      tags$label(label),
      optionalSelectInput(
        inputId = ns("ds"),
        label = "Dataset",
        choices = datanames,
        selected = datanames[1],
        multiple = FALSE
      ),
      build_extract_inputs(output_panel)
    )
  } else {
    div()
  }
}

#' @importFrom shinyjs hidden
#' @importFrom methods is
data_extract_input_single <- function(id = NULL, value = data_extract(), filtering_sep = " - ") {
  ns <- NS(id)

  stopifnot(methods::is(value, "data_extract"))

  div(
    # Construct a filtering for the key variables. In case no filtering
    # was set up return a logical false by a hidden checkbox
    if (!is.null(value$keys_filtering)) {
      shiny::tagList(
        optionalSelectInput(
          inputId = ns("filter"),
          label = if (!is.null(value$keys_filtering$label)) {
            value$keys_filtering$label
          } else {
            "Filter"
          },
          choices = filter2choices(value$keys_filtering$choices, filtering_sep),
          selected = filter2choices(value$keys_filtering$selected, filtering_sep),
          multiple = value$keys_filtering$multiple
        )
      )
    } else {
      shinyjs::hidden(shiny::checkboxInput(inputId = ns("filter"), " ", value = FALSE))
    },

    # Construct a Column selector in case a column selection should be possible by
    # users.
    if (value$columns$show) {
      optionalSelectInput(
        inputId = ns("column"),
        label = if (is.null(value$columns$label)) {
          "Column"
        } else {
          value$columns$label
        },
        choices = value$columns$choices,
        selected = value$columns$selected,
        multiple = value$columns$multiple
      )
    } else {
      helpText("Column:", tags$code(paste(value$columns$selected, collapse = " ")))
    }
  )
}

build_extract_inputs <- function(inputs) {
  do.call(div, inputs)
}

filter2choices <- function(list_of_filters, filtering_sep) {
  choices <- unlist(
    lapply(list_of_filters, function(x) paste0(x, collapse = filtering_sep))
  )
  choices %<>% setNames(choices)
}

#------------------------------------------------------------------------------------------------------
#----------------------------------- Server Part ------------------------------------------------------
#------------------------------------------------------------------------------------------------------
#' teal Data Column Extraction module server
#'
#' @param input \code{shiny input}
#' @param output \code{shiny output}
#' @param session \code{shiny session}
#' @param datasets (\code{teal::FilteredData}) Data build up by teal
#' @param constant_values (\code{data_extract}) A list of data filter and select
#'  information constructed by \link{data_extract}
#'
#' @return A \code{data.frame} where the keys are filtered and the columns selected
#'  due to the \link{data_extract_input} that is called with this module.
#'
#' @export
data_extractor <- function(input, output, session, datasets, constant_values) {

  # Filtering-sep / Filtering-vars / Filtering-choices (inkl mapping) / Columns-choices

  data <- reactive({
    ns_data <- function(x) paste0(input$ds, "-", x)

    data <- get_data_with_keys(datasets = datasets, dataname = input$ds)

    if (!methods::is(constant_values, "data_extract")) {
      constant_values <- constant_values[[
      which(
        unlist(
          lapply(constant_values, function(x) {
            x$dataname == input$ds
          })
        )
      )
      ]]
    }

    if (is.logical(input[[ns_data("filter")]])) {
      filters <- NULL
    } else {
      filtering_names <- input[[ns_data("filter")]]

      filtering_list <- constant_values$keys_filtering$choices

      filters <- choices2filter(
        filter_choices = filtering_names,
        list_of_filters = filtering_list,
        filtering_sep = " - ",
        variable_names = constant_values$keys_filtering$vars
      )
    }

    if (constant_values$columns$show) {
      columns <- input[[ns_data("column")]]
    } else {
      columns <- constant_values$columns$selected
    }

    data_filter_select(
      input_data = data,
      filters = filters,
      columns = columns,
      dataname = input$ds
    )
  })

  return(data)
}

#' @importFrom dplyr filter select
#' @importFrom tidyr unite
#' @importFrom rlang .data
data_filter_select <- function(input_data, filters, columns, dataname = "") {
  old_keys <- attr(input_data, "keys")
  new_keys <- setdiff(old_keys, filters$variable_names)

  if (is.null(filters$filtering_sep)) {
    filters$filtering_sep <- "_"
  }

  accepted_combinations <- lapply(
    filters$filters,
    function(comb) paste(comb, collapse = filters$filtering_sep)
  )

  if (!is.null(filters$filters)) {
    input_data %<>%
      tidyr::unite("tmp_keys_to_remove",
        filters$variable_names,
        sep = filters$filtering_sep
      ) %>%
      dplyr::filter(.data$tmp_keys_to_remove %in% accepted_combinations)
  }

  if (!is.null(columns)) {
    input_data %<>%
      select(c(new_keys, unlist(columns), recursive = FALSE))
  }

  attr(input_data, "keys") <- new_keys
  attr(input_data, "dataname") <- dataname

  return(input_data)
}

choices2filter <- function(filter_choices, list_of_filters, filtering_sep, variable_names) {
  if (is.null(filter_choices) || is.null(list_of_filters)) {
    return(NULL)
  } else {
    names(list_of_filters) <- filter2choices(list_of_filters, filtering_sep = filtering_sep)

    list_of_filters <- list_of_filters[filter_choices]

    return(
      list(
        variable_names = variable_names,
        filters = list_of_filters,
        filtering_sep = filtering_sep
      )
    )
  }
}

get_data_with_keys <- function(datasets, dataname) {
  data <- datasets$get_data(dataname, reactive = TRUE, filtered = FALSE)

  keys_stored <- attr(data, "keys")

  data <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)

  attr(data, "keys") <- keys_stored

  return(data)
}
