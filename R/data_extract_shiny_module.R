#------------------------------------------------------------------------------------------------------
#----------------------------------- UI Part ------------------------------------------------------
#------------------------------------------------------------------------------------------------------

#' teal Data Column Extraction module ui
#'
#' @param id (\code{character}) Shiny input ID
#' @param label (\code{character}) Label above the Data extract input
#' @param data_extract_spec (\code{DataExtractSpec}) This is the outcome of a \link{DataExtractSpec}
#'  function constructor call.
#'
#' @return shiny Input that allows to define a filtering of key variables of
#'  a data set and the selection of non-key variables. This input is the UI function
#'  of the \link{data_extract_module} server function.
#'
#' @import shiny
#'
data_extract_input <- function(id, label, data_extract_spec = data_extract_spec()) {
  ns <- NS(id)

  # List of elements
  if (is.null(data_extract_spec$dataname)) {
    output_panel <- lapply(data_extract_spec, function(dataset_extract_spec) {
      dataname <- dataset_extract_spec$dataname
      conditionalPanel(
        condition = paste0("input['", ns("ds"), "'] == '", dataset_extract_spec$dataname, "'"),
        data_extract_input_single(id = ns(dataname), data_extract_spec = dataset_extract_spec)
      )
    }) # lapply
    datanames <- unlist(lapply(data_extract_spec, function(x) x$dataname))
  } else {
    output_panel <- list(data_extract_input_single(ns(data_extract_spec$dataname), data_extract_spec))

    datanames <- data_extract_spec$dataname
  }

  datanames %<>% setNames(datanames)
  if (!is.null(data_extract_spec)) {
    div(
      tags$label(label),
      optionalSelectInput(
        inputId = ns("ds"),
        label = "Dataset",
        choices = datanames,
        selected = datanames[1],
        multiple = FALSE
      ),
      do.call(div, output_panel)
    )
  } else {
    div()
  }
}

#' @importFrom shinyjs hidden
#' @importFrom methods is
data_extract_input_single <- function(id = NULL, data_extract_spec = data_extract_spec(), filtering_sep = " - ") {
  ns <- NS(id)

  stopifnot(methods::is(data_extract_spec, "DataExtractSpec"))

  div(
    # Construct a filtering for the key variables. In case no filtering
    # was set up return a logical false by a hidden checkbox
    if (!is.null(data_extract_spec$keys_filtering)) {
      stopifnot(methods::is(data_extract_spec$keys_filtering, "KeysFilteringSpec"))

      shiny::tagList(
        optionalSelectInput(
          inputId = ns("filter"),
          label = if (!is.null(data_extract_spec$keys_filtering$cs$label)) {
            data_extract_spec$keys_filtering$cs$label
          } else {
            "Filter"
          },
          choices = list_of_filters_to_label(data_extract_spec$keys_filtering$cs$choices, filtering_sep),
          selected = list_of_filters_to_label(data_extract_spec$keys_filtering$cs$selected, filtering_sep),
          multiple = data_extract_spec$keys_filtering$cs$multiple
        )
      )
    },

    # Construct a Column selector in case a column selection should be possible by
    # users.
    if (data_extract_spec$columns$show) {
      optionalSelectInput(
        inputId = ns("column"),
        label = if (is.null(data_extract_spec$columns$label)) {
          "Column"
        } else {
          data_extract_spec$columns$label
        },
        choices = data_extract_spec$columns$choices,
        selected = data_extract_spec$columns$selected,
        multiple = data_extract_spec$columns$multiple
      )
    } else {
      helpText("Column:", tags$code(paste(data_extract_spec$columns$selected, collapse = " ")))
    }
  )
}

data_extract_single_module <- function(input, output, session, data_extract_spec) {
  reactive({
    if (is.null(input$filter)) {
      filters <- NULL
    } else {
      filtering_names <- input$filter

      filtering_list <- data_extract_spec$keys_filtering$cs$choices

      filters <- label_to_list_of_filters(
        filter_choices = filtering_names,
        list_of_filters = filtering_list,
        filtering_sep = " - ",
        variable_names = data_extract_spec$keys_filtering$vars
      )
    }

    if (data_extract_spec$columns$show) {
      columns <- input$column
    } else {
      columns <- data_extract_spec$columns$selected
    }
    return(list(filters = filters, columns = columns))
  })
}
# List value 2 label
list_of_filters_to_label <- function(list_of_filters, filtering_sep) {
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
#' @param data_extract_spec (\code{DataExtractSpec}) A list of data filter and select
#'  information constructed by \link{DataExtractSpec}
#'
#' @return A \code{data.frame} where the keys are filtered and the columns selected
#'  due to the \link{data_extract_input} that is called with this module.
#'
#' @export
data_extract_module <- function(input, output, session, datasets, data_extract_spec) {

  # Filtering-sep / Filtering-vars / Filtering-choices (inkl mapping) / Columns-choices

  data <- reactive({

    data <- get_data_with_keys(datasets = datasets, dataname = input$ds)
    if (!methods::is(data_extract_spec, "DataExtractSpec")) {
      data_extract_spec <- data_extract_spec[[
      which(
        unlist(
          lapply(data_extract_spec, function(x) {
            x$dataname == input$ds
          })
        )
      )
      ]]
    }

    filter_columns <- callModule(data_extract_single_module, input$ds, data_extract_spec = data_extract_spec)

    data_filter_select(
      input_data = data,
      filters = filter_columns()$filters,
      columns = filter_columns()$columns,
      dataname = input$ds
    )
  })

  return(data)
}



label_to_list_of_filters <- function(filter_choices, list_of_filters, filtering_sep, variable_names) {
  if (is.null(filter_choices) || is.null(list_of_filters)) {
    return(NULL)
  } else {
    names(list_of_filters) <- list_of_filters_to_label(list_of_filters, filtering_sep = filtering_sep)

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
