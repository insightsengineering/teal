#' Hide, Show Label only or display a \code{pickerInput}
#'
#' @md
#' @description `r lifecycle::badge("maturing")`
#' Hidden input widgets are useful to have the \code{input[[inputId]]} variable
#' on available in the server function but no corresponding visual clutter from
#' input widgets that provide only a single choice.
#'
#' @inheritParams shinyWidgets::pickerInput
#' @param choices character vector or \code{NULL}. If \code{choices} is
#'   \code{NULL} no \code{pickerInput} widget is displayed and \code{input[[inputId]]}
#'   will be \code{""}. If \code{choices} is of length 1 then a label and
#'   character string will be displayed and the \code{pickerInput} widget will be
#'   hidden. If the length of \code{choices} is more than one the \code{pickerInput}
#'   element will be displayed.
#'   If elements of the list are named then that name rather than the value
#'   is displayed to the user.
#'
#' @param sep (\code{character}) A separator string to split the \code{choices} or
#'   \code{selected} inputs into the values of the different columns
#'
#' @param label_help optional an object of class \code{shiny.tag}. E.g. an object
#'   returned by \code{\link[shiny]{helpText}}
#'
#' @param fixed (\code{logical}) (optional) whether to block user to select choices
#'
#' @param width (\code{character}) The width of the input passed to \code{pickerInput}
#'   e.g. 'auto', 'fit', '100px' or '75%'
#'
#' @export
#'
#' @importFrom shinyjs hidden
#' @importFrom shinyWidgets pickerInput
#'
#' @examples
#'
#' optionalSelectInput(inputId = "xvar", label = "x variable", choices = "A", selected = "A")
#' optionalSelectInput(inputId = "xvar", label = "x variable", choices = LETTERS[1:5], selected = "A")
#' optionalSelectInput(inputId = "xvar",
#'                     label = "x variable",
#'                     choices = c("A - value A" = "A"),
#'                     selected = "A")
#'
#' library(random.cdisc.data)
#' ADRS <- radrs(cached = TRUE)
#' optionalSelectInput(inputId = "xvar",
#'                     label = "x variable",
#'                     choices = choices_labeled(
#'                       choices = letters,
#'                       labels = LETTERS,
#'                       subset = c("a", "b", "c")
#'                     ),
#'                     selected = "a")
#' optionalSelectInput(inputId = "xvar",
#'                     label = "x variable",
#'                     choices = variable_choices(data = ADRS, subset = c("AGE", "SEX", "PARAMCD")),
#'                     selected = "PARAMCD")
#'
#' selected_value <- paste0(lapply(ADRS[1, c("PARAMCD", "AVISIT")], as.character), collapse = " - ")
#' optionalSelectInput(inputId = "xvar",
#'                     label = "x variable",
#'                     choices = value_choices(data = ADRS,
#'                                             var_choices = c("PARAMCD", "AVISIT"),
#'                                             var_label = c("PARAM", "AVISIT")),
#'                     selected = selected_value)
#'
optionalSelectInput <- function(inputId, # nolint
                                label = NULL,
                                choices = NULL,
                                selected = NULL,
                                multiple = FALSE,
                                sep = NULL,
                                options = list(),
                                label_help = NULL,
                                fixed = FALSE,
                                width = NULL) {
  stopifnot(is_character_single(inputId))
  stopifnot(is.null(label) || is_character_single(label) || is_html_like(label))
  stopifnot(is.null(choices) || length(choices) >= 1)
  stopifnot(
    is.null(selected) ||
    length(selected) == 0 ||
    all(selected %in% choices) ||
    all(selected %in% unlist(choices, recursive = FALSE)))
  stopifnot(is_logical_single(multiple))
  stopifnot(is.null(sep) || is_character_single(sep))
  stopifnot(is.list(options))
  stopifnot(is.null(label_help) || is_character_single(label_help) || is_html_like(label_help))
  stopifnot(is_logical_single(fixed))

  if (!is.null(width)) {
    shiny::validateCssUnit(width)
  }

  default_options <- list(
    "actions-box" = multiple,
    "none-selected-text" = "- Nothing selected -",
    "max-options" = ifelse(multiple, Inf, 1),
    "show-subtext" = TRUE,
    "live-search" = ifelse(length(choices) > 10, TRUE, FALSE)
  )
  options <- if (!identical(options, list())) {
    c(options, default_options[setdiff(names(default_options), names(options))])
  } else {
    default_options
  }

  if (is.null(choices)) {
    choices <- ""
    selected <- NULL
  }

  raw_choices <- extract_raw_choices(choices, attr(choices, "sep"))
  raw_selected <- extract_raw_choices(selected, attr(choices, "sep"))



  ui <- pickerInput(
    inputId = inputId,
    label = label,
    choices = raw_choices,
    selected = raw_selected,
    multiple = TRUE,
    width = width,
    options = options,
    choicesOpt = picker_options(choices)
  )

  if (!is.null(label_help)) {
    ui[[3]] <- append(ui[[3]], list(div(class = "label-help", label_help)), after = 1)
  }

  if (is.null(choices)) {

    return(hidden(ui))

  } else {

    if (fixed) {
      label_selected <- extract_choices_labels(choices, selected)

      return(div(
        hidden(ui),
        tags$span(id = paste0(inputId, "_textonly"), style = "font-weight:bold", sub(":[[:space:]]+$", "", label)),
        tags$span(
          id = paste0(inputId, "_valueonly"),
          paste(if_null(label_selected, selected), collapse = ", ")
        ),
        label_help
      ))
    } else {
      return(ui)
    }
  }
}

#' Update \code{optionalSelectInput}
#'
#' @md
#' @description `r lifecycle::badge("maturing")`
#'
#' @inheritParams shinyWidgets::updatePickerInput
#'
#' @return \code{NULL}
#'
#' @export
#'
#' @importFrom shinyjs hide show
#' @importFrom shinyWidgets updatePickerInput
updateOptionalSelectInput <- function(session, # nolint
                                      inputId, # nolint
                                      label = NULL,
                                      selected = NULL,
                                      choices = NULL) {

  raw_choices <- extract_raw_choices(choices, attr(choices, "sep"))
  raw_selected <- extract_raw_choices(selected, attr(choices, "sep"))

  updatePickerInput(
    session = session,
    inputId = inputId,
    label = label,
    selected = raw_selected,
    choices = raw_choices,
    choicesOpt = picker_options(choices)
  )

  shinyjs::show(inputId)
  shinyjs::hide(paste0(inputId, "_textonly"))

  invisible(NULL)
}

#' Get icons to represent variable types in dataset
#'
#' @param var_type (\code{character} vector) of R internal types (classes).
#'
#' @return (atomic vector of \code{character}) vector of HTML icons corresponding to
#'   data type in each column.
#'
#' @examples
#' teal:::variable_type_icons(c("integer", "numeric", "logical", "Date", "POSIXct", "POSIXlt",
#' "factor", "character", "unknown", ""))
variable_type_icons <- function(var_type) {
  stopifnot(is_character_vector(var_type, min_length = 0))

  class_to_icon <- list(
    numeric = "sort-numeric-up",
    integer = "sort-numeric-up",
    logical = "pause",
    Date = "calendar",
    POSIXct = "calendar",
    POSIXlt = "calendar",
    factor = "chart-bar",
    character = "keyboard",
    unknown = "question-circle"
  )
  class_to_icon <- lapply(class_to_icon, function(icon_name) toString(icon(icon_name, lib = "font-awesome")))

  res <- unname(vapply(
    var_type,
    function(class) {
      if_not_cond(class, if_null(class_to_icon[[class]], class_to_icon[["unknown"]]), function(x) x == "")
    },
    character(1)
  ))

  return(res)
}

#' Optional content for \code{optionalSelectInput}
#'
#' Prepares content to be displayed in \code{optionalSelectInput} with icons and labels
#'
#' @param var_name (atomic vector of \code{character}) variable name
#' @param var_label (atomic vector of \code{character})
#' variable alternative name - for example variable label
#' @param var_type (atomic vector of \code{character})
#' class of the variable.
#'
#' @return (atomic vector of \code{character}) HTML contents with all elements combined
#'
#' @examples
#' teal:::picker_options_content(
#'   var_name = c("SEX", "BRMRKR1"),
#'   var_label = c("Sex", "Biomarker 1"),
#'   var_type = c("factor", "numeric")
#' )
picker_options_content <- function(var_name, var_label, var_type) {
  if (utils.nest::is_empty(var_name)) {
    return(character(0))
  }
  if (utils.nest::is_empty(var_type) && utils.nest::is_empty(var_label)) {
    return(var_name)
  }
  stopifnot(
    is_character_vector(var_name),
    is_character_empty(var_type) || length(var_type) == length(var_name),
    is_character_empty(var_label) || length(var_label) == length(var_name)
  )

  var_icon <- variable_type_icons(var_type)

  res <- trimws(paste(
    var_icon,
    var_name,
    vapply(
      var_label,
      function(x) {
        ifelse(x == "", "", toString(tags$small(x, class = "text-muted")))
      },
      character(1)
    )
  ))

  return(res)
}

#' Create \code{choicesOpt} for \code{pickerInput}
#'
#' @param choices (\code{choices_labeled} or \code{character} vector) choices vector
#'
#' @return (\code{list}) to be passed as \code{choicesOpt} argument
picker_options <- function(choices) {
  if (is(choices, "choices_labeled")) {
    raw_choices <- extract_raw_choices(choices, sep = attr(choices, "sep"))
    return(
      list(
        content = picker_options_content(
          var_name  = raw_choices,
          var_label = extract_choices_labels(choices),
          var_type  = if_null(attr(choices, "types"), character(0))
        )
      )
    )
  } else if (all(vapply(choices, is, logical(1), "choices_labeled"))) {
    choices <- unlist(unname(choices))
    return(
      list(content = picker_options_content(
        var_name  = choices,
        var_label = extract_choices_labels(choices),
        var_type  = if_null(attr(choices, "types"), character(0))
      ))
    )
  } else {
    return(NULL)
  }
}

#' Extract raw values from choices
#'
#' @param choices (\code{choices_labeled} or \code{list} or \code{character}) with choices
#' @param sep (\code{character}) A separator string to split the \code{choices} or
#'   \code{selected} inputs into the values of the different columns
#' @return choices simplified
extract_raw_choices <- function(choices, sep) {
 if (!is.null(sep)) {
   vapply(choices, paste, collapse = sep, character(1))
  } else if (is(choices, "choices_labeled")) {
    unname(unlist(choices))
  } else {
    choices
  }
}




#' if min or max are \code{NA} then the slider widget will be hidden
#'
#' @md
#' @description `r lifecycle::badge("maturing")`
#' Hidden input widgets are useful to have the \code{input[[inputId]]} variable
#' on available in the server function but no corresponding visual clutter from
#' input widgets that provide only a single choice.
#'
#' @inheritParams shiny::sliderInput
#' @param ... optional arguments to \code{sliderInput}
#'
#' @export
#'
#' @examples
#' optionalSliderInput("a", "b", 0, 1, 0.2)
optionalSliderInput <- function(inputId, label, min, max, value, ...) { # nolint
  hide <- if (is.na(min) || is.na(max)) {
    min <- value - 1
    max <- value + 1
    TRUE
  } else if (min > value || max < value) {
    stop("arguments inconsistent: min <= value <= max expected")
  } else {
    FALSE
  }

  slider <- sliderInput(inputId, label, min, max, value, ...)

  if (hide) {
    hidden(slider)
  } else {
    slider
  }
}



#' For teal modules we parameterize an \code{optionalSliderInput} with one argument
#' \code{value_min_max}
#'
#' @md
#' @description `r lifecycle::badge("maturing")`
#' The \code{\link{optionalSliderInput}} function needs three arguments to decided
#' whether to hide the \code{sliderInput} widget or not. For teal modules we specify an
#' optional slider input with one argument here called \code{value_min_max}.
#'
#' @inheritParams optionalSliderInput
#'
#' @param value_min_max numeric vector. If of length 1 then the value gets set
#'   to that number and the \code{sliderInput} will be hidden. Otherwise, if it is of
#'   length three the three elements will map to \code{value}, \code{min} and
#'   \code{max} of the \code{\link{optionalSliderInput}} function.
#'
#' @export
#'
#' @importFrom shinyjs hidden
#'
#' @examples
#'
#' optionalSliderInputValMinMax("a", "b", 1)
#' optionalSliderInputValMinMax("a", "b", c(3, 1, 5))
optionalSliderInputValMinMax <- function(inputId, label, value_min_max, ...) { # nolint

  x <- value_min_max

  if (!is.numeric(x)) stop("value_min_max is expected to be numeric")

  vals <- if (length(x) == 3) {
    if (any(diff(x[c(2, 1, 3)]) < 0)) {
      stop(paste("value_min_max is expected to be (value, min, max) where min <= value <= max"))
    }
    list(value = x[1], min = x[2], max = x[3])
  } else if (length(x) == 1) {
    list(value = x, min = NA_real_, max = NA_real_)
  } else {
    stop(paste("value_min_max is expected to be of length 1 (value) or of length 3 (value, min, max)"))
  }

  optionalSliderInput(inputId, label, vals$min, vals$max, vals$value, ...)
}
