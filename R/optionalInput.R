#' Hide, Show Label only or display a pickerInput
#'
#' @template descr_hidden_input
#'
#' @inheritParams shinyWidgets::pickerInput
#'
#' @param choices character vector or \code{NULL}. If \code{choices} is
#'   \code{NULL} no pickerInput widget is displayed and \code{input[[inputId]]}
#'   will be \code{""}. If \code{choices} is of length 1 then a label and
#'   character string will be displayed and the pickerInput widget will be
#'   hidden. If the length of \code{choices} is more than one the pickerInput
#'   element will be displayed.
#' @param ... arguments passed to \code{\link[shinyWidgets]{pickerInput}}
#' @param label_help optional an object of class \code{shiny.tag}. E.g. an object
#'   returned by \code{\link[shiny]{helpText}}
#'
#' @export
#'
#' @importFrom shinyjs hidden
#' @importFrom shinyWidgets pickerInput
#'
#' @examples
#'
#' optionalSelectInput("xvar", "x variable", 'A', 'A')
#' optionalSelectInput("xvar", "x variable", LETTERS[1:5], 'A')
#'
optionalSelectInput <- function(inputId, # nolint
                                label,
                                choices = NULL,
                                selected = NULL,
                                multiple = FALSE,
                                options = list(),
                                ...,
                                label_help = NULL) {
  stopifnot(
    is.character(inputId),
    length(inputId) == 1,
    (is.character(label) && length(label) == 1) || inherits(label, "shiny.tag") || inherits(label, "shiny.tag.list"),
    is.null(choices) || length(choices) >= 1,
    is.null(selected) || length(selected) == 0 || all(selected %in% choices),
    is.logical(multiple),
    length(multiple) == 1,
    is.list(options)
  )
  print(label)

  default_options <- list(`actions-box` = TRUE)

  options <- if (!identical(options, list())) {
    c(options, default_options)
  } else {
    default_options
  }

  ui <- pickerInput(
    inputId = inputId,
    label = label,
    choices = choices,
    selected = selected,
    multiple = multiple,
    options = options,
    ...
  )

  if (is.null(choices)) {

    choices <- ""
    selected <- NULL
    return(hidden(ui))

  } else {

    if (!is.null(label_help)) {
      label_help <- tagAppendAttributes(label_help, style = "margin-top: -4px; margin-bottom: 3px;")
    }

    if (length(choices) == 1) {

      return(div(
        hidden(ui),
        tags$span(tags$label(paste0(sub(":[[:space:]]+$", "", label), ":")), selected),
        label_help
      ))

    } else {

      if (!is.null(label_help)) {
        ui[[3]] <- list(ui[[3]][[1]], label_help, ui[[3]][[2]], ui[[3]][[3]])
      }

      return(ui)

    }
  }
}


#' if min or max are \code{NA} then the slider widget will be hidden
#'
#' @template descr_hidden_input
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
    stop("arguments inconsisten: min <= value <= max expected")
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



#' For teal modules we parameterize an optionalSliderInput with one argument
#' \code{value_min_max}
#'
#' The \code{\link{optionalSliderInput}} function needs three arguments to decided
#' wheter to hide the sliderInput widget or not. For teal modules we specify an
#' optional slider input with one argument here called \code{value_min_max}.
#'
#' @inheritParams optionalSliderInput
#'
#' @param value_min_max numeric vector. If of length 1 then the value gets set
#'   to that number and the sliderInput will be hidden. Otherwise, if it is of
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
#'
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
