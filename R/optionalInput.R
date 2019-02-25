#' @import methods
# add hidden class to a \code{shiny.tag} object
hidden <- function(x) {
  if (!is(x, "shiny.tag")) stop("x needs to be of class shiny.tag")
  x$attribs$class <- paste(x$attribs$class, "hidden")
  x
}

#' Hide, Show Label only or display a selectInput
#'
#' @template descr_hidden_input
#'
#' @inheritParams shiny::selectInput
#'
#' @param choices character vector or \code{NULL}. If \code{choices} is
#'   \code{NULL} no selectInput widget is displayed and \code{input[[inputId]]}
#'   will be \code{""}. If \code{choices} is of length 1 then a label and
#'   character string will be displayed and the selectInput widget will be
#'   hidden. If the length of \code{choices} is more than one the selectInput
#'   element will be displayed.
#' @param label_help optional an object of class \code{shiny.tag}. E.g. an object
#'   returned by \code{\link[shiny]{helpText}}
#'
#' @export
#'
#' @examples
#'
#' optionalSelectInput("xvar", "x variable", 'A', 'A')
#' optionalSelectInput("xvar", "x variable", LETTERS[1:5], 'A')
#'
optionalSelectInput <- function(inputId, label, choices, selected, ..., label_help = NULL) { # nolint

  if (is.null(choices)) {
    choices <- ""
    selected <- NULL
    disp <- "nothing"
  } else {
    if (!all(selected %in% choices)) {
      stop(paste0("argument selected", paste(selected, collapse = ", "),
                  "is not in choices:", paste(choices, collapse = ", ")))
    }
    choices <- choices
    selected <- selected
    disp <- if (length(choices) == 1) "label" else "all"
  }


  sel_in <- selectInput(inputId, label, choices, selected, ...)

  if (!is.null(label_help)) {
    label_help$attribs$style <- "margin-top: -4px; margin-bottom: 3px;"
    sel_in[[3]] <- list(sel_in[[3]][[1]], label_help, sel_in[[3]][[2]])
  }


  switch(
    disp,
    nothing = hidden(sel_in),
    label = {
      div(
        hidden(sel_in),
        tags$span(tags$label(paste0(sub(":[[:space:]]+$", "", label), ":")), selected),
        label_help
      )
    },
    all = sel_in
  )
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

  if (hide) hidden(slider) else slider
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
#' @examples
#'
#' optionalSliderInputValMinMax("a", "b", 1)
#' optionalSliderInputValMinMax("a", "b", c(3, 1, 5))
#'
optionalSliderInputValMinMax <- function(inputId, label, value_min_max, ...) { # nolint

  x <- value_min_max

  if (!is.numeric(x)) stop("value_min_max is expected to be numeric")

  vals <- if (length(x) == 3) {
    if (any(diff(x[c(2, 1, 3)]) < 0))
      stop(paste("value_min_max is expected to be (value, min, max) where min <= value <= max"))
    list(value = x[1], min = x[2], max = x[3])
  } else if (length(x) == 1) {
    list(value = x, min = NA_real_, max = NA_real_)
  } else {
    stop(paste("value_min_max is expected to be of length 1 (value) or of length 3 (value, min, max)"))
  }

  optionalSliderInput(inputId, label, vals$min, vals$max, vals$value, ...)
}
