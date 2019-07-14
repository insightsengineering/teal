no_select_keyword <- "-- no selection --"

#' Choices function
#'
#' @param choices vector of possible choices
#' @param selected vector of pre-selected options, first element of \code{choices} if blank
#' @param keep_order (\code{logical}) In case of \code{FALSE} the selected variables
#'   will be on top of the drop-down field.
#'
#' @details
#'
#' Please note that the order of selected will always follow the order of choices. The \code{keep_order}
#' argument is set to false which will run the following code inside:
#'
#' \code{choices <- c(selected, setdiff(choices, selected))}
#'
#' in case you want to keep your specific order of choices, set \code{keep_order} to \code{TRUE}.
#'
#' @export
#'
#' @examples
#'
#' choices_selected(
#'    choices = setNames(LETTERS[1:5], paste("Letter", LETTERS[1:5])),
#'    selected = "X"
#' )
#'
choices_selected <- function(choices, selected = choices[1], keep_order = FALSE) {

  stopifnot(is.atomic(choices))

  if (!is.null(choices) && no_select_keyword %in% choices)
    stop(paste(no_select_keyword, "is not a valid choice as it is used as a keyword"))

  if (length(setdiff(selected, choices)) > 0)
    choices <- c(setdiff(selected, choices), choices)

  if (!keep_order) {
    choices <- c(selected, setdiff(choices, selected))
  }

  structure(
    list(
      choices = choices[!duplicated(choices)],
      selected = unique(selected)
    ),
    class = "choices_selected"
  )
}

#' Check if an object is a choices_selected class.
#'
#' @param x object to check
#'
#' @export
is.choices_selected <- function(x) is(x, "choices_selected")

#' Add empty choice to choices selected
#'
#' @param x (\code{choices_selected}) output
#' @param multiple (\code{logical}) whether multiple selections are allowed or not
#'
#'
#' @export
add_no_selected_choices <- function(x, multiple = FALSE) {

  if (is.null(x)) {
    choices_selected(NULL)
  } else {
    stopifnot(is.choices_selected(x))

    if (!multiple) {
      x$choices <- c(no_select_keyword, x$choices)
      if (is.null(x$selected)) x$selected <- no_select_keyword
    }

    x
  }

}

#' Check select choices for no choice made
#'
#' @export
#'
#' @param x (\code{character}) Word that shall be checked for
#'   NULL, empty, "--no-selection"
#'
#' @return the word or NULL
no_selected_as_NULL <- function(x) { #nolint
  if (is.null(x) || identical(x, no_select_keyword) || x == "") {
    NULL
  } else {
    x
  }
}
