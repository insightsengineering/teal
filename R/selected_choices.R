no_select_keyword <- "-- no selection --"

#' Choices function
#'
#' @param choices vector of possible choices
#' @param selected vector of pre-selected options, first element of \code{choices} if blank
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
choices_selected <- function(choices, selected = choices[1]) {

  stopifnot(is.atomic(choices))

  if (!is.null(choices) && no_select_keyword %in% choices)
    stop(paste(no_select_keyword, "is not a valid choice as it is used as a keyword"))

  if (length(setdiff(selected, choices)) > 0)
    choices <- c(setdiff(selected, choices), choices)


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

no_selected_as_NULL <- function(x) {
  if(is.null(x) || identical(x, no_select_keyword) || x == "") {
    NULL
  } else {
    x
  }
}
