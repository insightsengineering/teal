# Instead of column_filter overwrite choices_selected
# backwards compatible with new arguments show and label
#' column_choices_spec
#'
#' @param choices (\code{character}) Named character vector to define the choices
#' 	of a shiny select input.
#' @param selected (\code{character}) Named character vector to define the selected
#'  values of a shiny select input.
#' @param multiple (\code{logical}) Whether multiple values shall be allowed in the
#'  shiny select input.
#' @param show (\code{logical}) (optional) \link{DataExtractSpec} specific feature to
#'   hide the choices selected in case they are not needed.
#' @param label (\code{logical}) (optional) \link{DataExtractSpec} specific feature to
#'   show a different label on top of this specific \link{shiny}{selectInput}.
#' @param ... additional parameters
#'
#' @return A \code{list} of all input values. The function double checks the \code{choices}
#'   and \code{selected} inputs.
#'
#' @importFrom magrittr %<>%
#'
#'
#' @export
column_choices_spec <- function(choices, selected, multiple, show = FALSE, label = NULL, ...) {
  #todo: remove show and label
  # when choices and selected is not a list, we convert it to a list (because each entry is an atomic vector of possibly several entries)
  choices <- as.list(choices)
  selected <- as.list(selected)
  browser()
  stopifnot(is.list(choices) && length(choices) >= 1 && all_true(choices, is.atomic))
  stopifnot(is.list(selected) && length(selected) >= 1 && all_true(selected, is.atomic))
  stopifnot(is.logical(multiple))
  stopifnot(is.logical(show))
  stopifnot(all(selected %in% choices)) # selected and choices must be a list to work correcty
  # check for correct lengths
  stopifnot(multiple || length(selected) == 1)
  length_selected <- length(selected[[1]])
  has_same_length <- function(elem) {
    length(elem) == length_selected
  }
  stopifnot(all_true(choices, has_same_length))
  stopifnot(all_true(selected, has_same_length))

  # if names is NULL, shiny will put strange labels (with quotes etc.) in the selectInput, so we set it to the values
  if (is.null(names(choices))) {
    choices %<>% setNames(choices)
  }
  if (is.null(names(selected))) {
    selected %<>% setNames(selected)
  }
  out <- list(choices = choices, selected = selected, multiple = multiple, show = show, label = label, ...)
  class(out) <- "column_choices_spec"
  out
}
