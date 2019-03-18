# Instead of column_filter overwrite choices_selected
# backwards compatible with new arguments show and label
#' Choices selected
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
#'
#' @return A \code{list} of all input values. The function double checks the \code{choices}
#'   and \code{selected} inputs.
#'
#' @importFrom magrittr %<>%
#'
#'
#' @export
choices_selected <- function(choices, selected, multiple, show = FALSE, label = NULL) {
  stopifnot(length(choices) >= 1)
  stopifnot(length(selected) >= 1)
  stopifnot(is.logical(multiple))
  stopifnot(all(selected %in% choices))

  if (is.null(names(choices))) {
    choices %<>% setNames(choices)
  }
  if (is.null(names(selected))) {
    selected %<>% setNames(selected)
  }
  out <- list(choices = choices, selected = selected, multiple = multiple, show = show, label = label)
  class(out) <- "choices_selected"
  return(out)
}
