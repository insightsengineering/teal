# Instead of column_filter overwrite choices_selected
# backwards compatible with new arguments fixed and label
#' column_choices_spec
#'
#' @param choices (\code{character}) Named character vector to define the choices
#' 	of a shiny select input.
#' @param selected (\code{character}) Named character vector to define the selected
#'  values of a shiny select input.
#' @param multiple (\code{logical}) Whether multiple values shall be allowed in the
#'  shiny select input.
#' @param fixed (\code{logical}) (optional) \link{DataExtractSpec} specific feature to
#'   hide the choices selected in case they are not needed.
#' @param label (\code{logical}) (optional) \link{DataExtractSpec} specific feature to
#'   fixed a different label on top of this specific \link{shiny}{selectInput}.
#'
#' @return A \code{list} of all input values. The function double checks the \code{choices}
#'   and \code{selected} inputs.
#'
#' @importFrom magrittr %<>%
#' @importFrom purrr map_lgl
#' @importFrom stats setNames
#' @export
columns_spec <- function(choices, selected, multiple, fixed = TRUE, label = "Column(s)") {
  # when choices and selected is not a list, we convert it to a list (because each
  # entry is an atomic vector of possibly several entries, needed for filter_spec currently)
  choices <- as.list(choices)
  selected <- as.list(selected)
  stopifnot(is.list(choices) && length(choices) >= 1 && all_true(choices, is.atomic))
  stopifnot(is.list(selected) && length(selected) >= 1 && all_true(selected, is.atomic))
  stopifnot(all(selected %in% choices)) # selected and choices must be a list to work correcty
  stopifnot(is.logical(multiple))
  stopifnot(is.logical(fixed))
  stopifnot(is.character(label) && length(label) == 1)
  # check for correct lengths
  stopifnot(multiple || length(selected) == 1)

  stopifnot(all(map_lgl(selected, ~ length(.) == length(selected[[1]]))))
  stopifnot(all(map_lgl(choices, ~ length(.) == length(choices[[1]]))))
  # if names is NULL, shiny will put strange labels (with quotes etc.) in the selectInputs, so we set it to the values
  if (is.null(names(choices))) {
    choices %<>% setNames(choices)
  }
  if (is.null(names(selected))) {
    selected %<>% setNames(selected)
  }
  res <- list(choices = choices, selected = selected, multiple = multiple, fixed = fixed, label = label)
  class(res) <- "column_choices_spec"
  res
}
