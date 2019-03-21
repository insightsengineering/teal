#' filter spec
#'
#' It consists in choices and additionally the variable names for the choices
#'
#' @export
#'
#' @param vars (\code{character}) Character vector giving the columns to be filtered
#' @param sep (\code{character}) A separator string to split the \code{choices} or
#'   \code{selected} inputs into the values of the different columns
#' @param choices (\code{character}) Named character vector to define the choices
#' 	of a shiny select input. These shall be filter values of the \code{vars} input
#' 	separated by \code{sep}.
#'
#' E.g. \code{vars = c("PARAMCD","AVISITN")} and \code{choices = c("CRP - BASELINE","ALT - BASELINE")}
#'  will lead to a filtering of
#'  \code{(PARAMCD == "CRP" & AVISITN == "BASELINE") | (PARAMCD == "ALT" & AVISITN == "BASELINE")}.
#'
#' Please make sure the order is right. The \code{sep} input has to be \code{" - "} in this case.
#'
#' @param selected (\code{character}) Named character vector to define the selected
#'  values of a shiny select input (default values). Please check the \code{choices} description for further
#'  details
#'
#' @param multiple (\code{logical}) Whether multiple values shall be allowed in the
#'  shiny select input.
#'
#' @param label (\code{character}) Label of the Key Filtering Input inside the shiny app.
#'
#' @return \code{\link{KeysFilteringSpec}}-class object
#'
#' @examples
#' filter_spec(
#'   vars = c("PARAMCD", "AVISIT"),
#'   sep = " - ",
#'   choices = c("CRP - BASELINE", "CRP - SCREENING", "ALT - BASELINE"),
#'   selected = c("CRP - BASELINE"),
#'   multiple = TRUE
#' )
filter_spec <- function(vars, choices, selected, multiple, label = "Filter", sep = " - ") {
  stopifnot(is.atomic(vars))
  stopifnot(is.atomic(choices))
  stopifnot(is.atomic(selected))
  stopifnot(all(is.character(vars)))
  stopifnot(all(is.character(choices)))
  stopifnot(all(is.character(selected)))
  stopifnot(is.character(sep) && length(sep) == 1 && is.atomic(sep))
  stopifnot(multiple || length(selected) == 1)

  choices <- split_by_sep(choices, sep)
  selected <- split_by_sep(selected, sep)
  stopifnot(all(vapply(choices, length, 0) == length(vars)))

  res <- c(vars = vars, columns_spec(choices = choices, selected = selected, multiple, label = label))
  class(res) <- "filter_choices_spec"
  res
}
