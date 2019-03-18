#' Setup of Key Filtering for teal data sets
#'
#' @name KeysFilteringSpec
#' @field vars \code{character} The variables that shall be filtered with this specification
#' @field cs \code{choices_seleced} \link{choices_selected} outcome including \code{choices},
#'   \code{selected}, \code{multiple} and \code{label}
#' @section Arguments:
#' \describe{
#'  \item{\link{keys_filtering_spec}}{ Please see \link{keys_filtering_spec} for detailed argument descriptions}
#' }
#' @section Initialize:
#' \describe{
#'    \item{via new}{ \code{KeysFilteringSpec$new(vars, sep, choices, selected, multiple, label = "Filter")} }
#'   \item{via constructor}{
#'    \code{\link{keys_filtering_spec}(vars, sep, choices, selected, multiple, label = "Filter")}
#'   }
#' }
#'
#' @return A list of \code{vars} + a \link{choices_selected} outcome built by the split
#' 	selected and choices inputs.
#'
#'
#' @keywords data
#' @importFrom stats setNames
#' @export
#' @importFrom R6 R6Class
KeysFilteringSpec <- R6Class("KeysFilteringSpec", # nolint
  public = list(
    vars = character(0),
    cs = NULL,

    initialize = function(vars, sep, choices, selected, multiple, label = "Filter") {
      stopifnot(is.atomic(vars))
      stopifnot(is.atomic(choices))
      stopifnot(is.atomic(selected))
      stopifnot(multiple || (length(selected) == 1))

      split_by_sep <- function(txt) strsplit(txt, sep, fixed = TRUE)

      choices <- split_by_sep(choices)
      stopifnot(all(vapply(choices, length, 0) == length(vars)))

      selected <- split_by_sep(selected) # also a list if only a single element

      self$vars <- vars
      self$cs <- choices_selected(choices, selected, multiple, label = label)
    }
  )
)

#' Constructor for \link{KeysFilteringSpec}
#'
#' @export
#'
#' @param vars (\code{character}) Character vector giving the key columns to be filtered
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
#'  values of a shiny select input. Please check the \code{choices} description for further
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
#' keys_filtering_spec(
#'   vars = c("PARAMCD", "AVISIT"),
#'   sep = " - ",
#'   choices = c("CRP - BASELINE", "CRP - SCREENING", "ALT - BASELINE"),
#'   selected = c("CRP - BASELINE"),
#'   multiple = TRUE
#' )
keys_filtering_spec <- function(vars, sep, choices, selected, multiple, label = "Filter") {
  KeysFilteringSpec$new(
    vars = vars, sep = sep,
    choices = choices, selected = selected, multiple = multiple, label = label
  )
}
