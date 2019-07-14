#' Set "<choice>: <label>" type of Names
#'#' This is often useful for \code{\link[teal]{choices_selected}} as it marks up the dropdown boxes
#' for \code{\link[shiny]{selectInput}}.
#' @param choices a character vector
#' @param labels vector containing labels to be applied to \code{choices}
#' @param subset a character vector that is a subset of \code{choices}. This is useful if
#' only a few variables need to be named. If this argument is used, the returned vector will#' match it's order.
#'
#' @details If either \code{choices} or \code{labels} are factors, they are coerced to character.
#' Duplicated elements from \code{choices} get removed.
#'
#' @return a named character vector
#'
#' @importFrom stats setNames
#'
#' @export
#'
#' @examples
#' library(random.cdisc.data)
#' library(tern)
#'
#' ADSL <- radsl(N=10, seed = 1)
#' ADTTE <- radtte(ADSL, seed = 1)
#' choices1 <- choices_labeled(names(ADSL), var_labels(ADSL))
#' choices2 <- choices_labeled(ADTTE$PARAMCD, ADTTE$PARAM)
#' # if only a subset of variables are needed, use subset argument
#' choices3 <- choices_labeled(names(ADSL), var_labels(ADSL), subset = c("ARMCD", "ARM"))
#'\dontrun{
#' shinyApp(
#'   ui = fluidPage(selectInput("c1", label = "Choices from ADSL",
#'                              choices = choices1,
#'                              selected = choices1[1]),
#'                  selectInput("c2", label = "Choices from ADTTE",
#'                              choices = choices2,
#'                              selected = choices2[1]),
#'                  selectInput("c3", label = "Arm choices from ADSL",
#'                              choices = choices3,
#'                              selected = choices3[1])),
#'   server = function(input, output) {}
#' )
#'}

choices_labeled <- function(choices, labels, subset = NULL) {
  if (is.factor(choices)) {
    choices <- as.character(choices)
  }
  if (is.factor(labels)) {
    labels <- as.character(labels)
  }
  stopifnot(is.character(choices))
  length(choices) == length(labels) || stop("length of choices must be the same as labels")
  stopifnot(is.null(subset) || is.character(subset))
  if (is.character(subset)) {
    all(subset %in% choices) || stop("all of subset variables must be in choices")
    #subset choices + labels based on variables in subset
    labels <- labels[choices %in% subset]
    choices <- choices[choices %in% subset]
  }
  is_dupl <- duplicated(choices)
  choices <- choices[!is_dupl]
  labels <- labels[!is_dupl]

  labels[is.na(labels)] <- "Label Missing"
  labels <- paste0(choices, ": ", labels)
  if (is.character(subset)) {
    ord <- match(subset, choices)
    choices <- choices[ord]
    labels <- labels[ord]
  }
  setNames(choices, labels)
}
