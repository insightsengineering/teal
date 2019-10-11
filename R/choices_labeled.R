#' Set "<choice>: <label>" type of Names
#' This is often useful for \code{\link[teal]{choices_selected}} as it marks up the dropdown boxes
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
#' ADSL <- radsl(cached = TRUE)
#' ADTTE <- radtte(cached = TRUE)
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
  stopifnot(is.character.vector(choices))

  if (is.factor(labels)) {
    labels <- as.character(labels)
  }
  stopifnot(is.character.vector(labels))

  stop_if_not(list(length(choices) == length(labels), "length of choices must be the same as labels"))

  stopifnot(is.null(subset) || is.vector(subset))

  if (length(subset) > 0) {
    stop_if_not(list(all(subset %in% choices), "all of subset variables must be in choices"))
    #subset choices + labels based on variables in subset
    labels <- labels[choices %in% subset]
    choices <- choices[choices %in% subset]
  }

  is_dupl <- duplicated(choices)
  choices <- choices[!is_dupl]
  labels <- labels[!is_dupl]

  labels[is.na(labels)] <- "Label Missing"
  raw_labels <- labels
  combined_labels <- paste0(choices, ": ", labels)

  if (length(subset) > 0) {
    ord <- match(subset, choices)
    choices <- choices[ord]
    raw_labels <- raw_labels[ord]
    combined_labels <- combined_labels[ord]
  }

  choices <- setNames(choices, combined_labels)

  attr(choices, "raw_labels") <- raw_labels
  attr(choices, "combined_labels") <- combined_labels

  return(choices)
}


#' Wrapper on \code{\link{choices_labeled}} to label variables basing on existing labels in data
#'
#' @param data (\code{data.frame}) data to extract labels from
#' @param subset (\code{character}) vector of column names
#'
#' @return named character vector with additional attributes
#'
#' @export
#'
#' @examples
#' library(random.cdisc.data)
#' ADRS <- radrs(cached = TRUE)
#'
#' variable_choices(ADRS)
#' variable_choices(ADRS, subset = c("PARAM", "PARAMCD"))
variable_choices <- function(data, subset = NULL) {
  stopifnot(is.data.frame(data))
  if (is.null(subset)) {
    subset <- names(data)
  }
  stopifnot(is.character.vector(subset))
  stopifnot(all(subset %in% names(data)))

  res <- choices_labeled(choices = names(data), labels = unname(get_variable_labels(data)), subset = subset)

  icons <- add_variable_type_icons(res, data, attr(res, "raw_labels"))
  attr(res, "icons") <- icons

  return(res)
}

#' Wrapper on \code{\link{choices_labeled}} to label variable values basing on other variable values
#'
#' @param data (\code{data.frame}) data to extract labels from
#' @param var_choices (\code{character}) vector with choices column names
#' @param var_label (\code{character}) vector with labels collumn names
#' @param subset (\code{vector}) vector with values to subset
#' @param sep (\code{character}) separator used in case of multiple column names
#'
#' @return named character vector
#'
#' @export
#'
#' @examples
#' library(random.cdisc.data)
#' ADRS <- radrs(cached = TRUE)
#'
#' value_choices(cadrs, "PARAMCD", "PARAM", subset = c("BESRSPI", "INVET"))
#' value_choices(cadrs, c("PARAMCD", "ARMCD"), c("PARAM", "ARM"))
#' value_choices(cadrs, c("PARAMCD", "ARMCD"), c("PARAM", "ARM"),
#'   subset = c("BESRSPI - ARM A", "INVET - ARM A", "OVRINV - ARM A"))
#' value_choices(cadrs, c("PARAMCD", "ARMCD"), c("PARAM", "ARM"), sep = " --- ")
value_choices <- function(data, var_choices, var_label, subset = NULL, sep = " - ") {
  stopifnot(is.data.frame(data))
  stopifnot(is.character.vector(var_choices))
  stopifnot(is.character.vector(var_label))
  stopifnot(is.null(subset) || is.vector(subset))

  res <- choices_labeled(
    choices = apply(data[var_choices], 1, paste, collapse = sep),
    labels = apply(data[var_label], 1, paste, collapse = sep),
    subset = subset
  )
  attr(res, "sep") <- sep
  return(res)
}
