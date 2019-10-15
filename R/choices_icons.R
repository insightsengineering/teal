
#' Get icons to represent variable types in dataset
#'
#' @param columns (atomic vector of \code{character}) columns to take from data
#' @param data (\code{data.frame}) data to determine variable types from
#'
#' @return (atomic vector of \code{character}) vector of HTML icons corresponding to
#'   data type in each column, NULL item if in unknown category
#'
#' @examples
#' teal:::variable_type_icons(
#'   c("x"),
#'   data.frame(
#'     x = 1:3, y = factor(c("a", "b", "a")), z = c("h1", "h2", "h3"),
#'     stringsAsFactors = FALSE
#'   )
#' )
#'
#' teal:::variable_type_icons(
#'   c("x", "z"),
#'   data.frame(x = 1:3, y = factor(c("a", "b", "a")), z = c("h1", "h2", "h3"),
#'     stringsAsFactors = FALSE)
#' )
#' teal:::variable_type_icons(character(0), data.frame(x = 1:3))
variable_type_icons <- function(columns, data) {
  stopifnot(
    is.data.frame(data),
    is.character.vector(columns, min_length = 0),
    all(columns %in% colnames(data))
  )
  # we take first class in case of multiple ones
  column_classes <- vapply(data[, columns, drop = FALSE], function(x) class(x)[[1]], character(1))

  class_to_icon <- list(
    numeric = "sort-numeric-up",
    integer = "sort-numeric-up",
    logical = "pause",
    Date = "calendar",
    POSIXct = "calendar",
    POSIXlt = "calendar",
    factor = "chart-bar",
    character = "keyboard",
    unknown = "question-circle" # default
  )
  class_to_icon <- lapply(class_to_icon, function(icon_name) toString(icon(icon_name, lib = "font-awesome")))

  res <- unname(vapply(
    column_classes,
    function(class) {
      if_null(class_to_icon[[class]], class_to_icon[["unknown"]])
    },
    character(1)
  )) # replace unknown classes by question mark

  return(res)
}

#' Add variable type icons next to variable labels
#'
#' If icons are not displayed, this is a CSS problem. Make sure to include the CSS somewhere,
#' e.g. by calling icon("cog") somewhere and include it in the html
#'
#' @param column_labels (atomic vector of \code{character}) if not specified, icons are prefixed with column name
#' @inheritParams variable_type_icons
#'
#' @return (atomic vector of \code{character}) vector of strings that can be rendered as HTML
#'
#' @export
#'
#' @examples
#' add_variable_type_icons(
#'   c("x", "z"),
#'   data.frame(x = 1:3, y = factor(c("a", "b", "a")), z = c("h1", "h2", "h3"),
#'     stringsAsFactors = FALSE
#'   )
#' )
#' add_variable_type_icons(
#'   c("x", "z"),
#'   data.frame(x = 1:3, y = factor(c("a", "b", "a")), z = c("h1", "h2", "h3"),
#'     stringsAsFactors = FALSE
#'   ),
#'   column_labels = c("x var", "z var")
#' )
#'
#' add_variable_type_icons(character(0), data.frame(x = 1:3))
add_variable_type_icons <- function(columns, data, column_labels = columns) {
  stopifnot(
    is.data.frame(data),
    is.character.vector(columns, min_length = 0),
    all(columns %in% colnames(data)),
    length(columns) == length(column_labels)
  )
  return(
    paste(
      variable_type_icons(columns, data),
      column_labels
    )
  )
}

#' Add a subtext to a given list of choices
#'
#' content and data-subtext don't work in conjunction, subtext creates
#' \code{<small class="text-muted">Study Identifier</small>}
#' The output of this function can be added in choiceOpts as \code{list(content = ...)}
#'
#' @param content (atomic vector of \code{character}) choices as displayed in UI, not their internal values
#' @param subtext (atomic vector of \code{character}) subtexts to display for each item in content
#'
#' @export
#'
#' @return (atomic vector of \code{character}) HTML contents with content and subtext combined
#'
#' @examples
#' add_subtext(c("SEX", "BRMRKR1"), c("Sex", "Biomarker 1"))
#'
#' add_subtext(c("SEX", "BRMRKR1"), c("", "Biomarker 1"))
add_subtext <- function(content, subtext) {
  if (is.null(subtext)) {
    return(content)
  }
  stopifnot(
    is.character.vector(content, min_length = 0),
    is.character.vector(subtext, min_length = 0),
    length(content) == length(subtext)
  )

  # cannot use "", as select input will have size <small> otherwise due to subtext
  content[content == ""] <- toString(tags$i("H", style = "visibility:hidden;"))

  res <- paste(
    content,
    vapply(
      subtext,
      function(x) {
        if_not_cond(x, toString(tags$small(x, class = "text-muted")), function(y) identical(y, ""))
      },
      character(1)
    )
  )

  return(res)
}
