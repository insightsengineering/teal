#' An S3 structure representing the selection of all
#' possible choices in a `filter_spec`, `select_spec` or `choices_selected` object.
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @return `all_choices` object
#'
#' @examples
#' # Both structures are semantically identical
#' filter_spec(
#'   vars = c("selected_variable"),
#'   choices = c("value1", "value2"),
#'   selected = c("value1", "value2")
#' )
#'
#' filter_spec(
#'   vars = c("selected_variable"),
#'   choices = c("value1", "value2"),
#'   selected = all_choices()
#' )
#'
#' choices_selected(choices = letters, selected = letters)
#' choices_selected(choices = letters, selected = all_choices())
#' @export
all_choices <- function() {
  structure(list(), class = "all_choices")
}
