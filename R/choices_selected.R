no_select_keyword <- "-- no selection --"

#' Choices Selected
#'
#' @description `r lifecycle::badge("maturing")`
#'   Construct a single list containing available choices, the default selected value, and
#'   additional settings such as to order the choices with the selected elements appearing first
#'   or whether to block the user from making selections. Can be used in `ui` input elements
#'   such as [`optionalSelectInput`]
#'
#' @param choices (`character`) vector of possible choices or `delayed_data` object\cr
#'   See [`variable_choices`] and [`value_choices`].
#' @param selected (`character`) vector of pre-selected options, (`all_choices`) object
#'  or (`delayed_data`) object. If `delayed_data` object then `choices` must also be
#'  a `delayed_data` object. If not supplied it will default to the first element of
#'  `choices` if `choices` is a vector, or `NULL` if `choices` is a `delayed_data` object.
#' @param keep_order (`logical`)\cr
#'  In case of `FALSE` the selected variables will be on top of the drop-down field.
#' @param fixed optional, (`logical`)\cr
#'  Whether to block user to select choices
#'
#' @details
#'
#' Please note that the order of selected will always follow the order of choices. The `keep_order`
#' argument is set to false which will run the following code inside:
#'
#' `choices <- c(selected, setdiff(choices, selected))`
#'
#' in case you want to keep your specific order of choices, set `keep_order` to `TRUE`.
#'
#' @return Object of class `choices_selected` and of type list which contains the specified
#'   `choices`, `selected`, `keep_order` and `fixed`.
#'
#' @export
#'
#' @examples
#'
#' # all_choices example - semantically the same objects
#' choices_selected(choices = letters, selected = all_choices())
#' choices_selected(choices = letters, selected = letters)
#'
#' choices_selected(
#'   choices = setNames(LETTERS[1:5], paste("Letter", LETTERS[1:5])),
#'   selected = "X"
#' )
#'
#' library(scda)
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#' choices_selected(variable_choices(ADSL), "SEX")
#'
#' # How to select nothing
#' # use an empty character
#' choices_selected(
#'   choices = c("", "A", "B", "C"),
#'   selected = ""
#' )
#'
#' # How to allow the user to select nothing
#' # use an empty character
#' choices_selected(
#'   choices = c("A", "", "B", "C"),
#'   selected = "A"
#' )
#'
#'
#' # How to make Nothing the Xth choice
#' # just use keep_order
#' choices_selected(
#'   choices = c("A", "", "B", "C"),
#'   selected = "A",
#'   keep_order = TRUE
#' )
#'
#'
#' # How to give labels to selections
#' # by adding names - choices will be replaced by "name" in UI, not in code
#' choices_selected(
#'   choices = c("name for A" = "A", "Name for nothing" = "", "name for b" = "B", "name for C" = "C"),
#'   selected = "A"
#' )
#'
#' # by using choices_labeled
#' # labels will be shown behind the choice
#' choices_selected(
#'   choices = choices_labeled(
#'     c("A", "", "B", "C"),
#'     c("name for A", "nothing", "name for B", "name for C")
#'   ),
#'   selected = "A"
#' )
#'
#' # Passing a `delayed_data` object to `selected`
#' choices_selected(
#'   choices = variable_choices("ADSL"),
#'   selected = variable_choices("ADSL", subset = c("STUDYID"))
#' )
#'
#' # functional form (subsetting for factor variables only) of choices_selected
#' # with delayed data loading
#' choices_selected(variable_choices("ADSL", subset = function(data) {
#'   idx <- vapply(data, is.factor, logical(1))
#'   return(names(data)[idx])
#' }))
#'
#' # used in a teal `optionalSelectInput`
#' cs <- choices_selected(
#'   choices = c("A", "B", "C"),
#'   selected = "A"
#' )
#'
#' ui <- fluidPage(
#'   optionalSelectInput(
#'     inputId = "id",
#'     choices = cs$choices,
#'     selected = cs$selected
#'   )
#' )
#' \dontrun{
#' shinyApp(ui, server = function(input, output, session) {})
#' }
choices_selected <- function(choices,
                             selected = if (is(choices, "delayed_data")) NULL else choices[1],
                             keep_order = FALSE,
                             fixed = FALSE) {
  stopifnot(is.atomic(choices) || is(choices, "delayed_data"))
  stopifnot(is.atomic(selected) || is(selected, "delayed_data") || is(selected, "all_choices"))
  stopifnot(is_logical_single(keep_order))
  stopifnot(is_logical_single(fixed))

  if (is(selected, "all_choices")) selected <- choices

  if (is(selected, "delayed_data") && !is(choices, "delayed_data")) {
    stop("If 'selected' is of class 'delayed_data', so must be 'choices'.")
  }

  if (is(choices, "delayed_data")) {
    out <- structure(
      list(choices = choices, selected = selected, keep_order = keep_order, fixed = fixed),
      class = c("delayed_choices_selected", "delayed_data", "choices_selected")
    )
    return(out)
  }

  if (!is.null(choices) && no_select_keyword %in% choices) {
    stop(paste(no_select_keyword, "is not a valid choice as it is used as a keyword"))
  }

  # remove duplicates
  choices <- vector_remove_dups(choices)
  selected <- vector_remove_dups(selected)

  if (!keep_order) {
    choices <- vector_reorder(
      choices,
      c(which(choices %in% selected), setdiff(seq_along(choices), which(choices %in% selected)))
    )
  }

  structure(
    list(
      choices = choices,
      selected = selected,
      fixed = fixed
    ),
    class = "choices_selected"
  )
}

#' Check if an object is a choices_selected class.
#'
#' @description `r lifecycle::badge("maturing")`
#'
#' @param x object to check
#' @export
is.choices_selected <- function(x) { # nolint
  is(x, "choices_selected")
}

#' Add empty choice to choices selected
#'
#' @description `r lifecycle::badge("maturing")`
#'
#' @param x (\code{choices_selected}) output
#' @param multiple (\code{logical}) whether multiple selections are allowed or not
#'
#' @export
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

#' Check select choices for no choice made
#'
#' @description `r lifecycle::badge("maturing")`
#'
#' @param x (\code{character}) Word that shall be checked for
#'   NULL, empty, "--no-selection"
#'
#' @return the word or NULL
#'
#' @export
no_selected_as_NULL <- function(x) { # nolint
  if (is.null(x) || identical(x, no_select_keyword) || x == "") {
    NULL
  } else {
    x
  }
}

## Non-exported utils functions ----
## Modify vectors and keep attributes
vector_reorder <- function(vec, idx) {
  stopifnot(is.atomic(vec))
  stopifnot(is_integer_vector(idx))
  stopifnot(length(vec) == length(idx))

  vec_attrs <- attributes(vec)

  vec <- vec[idx]

  for (vec_attrs_idx in seq_along(vec_attrs)) {
    if (length(vec_attrs[[vec_attrs_idx]]) == length(vec)) {
      vec_attrs[[vec_attrs_idx]] <- vec_attrs[[vec_attrs_idx]][idx]
    }
  }

  attributes(vec) <- vec_attrs

  return(vec)
}

vector_pop <- function(vec, idx) {
  stopifnot(is.atomic(vec))
  stopifnot(is_integer_vector(idx, min_length = 0))

  if (length(idx) == 0) {
    return(vec)
  }

  vec_attrs <- attributes(vec)

  for (vec_attrs_idx in seq_along(vec_attrs)) {
    if (length(vec_attrs[[vec_attrs_idx]]) == length(vec)) {
      vec_attrs[[vec_attrs_idx]] <- vec_attrs[[vec_attrs_idx]][-idx]
    }
  }
}

vector_remove_dups <- function(vec) {
  stopifnot(is.atomic(vec))

  idx <- which(duplicated(vec))

  if (length(idx) == 0) {
    return(vec)
  } else if (is.null(attributes(vec))) {
    return(unique(vec))
  } else if (identical(names(attributes(vec)), "names")) {
    return(vec[-idx])
  } else {
    return(vector_pop(vec, idx))
  }
}
