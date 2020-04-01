no_select_keyword <- "-- no selection --"

#' Choices function
#'
#' @param choices vector of possible choices
#' @param selected vector of pre-selected options, first element of \code{choices} if blank
#' @param keep_order (\code{logical}) In case of \code{FALSE} the selected variables
#'   will be on top of the drop-down field.
#' @param fixed (\code{logical}) (optional) whether to block user to select choices
#'
#' @details
#'
#' Please note that the order of selected will always follow the order of choices. The \code{keep_order}
#' argument is set to false which will run the following code inside:
#'
#' \code{choices <- c(selected, setdiff(choices, selected))}
#'
#' in case you want to keep your specific order of choices, set \code{keep_order} to \code{TRUE}.
#'
#' @export
#'
#' @examples
#'
#' choices_selected(
#'    choices = setNames(LETTERS[1:5], paste("Letter", LETTERS[1:5])),
#'    selected = "X"
#' )
#'
#' library(random.cdisc.data)
#' ADSL <- radsl(cached = TRUE)
#' choices_selected(variable_choices(ADSL), "SEX")
choices_selected <- function(choices, selected = choices[1], keep_order = FALSE, fixed = FALSE) {

  stopifnot(is.atomic(choices))

  if (!is.null(choices) && no_select_keyword %in% choices) {
    stop(paste(no_select_keyword, "is not a valid choice as it is used as a keyword"))
  }

  # remove duplicates
  choices <- vector_remove_dups(choices)
  selected <- vector_remove_dups(selected)

  # add selected choices if they are missing
  selected_to_add_idx <- which(!(selected %in% choices))
  if (length(selected_to_add_idx) > 0) {
    selected_to_add <- vector_keep(selected, selected_to_add_idx)

    choices <- vector_append(choices, selected_to_add)
  }


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
#' @param x object to check
#'
#' @export
is.choices_selected <- function(x) { # nolint
  is(x, "choices_selected")
}

#' Add empty choice to choices selected
#'
#' @param x (\code{choices_selected}) output
#' @param multiple (\code{logical}) whether multiple selections are allowed or not
#'
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
#' @export
#'
#' @param x (\code{character}) Word that shall be checked for
#'   NULL, empty, "--no-selection"
#'
#' @return the word or NULL
no_selected_as_NULL <- function(x) { #nolint
  if (is.null(x) || identical(x, no_select_keyword) || x == "") {
    NULL
  } else {
    x
  }
}


## Non-exported utils functions ----
## Modify vectors and keep attributes
#' @importFrom utils head tail
vector_append <- function(vec1, vec2, idx = seq_along(vec2)) {
  stopifnot(is.atomic(vec1))
  stopifnot(is.atomic(vec2))
  stopifnot(is_integer_vector(idx))
  stopifnot(length(vec2) == length(idx))

  vector_append_internal <- function(x, elem, idx = seq_along(elem)) {
    res <- x
    for (i in seq_along(idx)) {
      res <- c(head(res, idx[i] - 1L), elem[i], `if`(idx[i] == 1L, res, tail(res, -idx[i] + 1L)))
    }
    return(res)
  }

  vec_attrs <- vec1_attrs <- attributes(vec1)
  vec2_attrs <- attributes(vec2)

  for (vec_attrs_idx in seq_along(vec_attrs)) {
    if (length(vec_attrs[[vec_attrs_idx]]) == length(vec1)) {
      attr_name <- names(vec_attrs)[[vec_attrs_idx]]
      if (names(vec_attrs)[[vec_attrs_idx]] %in% names(vec2_attrs)) {
        vec_attrs[[attr_name]] <- vector_append_internal(vec1_attrs[[attr_name]], vec2_attrs[[attr_name]], idx)
      } else {
        vec_attrs[[attr_name]] <- vector_append_internal(vec1_attrs[[attr_name]], NA, idx)
      }
    }
  }

  vec <- vector_append_internal(vec1, vec2, idx)

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

  vec <- vec[-idx]
  attributes(vec) <- vec_attrs

  return(vec)
}
vector_keep <- function(vec, idx) {
  stopifnot(is.atomic(vec))
  stopifnot(is_integer_vector(idx))

  vector_pop(vec, setdiff(seq_along(vec), idx))
}
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
vector_remove_dups <- function(vec) {
  stopifnot(is.atomic(vec))

  idx <- which(duplicated(vec))

  if (length(idx) == 0) {
    return(vec)
  } else {
    return(vector_pop(vec, idx))
  }
}
