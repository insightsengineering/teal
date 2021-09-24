#' Column selection input specification
#'
#' @description `r lifecycle::badge("maturing")`
#' \code{select_spec} is used inside teal to create a \code{\link[shiny]{selectInput}}
#' that will select columns from a dataset.
#'
#' @param choices (\code{character}) or (\code{delayed_data}) object.
#'   Named character vector to define the choices
#'   of a shiny \code{\link[shiny]{selectInput}}. These have to be columns in the
#'   dataset defined in the \code{\link{data_extract_spec}} where this is called.
#'   \code{delayed_data} objects can be created via \code{\link{variable_choices}} or \code{\link{value_choices}}.
#'
#' @param selected optional (\code{character} or \code{NULL} or \code{all_choices} or \code{delayed_data} object).
#' Named character vector to define the selected values of a shiny \code{\link[shiny]{selectInput}}.
#' Passing an `all_choices()` object indicates selecting all possible choices.
#' Defaults to the first value of \code{choices} or \code{NULL} for delayed data loading.
#'
#' @param multiple (\code{logical}) Whether multiple values shall be allowed in the
#'  shiny \code{\link[shiny]{selectInput}}.
#'
#' @param fixed optional (\code{logical}). \code{\link{data_extract_spec}} specific feature to
#'   hide the choices selected in case they are not needed. Setting fixed to \code{TRUE}
#'   will not allow the user to select columns. It will then lead to a selection of
#'   columns in the dataset that is defined by the developer of the app.
#'
#' @param always_selected (\code{character}) Additional column names from the data set that should
#'   always be selected
#'
#' @param label optional (\code{character}). Define a label
#' on top of this specific shiny \code{\link[shiny]{selectInput}}.
#'
#' @return A \code{select_spec}-S3 class object or \code{delayed_select_spec}-S3-class object.
#' It contains all input values.
#' If \code{select_spec}, then the function double checks the \code{choices} and \code{selected} inputs.
#'
#' @details
#'
#' To give you some more insights into this function there are several examples. These all
#' start by a data set containing the columns \code{"AGE"}, \code{"AVAL"} and \code{"BMRKR1"}.
#'
#' \enumerate{
#'   \item{Selection with just one column allowed }{
#'     \preformatted{
#' select = select_spec(
#'   choices = c("AVAL", "BMRKR1", "AGE"),
#'   selected = c("AVAL"),
#'   multiple = FALSE,
#'   fixed = FALSE,
#'   label = "Column"
#' )
#'     }
#'     \if{html}{
#'       \figure{select_spec_1.png}{options: alt="Selection with just one column allowed"}
#'     }
#'     \if{html}{
#'       \figure{select_spec_11.png}{options: alt="Selection with just one column allowed"}
#'     }
#'   }
#'   \item{Selection with just multiple columns allowed }{
#'     \preformatted{
#' select = select_spec(
#'   choices = c("AVAL", "BMRKR1", "AGE"),
#'   selected = c("AVAL", "BMRKR1"),
#'   multiple = TRUE,
#'   fixed = FALSE,
#'   label = "Columns"
#' )
#'     }
#'     \if{html}{
#'       \figure{select_spec_2.png}{options: alt="Selection with just multiple columns allowed"}
#'     }
#'     \if{html}{
#'       \figure{select_spec_21.png}{options: alt="Selection with just multiple columns allowed"}
#'     }
#'   }
#'   \item{Selection without user access }{
#'     \preformatted{
#' select = select_spec(
#'   choices = c("AVAL", "BMRKR1"),
#'   selected = c("AVAL", "BMRKR1"),
#'   multiple = TRUE,
#'   fixed = TRUE,
#'   label = "Columns"
#' )
#'     }
#'     \if{html}{
#'       \figure{select_spec_3.png}{options: alt="Selection without user access"}
#'     }
#'   }
#'   \item{Delayed version}{
#'     \preformatted{
#'       adsl_select <- select_spec(
#'         label = "Select variable:",
#'         choices = variable_choices("ADSL", c("BMRKR1", "BMRKR2")),
#'         selected = "BMRKR1",
#'         multiple = FALSE,
#'         fixed = FALSE
#'       )
#'     }
#'   }
#'   \item{all_choices passed to selected}{
#'     \preformatted{
#'       adsl_select <- select_spec(
#'         label = "Select variable:",
#'         choices = variable_choices("ADSL", c("BMRKR1", "BMRKR2")),
#'         selected = all_choices()
#'       )
#'     }
#'   }
#' }
#'
#' @rdname select_spec
#'
#' @export
#'
#' @examples
#' # functional form (subsetting for factor variables only) of select_spec with delayed data loading
#' select_spec(
#'   choices = variable_choices("ADSL", subset = function(data) {
#'     idx <- vapply(data, is.factor, logical(1))
#'     return(names(data)[idx])
#'   }),
#'   # setting first factor variable as default
#'   selected = variable_choices("ADSL", subset = function(data) {
#'     idx <- vapply(data, is.factor, logical(1))
#'     return(names(data)[idx][1])
#'   }),
#'   multiple = TRUE
#' )
#'
#' # Both below objects are semantically the same
#' select_spec(choices = variable_choices("ADSL"), selected = variable_choices("ADSL"))
#' select_spec(choices = variable_choices("ADSL"), selected = all_choices())
#'
select_spec <- function(choices,
                        selected = `if`(is(choices, "delayed_data"), NULL, choices[1]),
                        multiple = length(selected) > 1 || is(selected, "all_choices"),
                        fixed = FALSE,
                        always_selected = NULL,
                        label = NULL) {
  stopifnot(is_logical_single(multiple))
  stopifnot(is_logical_single(fixed))
  stopifnot(is.null(always_selected) || is_character_vector(always_selected, 1))
  stopifnot(is.null(label) || is_character_single(label))
  stopifnot(multiple || !is(selected, "all_choices"))
  if (fixed) stopifnot(is.null(always_selected))

  if (is(selected, "all_choices")) selected <- choices
  if (is(choices, "delayed_data") || is(selected, "delayed_data")) {
    select_spec.delayed_data(choices, selected, multiple, fixed, always_selected, label)
  } else {
    select_spec.default(choices, selected, multiple, fixed, always_selected, label)
  }
}

#' @rdname select_spec
#' @export
select_spec.delayed_data <- function(choices,
                                     selected = NULL,
                                     multiple = length(selected) > 1,
                                     fixed = FALSE,
                                     always_selected = NULL,
                                     label = NULL) {
  stopifnot(is.null(selected) || is.atomic(selected) || is(selected, "delayed_data"))
  stopifnot(is.null(choices) || is.atomic(choices) || is(choices, "delayed_data"))

  structure(
    list(
      choices = choices,
      selected = selected,
      always_selected = always_selected,
      multiple = multiple,
      fixed = fixed,
      label = label),
    class = c("delayed_select_spec", "delayed_data", "select_spec")
  )
}

#' @rdname select_spec
#' @export
select_spec.default <- function(choices,
                                selected = choices[1],
                                multiple = length(selected) > 1,
                                fixed = FALSE,
                                always_selected = NULL,
                                label = NULL) {
  stopifnot(is.null(selected) || is.atomic(selected))

  # if names is NULL, shiny will put strange labels (with quotes etc.) in the selectInputs, so we set it to the values
  if (is.null(names(choices))) {
    names(choices) <- as.character(choices)
  }

  # Deal with selected
  if (length(selected) > 0) {
    stopifnot(is.atomic(selected))
    stopifnot(all(selected %in% choices))
    stopifnot(multiple || length(selected) == 1)
    if (is.null(names(selected))) {
      names(selected) <- as.character(selected)
    }
  }

  if (length(intersect(choices, always_selected)) > 0) {
    warning("You cannot allow the user to select 'always_selected' columns.
      'choices' and 'always_selected' will be intersected")
    test_c <- choices[which(!choices %in% always_selected)]
    if (length(test_c) > 0) {
      class(test_c) <- c("choices_labeled", "character")
      choices <- test_c
    } else {
      choices <- NULL
    }
  }

  res <- list(choices = choices, selected = selected, always_selected = always_selected, multiple = multiple,
    fixed = fixed, label = label)
  class(res) <- "select_spec"

  return(res)
}
