#' Column selection input specification
#'
#' \code{columns_spec} is used inside \code{\link[teal]{teal}} to create a \code{\link[shiny]{selectInput}}
#' that will select columns from a dataset.
#'
#' @param choices (\code{character}) Named character vector to define the choices
#' 	of a shiny \code{\link[shiny]{selectInput}}. These have to be columns in the
#' 	dataset defined in the \link{DataExtractSpec} where this is called.
#'
#' @param selected (\code{character}) (default value) Named character vector to define the selected
#'  values of a shiny \code{\link[shiny]{selectInput}}. This can be just one column
#'  name or multiple column names.
#'
#' @param multiple (\code{logical}) Whether multiple values shall be allowed in the
#'  shiny \code{\link[shiny]{selectInput}}.
#'
#' @param fixed (\code{logical}) (optional) \link{DataExtractSpec} specific feature to
#'   hide the choices selected in case they are not needed. Setting fixed to \code{TRUE}
#'   will not allow the user to select columns. It will then lead to a selection of
#'   columns in the dataset that is defined by the developer of the app.
#'
#' @param label (\code{logical}) (optional) Define a label
#' on top of this specific shiny \code{\link[shiny]{selectInput}}.
#'
#' @return A \code{column_choices_spec}-S3 class object. It contains all input values.
#' The function double checks the \code{choices} and \code{selected} inputs.
#'
#' @details
#'
#' To give you some more insights into this function there are several examples. These all
#' start by a data set containing the columns \code{"AGE"}, \code{"AVAL"} and \code{"BMRKR1"}.
#'
#' \enumerate{
#'   \item{Selection with just one column allowed }{
#'     \preformatted{
#' columns = columns_spec(
#'   choices = c("AVAL", "BMRKR1", "AGE”),
#'   selected = c("AVAL"),
#'   multiple = FALSE,
#'   fixed = FALSE,
#'   label = "Column"
#' )
#'     }
#'     \if{html}{
#'       \figure{columns_spec_1.png}{options: alt="Selection with just one column allowed"}
#'     }
#'     \if{html}{
#'       \figure{columns_spec_11.png}{options: alt="Selection with just one column allowed"}
#'     }
#'   }
#'   \item{Selection with just multiple columns allowed }{
#'     \preformatted{
#' columns = columns_spec(
#'   choices = c("AVAL", "BMRKR1", "AGE”),
#'   selected = c("AVAL", "BMRKR1"),
#'   multiple = TRUE,
#'   fixed = FALSE,
#'   label = "Columns"
#' )
#'     }
#'     \if{html}{
#'       \figure{columns_spec_2.png}{options: alt="Selection with just multiple columns allowed"}
#'     }
#'     \if{html}{
#'       \figure{columns_spec_21.png}{options: alt="Selection with just multiple columns allowed"}
#'     }
#'   }
#'   \item{Selection without user access }{
#'     \preformatted{
#' columns = columns_spec(
#'   choices = c("AVAL", "BMRKR1"),
#'   selected = c("AVAL", "BMRKR1"),
#'   multiple = TRUE,
#'   fixed = TRUE,
#'   label = "Columns"
#' )
#'     }
#'     \if{html}{
#'       \figure{columns_spec_3.png}{options: alt="Selection without user access"}
#'     }
#'   }
#' }
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
