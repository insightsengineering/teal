#' Column selection input specification
#'
#' \code{select_spec} is used inside teal to create a \code{\link[shiny]{selectInput}}
#' that will select columns from a dataset.
#'
#' @param choices (\code{character}) Named character vector to define the choices
#'   of a shiny \code{\link[shiny]{selectInput}}. These have to be columns in the
#'   dataset defined in the \link{data_extract_spec} where this is called.
#'
#' @param selected (\code{character}) (default value) Named character vector to define the selected
#'  values of a shiny \code{\link[shiny]{selectInput}}. This can be just one column
#'  name or multiple column names.
#'
#' @param multiple (\code{logical}) Whether multiple values shall be allowed in the
#'  shiny \code{\link[shiny]{selectInput}}.
#'
#' @param fixed (\code{logical}) (optional) \link{data_extract_spec} specific feature to
#'   hide the choices selected in case they are not needed. Setting fixed to \code{TRUE}
#'   will not allow the user to select columns. It will then lead to a selection of
#'   columns in the dataset that is defined by the developer of the app.
#'
#' @param label (\code{logical}) (optional) Define a label
#' on top of this specific shiny \code{\link[shiny]{selectInput}}.
#'
#' @return A \code{select_spec}-S3 class object. It contains all input values.
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
#' }
#'
#' @importFrom magrittr %<>%
#' @importFrom purrr map_lgl
#' @importFrom stats setNames
#' @export
select_spec <- function(choices,
                        selected = choices[1],
                        multiple = length(selected) > 1,
                        fixed = FALSE,
                        label = "Column(s)") {

  stopifnot(length(choices) >= 1 && is.atomic(choices))
  stopifnot(is_logical_single(multiple))
  stopifnot(is_logical_single(fixed))
  stopifnot(is_character_single(label))

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

  res <- list(choices = choices, selected = selected, multiple = multiple, fixed = fixed, label = label)
  class(res) <- "select_spec"

  return(res)
}
