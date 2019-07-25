#' filter spec
#'
#' It consists in choices and additionally the variable names for the choices
#'
#' @export
#'
#' @param vars (\code{character}) Character vector giving the columns to be filtered. These should be
#'   key variables of the data set to be filtered.
#' @param sep (\code{character}) A separator string to split the \code{choices} or
#'   \code{selected} inputs into the values of the different columns
#' @param choices (\code{character}) Named character vector to define the choices
#'   of a shiny \code{\link[shiny]{selectInput}}. These choices will be used to filter the
#'   dataset.
#'
#'   These shall be filter values of the \code{vars} input separated by the separator(\code{sep}). Please
#'   watch out that the filter values have to follow the order of the \code{vars} input. In the following
#'   example we will show how to filter two columns:
#'
#'    \code{vars = c("PARAMCD","AVISITN")} and \code{choices = c("CRP - BASELINE", "ALT - BASELINE")}
#'  will lead to a filtering of
#'  \code{(PARAMCD == "CRP" & AVISITN == "BASELINE") | (PARAMCD == "ALT" & AVISITN == "BASELINE")}.
#'
#'  The \code{sep} input has to be \code{" - "} in this case.
#'
#' @param selected (\code{character}) Named character vector to define the selected
#'  values of a shiny \code{\link[shiny]{selectInput}} (default values). This value will
#'  be displayed inside the shiny app upon start.
#'  Please check the \code{choices} description for further
#'  details
#'
#' @param multiple (\code{logical}) Whether multiple values shall be allowed in the
#'  shiny \code{\link[shiny]{selectInput}}.
#'
#' @param label (\code{character}) Label on top of the shiny \code{\link[shiny]{selectInput}}
#'  created from this specification.
#'
#' @return \code{filter_spec}-S3-class object
#'
#' @examples
#' filter_spec(
#'   vars = c("PARAMCD", "AVISIT"),
#'   sep = " - ",
#'   choices = c("CRP - BASELINE", "CRP - SCREENING", "ALT - BASELINE"),
#'   selected = c("CRP - BASELINE"),
#'   multiple = TRUE
#' )
#'
#' @details
#'
#' The \code{filter_spec} is used inside \code{teal} apps to allow filtering datasets
#' for their key variables. Imagine having an adverse events table. It has
#' the columns \code{PARAMCD} and \code{CNSR}. \code{PARAMCD} contains the levels
#' \code{"OS"}, \code{"PFS"}, \code{"EFS"}. \code{CNSR} contains the levels \code{"0"} and \code{"1"}.
#' The following examples should show how a \code{filter_spec} setup will influence
#' the drop-down menu the app user will see:
#'
#' \enumerate{
#'   \item{Filtering two variables }{
#'     \preformatted{
#'      filter_spec(
#'        vars = c("PARAMCD", "CNSR"),
#'        sep = "-",
#'        choices = c("OS-1" = "OS-1", "OS-0" = "OS-0", "PFS-1" = "PFS-1"),
#'        selected = "OS-1",
#'        multiple = FALSE,
#'        label = "Choose endpoint and Censor"
#'      )
#'    }
#'
#'    \if{html}{
#'      \figure{filter_spec_1.png}{options: alt="Filtering two variables"}
#'    }
#'    \if{html}{
#'      \figure{filter_spec_11.png}{options: alt="Filtering two variables"}
#'    }
#'   }
#'
#'   \item{Filtering a single variable }{
#'     \preformatted{
#'      filter_spec(
#'        vars = c("PARAMCD"),
#'        sep = "-",
#'        choices = c("OS", "PFS", "EFS"),
#'        selected = "OS",
#'        multiple = FALSE,
#'        label = "Choose endpoint"
#'      )
#'    }
#'
#'    \if{html}{
#'      \figure{filter_spec_2.png}{options: alt="Filtering two variables"}
#'    }
#'    \if{html}{
#'      \figure{filter_spec_21.png}{options: alt="Filtering two variables"}
#'    }
#'   }
#'
#'   \item{Filtering a single variable with multiple selections }{
#'     \preformatted{
#'      filter_spec(
#'        vars = c("PARAMCD"),
#'        sep = "-",
#'        choices = c("OS", "PFS", "EFS"),
#'        selected = c("OS", "PFS"),
#'        multiple = TRUE,
#'        label = "Choose endpoint"
#'      )
#'    }
#'
#'    \if{html}{
#'      \figure{filter_spec_3.png}{options: alt="Filtering two variables"}
#'    }
#'    \if{html}{
#'      \figure{filter_spec_31.png}{options: alt="Filtering two variables"}
#'    }
#'   }
#'
#' }
#'
#'
#' @importFrom utils.nest is.character.single is.logical.single
filter_spec <- function(vars,
                        choices,
                        selected = choices[1],
                        multiple = length(selected) > 1,
                        label = "Filter",
                        sep = " - ") {
  stopifnot(is.atomic(vars))
  stopifnot(is.atomic(choices))
  stopifnot(all(is.character(vars)))
  stopifnot(all(is.character(choices)))
  stopifnot(all(!duplicated(vars)))
  stopifnot(all(!duplicated(choices)))
  stopifnot(is.character.single(sep))
  stopifnot(is.logical.single(multiple))
  stopifnot(is.character.single(label))

  choices <- split_by_sep(choices, sep)
  stopifnot(all(vapply(choices, length, 0) == length(vars)))

  if (!is.null(selected) && selected != "__NONE__") {

    stopifnot(is.atomic(selected))
    stopifnot(all(!duplicated(selected)))
    stopifnot(all(is.character(selected)))
    selected <- split_by_sep(selected, sep)
  } else {
    selected <- NULL
  }

  res <- append(
      list(vars),
      columns_spec(choices = choices, selected = selected, multiple, label = label)
  )
  names(res)[1] <- "vars"
  class(res) <- "filter_spec"
  res
}
