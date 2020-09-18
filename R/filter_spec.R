#' filter spec
#'
#' @md
#' @description `r lifecycle::badge("maturing")`
#' It consists in choices and additionally the variable names for the choices
#'
#' @export
#'
#' @param vars (\code{character}) or (\code{delayed_data}) object.
#'   Character vector giving the columns to be filtered. These should be
#'   key variables of the data set to be filtered.
#'   \code{delayed_data} objects can be created via \code{variable_choices} or \code{value_choices}.
#' @param sep (\code{character}) A separator string to split the \code{choices} or
#'   \code{selected} inputs into the values of the different columns
#' @param choices (\code{character}) or (\code{delayed_data}) object.
#'   Named character vector to define the choices
#'   of a shiny \code{\link[shiny]{selectInput}}. These choices will be used to filter the
#'   dataset.
#'
#'   These shall be filter values of the \code{vars} input separated by the separator(\code{sep}). Please
#'   watch out that the filter values have to follow the order of the \code{vars} input. In the following
#'   example we will show how to filter two columns:
#'
#'    \code{vars = c("PARAMCD","AVISIT")} and \code{choices = c("CRP - BASELINE", "ALT - BASELINE")}
#'  will lead to a filtering of
#'  \code{(PARAMCD == "CRP" & AVISIT == "BASELINE") | (PARAMCD == "ALT" & AVISIT == "BASELINE")}.
#'
#'  The \code{sep} input has to be \code{" - "} in this case.
#'
#'  \code{delayed_data} objects can be created via \code{variable_choices} or \code{value_choices}.
#'
#' @param selected (\code{character} or \code{delayed_data} object) Named character vector to define the selected
#'  values of a shiny \code{\link[shiny]{selectInput}} (default values). This value will
#'  be displayed inside the shiny app upon start.
#'
#' @inheritParams select_spec
#'
#' @return \code{filter_spec}-S3-class object or \code{delayed_filter_spec}-S3-class object.
#'
#' @rdname filter_spec
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
#' filter_spec(
#'   vars = c("PARAMCD"),
#'   sep = " - ",
#'   choices = c("CRP", "ALT"),
#'   selected = c("CRP"),
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
#'   \item{Delayed version}{
#'     \preformatted{
#'       adsl_filter <- filter_spec(
#'         vars = variable_choices("ADSL", "SEX"),
#'         sep = "-",
#'         choices = value_choices("ADSL", "SEX", "SEX"),
#'         selected = "F",
#'         multiple = FALSE,
#'         label = "Choose endpoint and Censor"
#'       )
#'     }
#'   }
#'
#' }
#'
#' @examples
#' filter_spec(
#'   vars = variable_choices("ADSL", "ARMCD"),
#'   choices = value_choices("ADSL", var_choices = "ARMCD", var_label = "ARM",
#'   subset = function(data) {
#'     levels(data$ARMCD)[1:2]
#'   }),
#'   selected = value_choices("ADSL", var_choices = "ARMCD", var_label = "ARM",
#'   subset = function(data) {
#'     levels(data$ARMCD)[1]
#'   })
#' )
filter_spec <- function(vars,
                        choices,
                        selected = `if`(is(choices, "delayed_data"), NULL, choices[1]),
                        multiple = length(selected) > 1,
                        label = "Filter",
                        sep = if_null(attr(choices, "sep"), " - ")) {
  stopifnot(is_logical_single(multiple))
  stopifnot(is_character_single(label))
  stopifnot(is_character_single(sep))

  # class dispatch also on a choices arg
  # filter_spec.delayed_data dispatch on two arguments - s3 can dispatch only on 1 argument
  if (is(choices, "delayed_data") || is(selected, "delayed_data")) {
    filter_spec.delayed_data(vars = vars, choices = choices, selected = selected,
                             multiple = multiple, label = label, sep = sep)
  } else {
    UseMethod("filter_spec")
  }
}

#' @rdname filter_spec
#' @export
filter_spec.delayed_data <- function(vars,
                                     choices,
                                     selected = NULL,
                                     multiple = length(selected) > 1,
                                     label = "Filter",
                                     sep = if_null(attr(choices, "sep"), " - ")) {
  stopifnot(is_character_vector(choices) || is(choices, "delayed_data"))
  stopifnot(is.null(selected) || is_character_vector(selected) || is(selected, "delayed_data"))

  out <- structure(list(vars = vars,
                        choices = choices,
                        selected = selected,
                        multiple = multiple,
                        label = label,
                        sep = sep),
                   class = c("delayed_filter_spec", "delayed_data", "filter_spec"))
  return(out)
}

#' @rdname filter_spec
#' @export
filter_spec.default <- function(vars,
                                choices,
                                selected = choices[1],
                                multiple = length(selected) > 1,
                                label = "Filter",
                                sep = if_null(attr(choices, "sep"), " - ")) {

  stopifnot(is_character_vector(vars))
  stopifnot(is_character_vector(choices))
  stopifnot(is.null(selected) || is_character_vector(selected))

  stopifnot(all(!duplicated(vars)))
  stopifnot(all(!duplicated(choices)))

  choices_attrs <- attributes(choices)
  choices <- split_by_sep(choices, sep)

  stopifnot(all(vapply(choices, length, integer(1)) == length(vars)))

  attributes(choices) <- choices_attrs
  names(choices) <- if_null(names(choices), vapply(choices, paste, collapse = sep, character(1)))


  if (!is.null(selected)) {
    stopifnot(multiple || length(selected) == 1)
    stopifnot(is_character_vector(selected))
    stopifnot(all(!duplicated(selected)))
    selected <- split_by_sep(selected, sep)
    stopifnot(all(selected %is_in% choices))
    names(selected) <- if_null(names(selected), vapply(selected, paste, collapse = sep, character(1)))
  }


  res <- list(vars = vars,
              choices = choices,
              selected = selected,
              multiple = multiple,
              label = label,
              sep = sep)
  class(res) <- "filter_spec"

  return(res)
}
