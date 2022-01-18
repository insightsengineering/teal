#' Data extract filter specification
#'
#' @description `r lifecycle::badge("stable")`
#' It consists in choices and additionally the variable names for the choices
#'
#' @export
#'
#' @inheritParams select_spec
#'
#' @param vars (\code{character}) or (\code{delayed_data}) object.
#'   Character vector giving the columns to be filtered. These should be
#'   key variables of the data set to be filtered.
#'   \code{delayed_data} objects can be created via \code{\link{variable_choices}}, \code{\link{value_choices}},
#'   or \code{\link{choices_selected}}.
#' @param sep (\code{character}) A separator string to split the \code{choices} or
#'   \code{selected} inputs into the values of the different columns
#' @param choices (\code{character} or \code{numeric} or \code{logical} or (\code{delayed_data}) object.
#'   Named character vector to define the choices
#'   of a shiny \code{\link[shiny]{selectInput}}. These choices will be used to filter the
#'   dataset.
#'
#'   These shall be filter values of the \code{vars} input separated by the separator(\code{sep}). Please
#'   watch out that the filter values have to follow the order of the \code{vars} input. In the following
#'   example we will show how to filter two columns:
#'
#'   \code{vars = c("PARAMCD","AVISIT")} and \code{choices = c("CRP - BASELINE", "ALT - BASELINE")}
#'   will lead to a filtering of
#'   \code{(PARAMCD == "CRP" & AVISIT == "BASELINE") | (PARAMCD == "ALT" & AVISIT == "BASELINE")}.
#'
#'   The \code{sep} input has to be \code{" - "} in this case.
#'
#'   \code{delayed_data} objects can be created via \code{\link{variable_choices}} or \code{\link{value_choices}}.
#'
#' @param selected (\code{character} or \code{numeric} or \code{logical} or
#'  (\code{delayed_data} or (\code{all_choices})) object.
#'  Named character vector to define the selected
#'  values of a shiny \code{\link[shiny]{selectInput}} (default values). This value will
#'  be displayed inside the shiny app upon start. The `all_choices` object indicates selecting
#'  all possible choices.
#'
#' @param drop_keys optional, (\code{logical}) whether to drop filter column from the dataset keys,
#'   \code{TRUE} on default
#'
#' @return \code{filter_spec}-S3-class object or \code{delayed_filter_spec}-S3-class object.
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
#'   \item{Filtering a single variable by multiple levels of the variable}{
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
#'   \item{Using \code{choices_selected}
#'     \preformatted{
#'       adsl_filter <- filter_spec(
#'         vars = choices_selected(variable_choices("ADSL", subset = c("SEX", "AGE")), "SEX", fixed = FALSE),
#'         multiple = TRUE
#'       )
#'
#'       adsl_filter2 <- filter_spec(
#'         vars = choices_selected(variable_choices("ADSL"), "SEX", fixed = TRUE),
#'         multiple = TRUE
#'       )
#'     }
#'   }
#'   \item{Choose all choices
#'     \preformatted{
#'       adsl_filter <- filter_spec(
#'         vars = choices_selected(variable_choices("ADSL"), "SEX", fixed = FALSE),
#'         choices = value_choices("ADSL", "SEX"),
#'         selected = all_choices()
#'       )
#'     }
#'   }
#' }
#'
#' @examples
#' filter_spec(
#'   vars = variable_choices("ADSL", "ARMCD"),
#'   choices = value_choices("ADSL",
#'     var_choices = "ARMCD", var_label = "ARM",
#'     subset = function(data) {
#'       levels(data$ARMCD)[1:2]
#'     }
#'   ),
#'   selected = value_choices("ADSL",
#'     var_choices = "ARMCD", var_label = "ARM",
#'     subset = function(data) {
#'       levels(data$ARMCD)[1]
#'     }
#'   )
#' )
filter_spec <- function(vars,
                        choices = NULL,
                        selected = `if`(is(choices, "delayed_data"), NULL, choices[1]),
                        multiple = length(selected) > 1 || is(selected, "all_choices"),
                        label = NULL,
                        sep = attr(choices, "sep"),
                        drop_keys = FALSE) {
  if (is.null(sep)) sep <- " - "
  checkmate::assert(
    checkmate::check_character(vars, min.len = 1, any.missing = FALSE),
    checkmate::check_class(vars, "delayed_data"),
    checkmate::check_class(vars, "choices_selected")
  )
  checkmate::assert(
    checkmate::check_null(choices),
    checkmate::check_character(choices, min.len = 1, any.missing = FALSE),
    checkmate::check_numeric(choices, min.len = 1, any.missing = FALSE),
    checkmate::check_logical(choices, min.len = 1, any.missing = FALSE),
    checkmate::check_class(choices, "delayed_data")
  )
  checkmate::assert(
    checkmate::check_null(selected),
    checkmate::check_character(selected, min.len = 1, any.missing = FALSE),
    checkmate::check_numeric(selected, min.len = 1, any.missing = FALSE),
    checkmate::check_logical(selected, min.len = 1, any.missing = FALSE),
    checkmate::check_class(selected, "delayed_data"),
    checkmate::check_class(selected, "all_choices")
  )

  checkmate::assert_flag(multiple)
  checkmate::assert_string(label, null.ok = TRUE)
  checkmate::assert_string(sep)
  checkmate::assert_flag(drop_keys)
  stopifnot(multiple || !is(selected, "all_choices"))

  if (is(selected, "all_choices") && !is.null(choices)) selected <- choices

  if (is(vars, "choices_selected")) {
    filter_spec_internal(
      vars_choices = vars$choices,
      vars_selected = vars$selected,
      vars_label = if (vars$fixed) NULL else label,
      vars_fixed = vars$fixed,
      vars_multiple = if (is.null(vars$selected)) FALSE else length(vars$selected) > 1,
      choices = choices,
      selected = selected,
      label = if (vars$fixed) label else NULL,
      fixed = FALSE,
      multiple = multiple,
      sep = sep,
      drop_keys = drop_keys
    )
  } else {
    filter_spec_internal(
      vars_choices = vars,
      vars_selected = vars,
      vars_label = NULL,
      vars_fixed = TRUE,
      vars_multiple = TRUE,
      choices = choices,
      selected = selected,
      label = label,
      fixed = FALSE,
      multiple = multiple,
      sep = sep,
      drop_keys = drop_keys
    )
  }
}


#' Data extract dynamic filter specification
#'
#' This function returns a configuration for the \code{data_extract_ui} module. This function covers
#' the configuration of filtering datasets (so called `filter_spec`), which then is used to build
#' the UI element in the `teal` app.
#'
#' @inheritParams filter_spec
#' @param vars_choices (`character` or `delayed_data`) \cr
#'   the vector of dataset column names available to build dynamic filter
#'   \code{delayed_data} objects can be created via \code{\link{variable_choices}}.
#' @param vars_selected (`NULL` or named `character`) \cr
#'   the selected column name out from `choices`.
#' @param vars_label (`character`)\cr
#'   the title printed on the UI element generated on the basis of this \code{filter_spec}.
#' @param vars_fixed (`logical`)\cr
#'   if true allow to change the selected variables in the UI element; otherwise, do not allow.
#' @param vars_multiple (`logical`)\cr
#'   if true allow to select multiple variables in the UI elements; otherwise, do not allow.
#' @param fixed (`logical`)\cr
#'   if true allow to change the initially selected values of the variables; otherwise, do not allow.
#' @param dataname (`character`)\cr
#'   the name of the dataset this filter covers. Set during the initialization of the teal application.
#' @param initialized (`logical`)\cr
#'   indicates whether this filter was already initialized in the application.
#'   TRUE if this filter was already consumed by the server function; FALSE otherwise.
#'
#' @return `filter_spec` or `delayed_filter_spec` S3-class object.
#' @keywords internal
#'
#' @seealso filter_spec
#'
#' @examples
#' teal:::filter_spec_internal(
#'   vars_choices = c("PARAMCD", "AVISIT"),
#'   vars_selected = "PARAMCD",
#'   vars_multiple = TRUE
#' )
#'
#' library(scda)
#' ADRS <- synthetic_cdisc_data("latest")$adrs
#' teal:::filter_spec_internal(
#'   vars_choices = variable_choices(ADRS),
#'   vars_selected = "PARAMCD",
#'   vars_multiple = TRUE
#' )
#'
#' teal:::filter_spec_internal(
#'   vars_choices = variable_choices("ADRS"),
#'   vars_selected = "PARAMCD",
#'   vars_multiple = TRUE
#' )
filter_spec_internal <- function(vars_choices,
                                 vars_selected = NULL,
                                 vars_label = NULL,
                                 vars_fixed = FALSE,
                                 vars_multiple = TRUE,
                                 choices = NULL,
                                 selected = NULL,
                                 label = NULL,
                                 fixed = FALSE,
                                 multiple = TRUE,
                                 sep = attr(vars_choices, "sep"),
                                 drop_keys = FALSE,
                                 dataname = NULL,
                                 initialized = FALSE) {
  if (is.null(sep)) sep <- " - "
  checkmate::assert_string(vars_label, null.ok = TRUE)
  checkmate::assert_flag(vars_fixed)
  checkmate::assert_flag(vars_multiple)
  checkmate::assert_string(label, null.ok = TRUE)
  checkmate::assert_flag(fixed)
  checkmate::assert_flag(multiple)
  checkmate::assert_string(sep)
  checkmate::assert_flag(drop_keys)

  if (is(vars_choices, "delayed_data") ||
    is(vars_selected, "delayed_data") ||
    is(choices, "delayed_data") ||
    is(selected, "delayed_data")) {
    filter_spec_internal.delayed_data(
      vars_choices = vars_choices,
      vars_selected = vars_selected,
      vars_label = vars_label,
      vars_fixed = vars_fixed,
      vars_multiple = vars_multiple,
      choices = choices,
      selected = selected,
      label = label,
      multiple = multiple,
      fixed = fixed,
      sep = sep,
      drop_keys = drop_keys,
      dataname = dataname,
      initialized = initialized
    )
  } else {
    UseMethod("filter_spec_internal")
  }
}

#' @rdname filter_spec_internal
#' @export
filter_spec_internal.delayed_data <- function(vars_choices, # nolint
                                              vars_selected = NULL,
                                              vars_label = NULL,
                                              vars_fixed = FALSE,
                                              vars_multiple = TRUE,
                                              choices = NULL,
                                              selected = NULL,
                                              label = NULL,
                                              fixed = FALSE,
                                              multiple = TRUE,
                                              sep = attr(vars_choices, "sep"),
                                              drop_keys = FALSE,
                                              dataname = NULL,
                                              initialized = FALSE) {
  if (is.null(sep)) sep <- " - "
  checkmate::assert(
    checkmate::check_character(vars_choices, min.len = 1, any.missing = FALSE),
    checkmate::check_numeric(vars_choices, min.len = 1, any.missing = FALSE),
    checkmate::check_logical(vars_choices, min.len = 1, any.missing = FALSE),
    checkmate::check_class(vars_choices, "delayed_data")
  )

  checkmate::assert(
    checkmate::check_null(vars_selected),
    checkmate::check_character(vars_selected, min.len = 1, any.missing = FALSE),
    checkmate::check_numeric(vars_selected, min.len = 1, any.missing = FALSE),
    checkmate::check_logical(vars_selected, min.len = 1, any.missing = FALSE),
    checkmate::check_class(vars_selected, "delayed_data")
  )

  checkmate::assert(
    checkmate::check_null(choices),
    checkmate::check_character(choices, min.len = 1, any.missing = FALSE),
    checkmate::check_numeric(choices, min.len = 1, any.missing = FALSE),
    checkmate::check_logical(choices, min.len = 1, any.missing = FALSE),
    checkmate::check_class(choices, "delayed_data")
  )

  checkmate::assert(
    checkmate::check_null(selected),
    checkmate::check_character(selected, min.len = 1, any.missing = FALSE),
    checkmate::check_numeric(selected, min.len = 1, any.missing = FALSE),
    checkmate::check_logical(selected, min.len = 1, any.missing = FALSE),
    checkmate::check_class(selected, "delayed_data"),
    checkmate::check_class(selected, "all_choices")
  )

  out <- structure(
    list(
      vars_choices = vars_choices,
      vars_selected = vars_selected,
      vars_label = vars_label,
      vars_fixed = vars_fixed,
      vars_multiple = vars_multiple,
      choices = choices,
      selected = selected,
      label = label,
      multiple = multiple,
      fixed = fixed,
      sep = sep,
      drop_keys = drop_keys,
      dataname = dataname, # modified by data_extract_spec,
      initialized = initialized
    ),
    class = c(
      "delayed_filter_spec",
      "filter_spec",
      "delayed_data"
    )
  )
  return(out)
}

#' @rdname filter_spec_internal
#' @export
filter_spec_internal.default <- function(vars_choices,
                                         vars_selected = NULL,
                                         vars_label = NULL,
                                         vars_fixed = FALSE,
                                         vars_multiple = TRUE,
                                         choices = NULL,
                                         selected = NULL,
                                         label = NULL,
                                         fixed = FALSE,
                                         multiple = TRUE,
                                         sep = attr(vars_choices, "sep"),
                                         drop_keys = FALSE,
                                         dataname = NULL,
                                         initialized = FALSE) {
  if (is.null(sep)) sep <- " - "
  checkmate::assert(
    checkmate::check_character(vars_choices, min.len = 1, any.missing = FALSE),
    checkmate::check_numeric(vars_choices, min.len = 1, any.missing = FALSE),
    checkmate::check_logical(vars_choices, min.len = 1, any.missing = FALSE)
  )
  stopifnot(all(!duplicated(vars_choices)))

  if (!is.null(vars_selected)) {
    stopifnot(vars_multiple || length(vars_selected) == 1)
    checkmate::assert(
      checkmate::check_character(vars_selected, min.len = 1, any.missing = FALSE),
      checkmate::check_numeric(vars_selected, min.len = 1, any.missing = FALSE),
      checkmate::check_logical(vars_selected, min.len = 1, any.missing = FALSE)
    )
    stopifnot(all(!duplicated(vars_selected)))
    stopifnot(all(vars_selected %in% vars_choices))
  }

  if (!is.null(choices)) {
    stopifnot(all(!duplicated(choices)))
    split_choices <- split_by_sep(choices, sep)
    stopifnot(all(vapply(split_choices, length, integer(1)) == length(vars_selected)))
  }

  if (!is.null(selected) && !is(selected, "all_choices")) {
    stopifnot(multiple || length(selected) == 1)
    checkmate::assert(
      checkmate::check_character(selected, min.len = 1, any.missing = FALSE),
      checkmate::check_numeric(selected, min.len = 1, any.missing = FALSE),
      checkmate::check_logical(selected, min.len = 1, any.missing = FALSE)
    )
    stopifnot(all(!duplicated(selected)))
    stopifnot(all(selected %in% choices))
  }

  res <- list(
    vars_choices = vars_choices,
    vars_selected = vars_selected,
    vars_label = vars_label,
    vars_fixed = vars_fixed,
    vars_multiple = vars_multiple,
    choices = choices,
    selected = selected,
    label = label,
    multiple = multiple,
    fixed = fixed,
    sep = sep,
    drop_keys = drop_keys,
    dataname = dataname, # modified by data_extract_spec
    initialized = initialized
  )
  class(res) <- "filter_spec"

  return(res)
}
