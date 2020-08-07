#' Set "<choice>: <label>" type of Names
#' This is often useful for \code{\link[teal]{choices_selected}} as it marks up the dropdown boxes
#' for \code{\link[shiny]{selectInput}}.
#' @param choices a character vector
#' @param labels vector containing labels to be applied to \code{choices}
#' @param subset a character vector that is a subset of \code{choices}. This is useful if
#'   only a few variables need to be named. If this argument is used, the returned vector will
#'   match its order.
#' @param types vector containing the types of the columns to be used for applying the appropriate
#'   icons to the \code{\link[teal]{choices_selected}} drop down box
#' @details If either \code{choices} or \code{labels} are factors, they are coerced to character.
#' Duplicated elements from \code{choices} get removed.
#'
#' @return a named character vector
#'
#' @export
#'
#' @examples
#' library(random.cdisc.data)
#'
#' ADSL <- radsl(cached = TRUE)
#' ADTTE <- radtte(cached = TRUE)
#' choices1 <- choices_labeled(names(ADSL), rtables::var_labels(ADSL))
#' choices2 <- choices_labeled(ADTTE$PARAMCD, ADTTE$PARAM)
#' # if only a subset of variables are needed, use subset argument
#' choices3 <- choices_labeled(names(ADSL), rtables::var_labels(ADSL), subset = c("ARMCD", "ARM"))
#'\dontrun{
#' shinyApp(
#'   ui = fluidPage(selectInput("c1", label = "Choices from ADSL",
#'                              choices = choices1,
#'                              selected = choices1[1]),
#'                  selectInput("c2", label = "Choices from ADTTE",
#'                              choices = choices2,
#'                              selected = choices2[1]),
#'                  selectInput("c3", label = "Arm choices from ADSL",
#'                              choices = choices3,
#'                              selected = choices3[1])),
#'   server = function(input, output) {}
#' )
#'}
choices_labeled <- function(choices, labels, subset = NULL, types = NULL) {
  if (is.factor(choices)) {
    choices <- as.character(choices)
  }
  stopifnot(is_character_vector(choices))

  if (is.factor(labels)) {
    labels <- as.character(labels)
  }
  stopifnot(is_character_vector(labels))

  stop_if_not(list(length(choices) == length(labels), "length of choices must be the same as labels"))

  stopifnot(is.null(subset) || is.vector(subset))
  stopifnot(is.null(types) || is.vector(types))

  if (is.vector(types)) {
    stopifnot(length(choices) == length(types))
  }

  if (!is.null(subset)) {
    stop_if_not(list(all(subset %in% choices), "all of subset variables must be in choices"))
    labels <- labels[choices %in% subset]
    types <- types[choices %in% subset]
    choices <- choices[choices %in% subset]
  }

  is_dupl <- duplicated(choices)
  choices <- choices[!is_dupl]
  labels <- labels[!is_dupl]
  types <- types[!is_dupl]

  labels[is.na(labels)] <- "Label Missing"
  raw_labels <- labels
  combined_labels <- paste0(choices, ": ", labels)

  if (!is.null(subset)) {
    ord <- match(subset, choices)
    choices <- choices[ord]
    raw_labels <- raw_labels[ord]
    combined_labels <- combined_labels[ord]
    types <- types[ord]
  }

  choices <- structure(
    choices,
    names = combined_labels,
    raw_labels = raw_labels,
    combined_labels = combined_labels,
    class = c("choices_labeled", "character"),
    types = types
  )

  return(choices)
}


#' Wrapper on \code{\link{choices_labeled}} to label variables basing on existing labels in data
#'
#' @param data (\code{data.frame}) or (\code{character})
#' If \code{data.frame}, then data to extract labels from.
#' If \code{character}, then name of the dataset to extract data from once available.
#' @param subset (\code{character} or \code{function})
#' If \code{character}, then a vector of column names.
#' If \code{function}, then this function is used to determine the possible columns (e.g. all factor columns).
#' In this case, the function must take only single argument "data" and return a character vector.
#' See examples for more details.
#' @inheritParams rtables::var_labels
#'
#' @return named character vector with additional attributes or \code{delayed_data} object
#'
#' @rdname variable_choices
#'
#' @export
#'
#' @examples
#' library(random.cdisc.data)
#' ADRS <- radrs(cached = TRUE)
#'
#' variable_choices(ADRS)
#' variable_choices(ADRS, subset = c("PARAM", "PARAMCD"))
#' variable_choices(ADRS, subset = c("", "PARAM", "PARAMCD"))
#'
#' # delayed version
#' variable_choices("ADRS", subset = c("USUBJID", "STUDYID"))
#'
#' # functional subset (with delayed data) - return only factor variables
#' variable_choices("ADRS", subset = function(data) {
#'   idx <- vapply(data, is.factor, logical(1))
#'   return(names(data)[idx])
#' })
variable_choices <- function(data, subset = NULL, fill = FALSE) {
  stopifnot(is.null(subset) || is_character_vector(subset, min_length = 1L) || is.function(subset))
  stopifnot(is_logical_single(fill))

  UseMethod("variable_choices")
}

#' @rdname variable_choices
#' @export
variable_choices.character <- function(data, subset = NULL, fill = FALSE) {
  out <- structure(list(data = data, subset = subset),
                   class = c("delayed_variable_choices", "delayed_data", "choices_labeled"))
  return(out)
}

#' @rdname variable_choices
#' @export
variable_choices.data.frame <- function(data, subset = NULL, fill = FALSE) { # nolint
  if (is.null(subset)) {
    subset <- names(data)
  } else if (is.function(subset)) {
    subset <- resolve_delayed_expr(subset, ds = data, is_value_choices = FALSE)
  }

  stopifnot(all(subset %in% c("", names(data))))

  if (any(duplicated(subset))) {
    warning("removed duplicated entries in subset:",
            paste(unique(subset[duplicated(subset)]), collapse = ", "))
    subset <- unique(subset)
  }

  res <- if ("" %in% subset) {
    choices_labeled(choices = c("", names(data)),
                    labels = c("", unname(get_variable_labels(data))),
                    subset = subset,
                    types = c("", variable_types(data = data)))
  } else {
    choices_labeled(choices = names(data),
                    labels = unname(get_variable_labels(data)),
                    subset = subset,
                    types = variable_types(data = data))
  }

  return(res)
}

#' @rdname variable_choices
#' @export
variable_choices.RawDataset <- function(data, subset = NULL, fill = FALSE) {
  variable_choices(data = get_raw_data(data),
                   subset = subset,
                   fill = fill)
}

#' @rdname variable_choices
#' @export
variable_choices.NamedDatasetConnector <- function(data, subset = NULL, fill = FALSE) { # nolint
  if (is_pulled(data)) {
    variable_choices(data = get_raw_data(data),
                     subset = subset,
                     fill = fill)
  } else {
    variable_choices(data = get_dataname(data),
                     subset = subset,
                     fill = fill)
  }
}


#' Wrapper on \code{\link{choices_labeled}} to label variable values basing on other variable values
#'
#' @param data (\code{data.frame}) or (\code{character})
#' If \code{data.frame}, then data to extract labels from
#' If \code{character}, then name of the dataset to extract data from once available.
#' @param var_choices (\code{character}) vector with choices column names
#' @param var_label (\code{character}) vector with labels column names
#' @param subset (\code{character} or \code{function})
#' If \code{character}, vector with values to subset.
#' If \code{function}, then this function is used to determine the possible columns (e.g. all factor columns).
#' In this case, the function must take only single argument "data" and return a character vector.
#' See examples for more details.
#' @param sep (\code{character}) separator used in case of multiple column names
#'
#' @return named character vector or \code{delayed_data} object
#'
#' @rdname value_choices
#'
#' @export
#'
#' @examples
#' library(random.cdisc.data)
#' ADRS <- radrs(cached = TRUE)
#'
#' value_choices(ADRS, "PARAMCD", "PARAM", subset = c("BESRSPI", "INVET"))
#' value_choices(ADRS, c("PARAMCD", "ARMCD"), c("PARAM", "ARM"))
#' value_choices(ADRS, c("PARAMCD", "ARMCD"), c("PARAM", "ARM"),
#'   subset = c("BESRSPI - ARM A", "INVET - ARM A", "OVRINV - ARM A"))
#' value_choices(ADRS, c("PARAMCD", "ARMCD"), c("PARAM", "ARM"), sep = " --- ")
#'
#' # delayed version
#' value_choices("ADRS", c("PARAMCD", "ARMCD"), c("PARAM", "ARM"))
#'
#' # functional subset
#' value_choices(ADRS, "PARAMCD", "PARAM", subset = function(data) {
#'   return(levels(data$PARAMCD)[1:2])
#' })
value_choices <- function(data, var_choices, var_label, subset = NULL, sep = " - ") {
  stopifnot(
    is_character_vector(var_choices),
    is_character_vector(var_label),
    length(var_choices) == length(var_label)
  )
  stopifnot(is.null(subset) || is.vector(subset) || is.function(subset))
  UseMethod("value_choices")
}

#' @rdname value_choices
#' @export
value_choices.character <- function(data, var_choices, var_label, subset = NULL, sep = " - ") {
  out <- structure(list(data = data,
                        var_choices = var_choices,
                        var_label = var_label,
                        subset = subset,
                        sep = sep),
                   class = c("delayed_value_choices", "delayed_data", "choices_labeled"))
  return(out)
}

#' @rdname value_choices
#' @export
value_choices.data.frame <- function(data, var_choices, var_label, subset = NULL, sep = " - ") { # nolint
  choices <- apply(data[var_choices], 1, paste, collapse = sep)
  labels <- apply(data[var_label], 1, paste, collapse = sep)
  df <- unique(data.frame(choices, labels, stringsAsFactors = FALSE)) # unique combo of choices x labels

  if (is.function(subset)) {
    subset <- resolve_delayed_expr(subset, ds = data, is_value_choices = TRUE)
  }

  res <- choices_labeled(
    choices = df$choices,
    labels = df$labels,
    subset = subset
  )
  attr(res, "sep") <- sep
  return(res)
}

#' @rdname value_choices
#' @export
value_choices.RawDataset <- function(data, var_choices, var_label, subset = NULL, sep = " - ") {
  value_choices(data = get_raw_data(data),
                var_choices = var_choices,
                var_label = var_label,
                subset = subset,
                sep = sep)
}

#' @rdname value_choices
#' @export
value_choices.NamedDatasetConnector <- function(data, var_choices, var_label, subset = NULL, sep = " - ") { # nolint
  if (is_pulled(data)) {
    value_choices(data = get_raw_data(data),
                  var_choices = var_choices,
                  var_label = var_label,
                  subset = subset,
                  sep = sep)
  } else {
    value_choices(data = get_dataname(data),
                  var_choices = var_choices,
                  var_label = var_label,
                  subset = subset,
                  sep = sep)
  }
}


#' Get classes selected columns from dataset
#'
#' @param data (\code{data.frame}) data to determine variable types from
#' @param columns (atomic vector of \code{character} or \code{NULL}) column names chosen chosen from \code{data},
#'   \code{NULL} for all data columns
#'
#' @return (atomic vector of \code{character}) classes of \code{columns} from provided \code{data}
#'
#' @examples
#' teal:::variable_types(
#'   data.frame(
#'     x = 1:3, y = factor(c("a", "b", "a")), z = c("h1", "h2", "h3"),
#'     stringsAsFactors = FALSE
#'   ),
#'  "x"
#' )
#'
#' teal:::variable_types(
#'   data.frame(x = 1:3, y = factor(c("a", "b", "a")), z = c("h1", "h2", "h3"),
#'     stringsAsFactors = FALSE),
#'   c("x", "z")
#' )
#' teal:::variable_types(data.frame(x = 1:3, y = factor(c("a", "b", "a")), z = c("h1", "h2", "h3"),
#'     stringsAsFactors = FALSE))
variable_types <- function(data, columns = NULL) {
  stopifnot(is.data.frame(data),
            is.null(columns) || is_character_vector(columns, min_length = 0))

  res <- if (is.null(columns)) {
    vapply(
      data,
      function(x) class(x)[[1]],
      character(1),
      USE.NAMES = FALSE
    )
  } else if (is_character_vector(columns)) {
    stopifnot(all(columns %in% names(data) | columns == ""))
    vapply(
      columns,
      function(x) ifelse(x == "", "", class(data[[x]])[[1]]),
      character(1),
      USE.NAMES = FALSE
    )

  } else {
    character(0)
  }

  return(res)
}

#' Print choices_labeled object
#' @inheritParams base::print
#' @export
print.choices_labeled <- function(x, ...) {
  cat(
    sprintf("number of choices: %s \n", length(x)),
    names(x),
    "",
    sep = "\n"
  )

  return(invisible(x))
}
