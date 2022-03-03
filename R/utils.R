# also returns a list if only a single element
#' Split by separator
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @param x (`character`) Character (single)
#' @param sep (`character`) Separator
#' @export
split_by_sep <- function(x, sep) {
  stopifnot(is.atomic(x))
  if (is.character(x)) {
    strsplit(x, sep, fixed = TRUE)
  } else {
    x
  }
}

#' Extract labels from choices basing on attributes and names
#'
#' @param choices (`list` or `vector`) select choices
#' @param values optional, choices subset for which labels should be extracted, `NULL` for all choices
#'
#' @return (`character`) vector with labels
#' @keywords internal
extract_choices_labels <- function(choices, values = NULL) {
  res <- if (is(choices, "choices_labeled")) {
    attr(choices, "raw_labels")
  } else if (!is.null(names(choices)) && !setequal(names(choices), unlist(unname(choices)))) {
    names(choices)
  } else {
    NULL
  }

  if (!is.null(values) && !is.null(res)) {
    stopifnot(all(values %in% choices))
    res <- res[vapply(values, function(val) which(val == choices), numeric(1))]
  }

  return(res)
}

#' Get Client Timezone
#'
#' Local timezone in the browser may differ from the system timezone from the server.
#'   This script can be run to register a shiny input which contains information about
#'   the timezone in the browser.
#'
#' @param ns (`function`) namespace function passed from the `session` object in the
#'   Shiny server. For Shiny modules this will allow for proper name spacing of the
#'   registered input.
#'
#' @return (`Shiny`) input variable accessible with `input$tz` which is a (`character`)
#'  string containing the timezone of the browser/client.
#' @keywords internal
get_client_timezone <- function(ns) {
  script <- sprintf(
    "Shiny.setInputValue(`%s`, Intl.DateTimeFormat().resolvedOptions().timeZone)",
    ns("timezone")
  )
  shinyjs::runjs(script) # function does not return anything
  return(invisible(NULL))
}

#' Check if package can be loaded
#'
#' @param pckg `character` package name.
#' @param msg `character` error message to display if package is not available.
#'
#' @return Error or invisible NULL.
#' @keywords internal
check_pkg_quietly <- function(pckg, msg) {
  checkmate::assert_string(pckg)
  checkmate::assert_string(msg)
  if (!pckg %in% utils::installed.packages()) {
    stop(msg)
  }

  return(invisible(NULL))
}

