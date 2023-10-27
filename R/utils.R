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

#' Resolve the expected bootstrap theme
#' @keywords internal
get_teal_bs_theme <- function() {
  bs_theme <- getOption("teal.bs_theme")
  if (is.null(bs_theme)) {
    NULL
  } else if (!inherits(bs_theme, "bs_theme")) {
    warning("teal.bs_theme has to be of a bslib::bs_theme class, the default shiny bootstrap is used.")
    NULL
  } else {
    bs_theme
  }
}

include_parent_datanames <- function(dataname, join_keys) {
  parents <- character(0)
  for (i in dataname) {
    while (length(i) > 0) {
      parent_i <- join_keys$get_parent(i)
      parents <- c(parent_i, parents)
      i <- parent_i
    }
  }

  return(unique(c(parents, dataname)))
}



#' Create a `FilteredData`
#'
#' Create a `FilteredData` object from a `teal_data` object
#' @param x (`teal_data`) object
#' @return (`FilteredData`) object
#' @keywords internal
teal_data_to_filtered_data <- function(x) { #     nolint
  checkmate::assert_class(x, "teal_data")
  datanames <- x@datanames

  teal.slice::init_filtered_data(
    x = as.list(x@env)[datanames],
    join_keys = x@join_keys,
    code = teal.data:::CodeClass$new(
      code = paste(teal.code::get_code(x), collapse = "\n"),
      dataname = teal.data::get_dataname(x)
    ),
    check = FALSE
  )
}

#' Template Function for `TealReportCard` Creation and Customization
#'
#' This function generates a report card with a title,
#' an optional description, and the option to append the filter state list.
#'
#' @param title (`character(1)`) title of the card (unless overwritten by label)
#' @param label (`character(1)`) label provided by the user when adding the card
#' @param description (`character(1)`) optional additional description
#' @param with_filter (`logical(1)`) flag indicating to add filter state
#' @param filter_panel_api (`FilterPanelAPI`) object with API that allows the generation
#' of the filter state in the report
#'
#' @return (`TealReportCard`) populated with a title, description and filter state
#'
#' @export
report_card_template <- function(title, label, description = NULL, with_filter, filter_panel_api) {
  checkmate::assert_string(title)
  checkmate::assert_string(label)
  checkmate::assert_string(description, null.ok = TRUE)
  checkmate::assert_flag(with_filter)
  checkmate::assert_class(filter_panel_api, classes = "FilterPanelAPI")

  card <- teal::TealReportCard$new()
  title <- if (label == "") title else label
  card$set_name(title)
  card$append_text(title, "header2")
  if (!is.null(description)) card$append_text(description, "header3")
  if (with_filter) card$append_fs(filter_panel_api$get_filter_state())
  card
}
#' Resolve the datanames for the modules
#'
#' Modifies `module$datanames` to include parent datanames (taken from join_keys).
#' When `datanames` is set to `"all"` it is replaced with all available datanames.
#' @param modules (`teal_modules`) object
#' @param datanames (`character`) datanames available in the `data` object
#' @param join_keys (`JoinKeys`) object
#' @retun `teal_modules` with resolved datanames
#' @keywords internal
resolve_modules_datanames <- function(modules, datanames, join_keys) {
  if (inherits(modules, "teal_modules")) {
    modules$children <- sapply(
      modules$children,
      resolve_modules_datanames,
      simplify = FALSE,
      datanames = datanames,
      join_keys = join_keys
    )
    modules
  } else {
    modules$datanames <- if (identical(modules$datanames, "all")) {
      datanames
    } else if (is.character(modules$datanames)) {
      extra_datanames <- setdiff(modules$datanames, datanames)
      if (length(extra_datanames)) {
        stop(
          sprintf(
            "Module %s has datanames that are not available in a 'data':\n %s not in %s",
            modules$label,
            toString(extra_datanames),
            toString(datanames)
          )
        )
      }
      datanames_adjusted <- intersect(modules$datanames, datanames)
      include_parent_datanames(dataname = datanames_adjusted, join_keys = join_keys)
    }
    modules
  }
}
