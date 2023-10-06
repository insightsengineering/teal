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
