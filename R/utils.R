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

