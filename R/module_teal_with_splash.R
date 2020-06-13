# This file adds a splash screen for delayed data loading on top of teal

#' UI to show a splash screen in the beginning, then delegate to `\link{srv_teal}`
#'
#' The splash screen could be used to query for a password to fetch the data.
#' `\link{init}` is a very thin wrapper around this module useful for end-users which
#' assumes that it is a top-level module and cannot be embedded.
#' This function instead adheres to the Shiny module conventions.
#'
#' If data is obtained through delayed loading, its splash screen is used. Otherwise,
#' a default splash screen is shown.
#'
#' Please also refer to the doc of `\link{init}`.
#'
#' @md
#' @param id module id
#' @param data `cdisc_data or DataConnector` object to obtain the data
#' @inheritParams ui_teal
#' @export
ui_teal_with_splash <- function(id, data, header = tags$p("Add Title Here"), footer = tags$p("Add Footer Here")) {
  stopifnot(is(data, "cdisc_data") || is(data, "DataConnector"))
  is_not_delayed_data <- is(data, "cdisc_data") # `cdisc_data` or `DataConnector`

  ns <- NS(id)

  # Startup splash screen for delayed loading
  # We use delayed loading in all cases, even when the data does not need to be fetched.
  # This has the benefit that when filtering the data takes a lot of time initially, the
  # Shiny app does not time out.
  splash_ui <- if (is_not_delayed_data) {
    h1("The teal app is starting up.")
  } else {
    message("App was initialized with delayed data loading.")
    data$get_ui(ns("startapp_module"))
  }

  ui_teal(id = ns("teal"), splash_ui = splash_ui, header = header, footer = footer)
}

#' Server function that loads the data through reactive loading and then delegates
#' to `\link{srv_teal}`.
#'
#' Please also refer to the doc of `\link{init}`.
#'
#' @md
#' @inheritParams srv_shiny_module_arguments
#' @param data `cdisc_data or DataConnector` object to obtain the data
#' @inheritParams srv_teal
#' @return `reactive`, return value of `\link{srv_teal}`
#' @export
srv_teal_with_splash <- function(input, output, session, data, modules, filter_states = list()) {
  stopifnot(
    is(data, "cdisc_data") || is(data, "DataConnector")
  )

  is_not_delayed_data <- is(data, "cdisc_data") # `cdisc_data` or `DataConnector`

  # raw_data contains cdisc_data(), i.e. list of unfiltered data frames
  # reactive to get data through delayed loading
  # we must leave it inside the server because of callModule which needs to pick up the right session
  if (is_not_delayed_data) {
    raw_data <- reactiveVal(data) # will trigger by setting it
  } else {
    .log("fetching the data through delayed loading - showing start screen")
    raw_data <- callModule(data$get_server(), "startapp_module")
    # for faster testing without fetching the data through delayed loading screen, but still testing the logic,
    # replace above line by this one
    # raw_data <- reactive(cdisc_data_global) # nolintr
    stop_if_not(list(is.reactive(raw_data), "The delayed loading module has to return a reactive object."))
  }
  return(callModule(srv_teal, "teal", modules = modules, raw_data = raw_data, filter_states = filter_states))
}
