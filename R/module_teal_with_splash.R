# This file adds a splash screen for delayed data loading on top of teal

#' UI to show a splash screen in the beginning, then delegate to [`srv_teal`]
#'
#' @description `r lifecycle::badge("maturing")`
#' The splash screen could be used to query for a password to fetch the data.
#' [`init`] is a very thin wrapper around this module useful for end-users which
#' assumes that it is a top-level module and cannot be embedded.
#' This function instead adheres to the Shiny module conventions.
#'
#' If data is obtained through delayed loading, its splash screen is used. Otherwise,
#' a default splash screen is shown.
#'
#' Please also refer to the doc of [`init`].
#'
#' @param id (`character` value)\cr
#'   module id
#' @param data (`TealDataAbstract`)\cr
#'   object containing data
#' @param title (`NULL` or `character`) The browser window title (defaults to the host URL of the page).
#' @param header (`character` or `shiny.tag`) the header of the app
#' @param footer (`character` or `shiny.tag`) the footer of the app
#' @export
ui_teal_with_splash <- function(id,
                                data,
                                title,
                                header = tags$p("Add Title Here"),
                                footer = tags$p("Add Footer Here")) {
  stopifnot(is(data, "TealDataAbstract"))
  is_pulled_data <- is_pulled(data)
  ns <- NS(id)

  # Startup splash screen for delayed loading
  # We use delayed loading in all cases, even when the data does not need to be fetched.
  # This has the benefit that when filtering the data takes a lot of time initially, the
  # Shiny app does not time out.
  splash_ui <- if (is_pulled_data) {
    # blank ui if data is already pulled
    div()
  } else {
    message("App was initialized with delayed data loading.")
    data$get_ui(ns("startapp_module"))
  }

  ui_teal(id = ns("teal"), splash_ui = splash_ui, title = title, header = header, footer = footer)
}

#' Server function that loads the data through reactive loading and then delegates
#' to [`srv_teal`].
#'
#' @description `r lifecycle::badge("maturing")`
#' Please also refer to the doc of [`init`].
#'
#' @inheritParams srv_shiny_module_arguments
#' @param data `TealDataAbstract` R6 object and container for data
#' @inheritParams srv_teal
#' @return `reactive`, return value of [`srv_teal`]
#' @export
srv_teal_with_splash <- function(input, output, session, data, modules, filter = list()) {
  stopifnot(is(data, "TealDataAbstract"))

  is_pulled_data <- is_pulled(data)

  # raw_data contains `TealDataAbstract`, i.e. R6 object and container for data
  # reactive to get data through delayed loading
  # we must leave it inside the server because of callModule which needs to pick up the right session
  if (is_pulled_data) {
    raw_data <- reactiveVal(data) # will trigger by setting it
  } else {
    raw_data <- data$get_server()(id ="startapp_module")
    stop_if_not(list(is.reactive(raw_data), "The delayed loading module has to return a reactive object."))
  }

  res <- callModule(
    srv_teal, "teal", modules = modules, raw_data = raw_data, filter = filter
  )
  return(res)
}
