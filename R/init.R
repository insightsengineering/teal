#' Create the Server and UI Function For the Shiny App
#'
#' Creates the server and UI part of a Shiny app
#'
#' @param data (\code{cdisc_data} or \code{DataConnector})
#'   \code{cdisc_data}: named list with datasets. Dataset names are case sensitive. The
#'   `ADSL` data is mandatory.
#'
#' @param modules nested list with one list per module with the
#'   following named list elements: \tabular{ll}{ name \tab string with name
#'   shown in menu for the analysis item \cr server \tab required, shiny server
#'   module function, see \code{\link[shiny]{callModule}} for more
#'   information\cr ui \tab required, shiny ui module function, see
#'   \code{\link[shiny]{callModule}} for more information\cr data \tab required,
#'   vector with datasets names that are passed on (filtered) to the server
#'   function\cr options \tab optional, other arguments passed on to the server
#'   function }
#' @param initial_filter_states (\code{list}) You can define filters that show when
#'   the app starts.
#'   Pass in a named list to overwrite filters, e.g.
#'   \code{list(ADSL = list(SEX = NULL))}
#'   to have the SEX filter appear with nothing selected (i.e. 0 patients)
#'   \code{list(ADSL = list(SEX = list(choices = "M", keep_na = TRUE)))}
#'   to keep patients that are male or have unknown SEX.
#'   \code{list(ADSL = list(SEX = "default"))}
#'   to have the default filter that appears also when you select to add this
#'   filtering variable in the running app.
#'   A general example is:
#'   \code{list(
#'   ADSL = list(AGE = "default", SEX = list(choices = "M", keep_na = TRUE)),
#'   ADAE = list(AETOXGR = "default")
#'   )}
#'   Note that if the app is restored from a bookmarked state, the filters
#'   are overwritten.
#' @param header (\code{character} or object of class `shiny.tag`) the header of the app
#' @param footer (\code{character} or object of class `shiny.tag`) the footer of the app
#' @return named list with server and ui function
#'
#' @export
#'
#' @importFrom shinyjs useShinyjs hidden hide show
#' @importFrom methods is
#'
#' @include FilteredData.R
#' @include modules.R
#'
#' @examples
#' library(random.cdisc.data)
#'
#' ADSL <- radsl(cached = TRUE)
#'
#' options(teal_logging = FALSE)
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     code = "ADSL <- radsl(seed = 1)"
#'   ),
#'   modules = root_modules(
#'     module(
#'       "data source",
#'       server = function(input, output, session, datasets) {},
#'       ui = function(id, ...) div(p("information about data source")),
#'       filters = "all"
#'     ),
#'     module(
#'       "ADSL AGE histogram",
#'       server = function(input, output, session, datasets) {
#'         output$hist <- renderPlot(
#'           hist(datasets$get_data("ADSL", filtered = TRUE)$AGE)
#'         )
#'       },
#'       ui = function(id, ...) {
#'         ns <- NS(id)
#'         plotOutput(ns("hist"))
#'       },
#'       filters = "ADSL"
#'     )
#'   ),
#'   initial_filter_states = list(ADSL = list(AGE = "default")),
#'   header = tags$h1("Sample App"),
#'   footer = tags$p("Copyright 2017 - 2020")
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
init <- function(data,
                 modules,
                 initial_filter_states = list(),
                 header = tags$p("Title here"),
                 footer = tags$p("Footer here")) {

  # currently not a Shiny module, but top-level app (module at top-level which cannot have any parents)
  # todo: make a module out of this as well
  stopifnot(is(data, "cdisc_data") || is(data, "DataConnector"))
  stopifnot(
    is(data, "cdisc_data") || is(data, "DataConnector"),
    all(names(initial_filter_states) %in% names(data))
  )

  is_not_delayed_data <- !is(data, "delayed_data") # `cdisc_data` or `delayed_data`
  # Startup splash screen for delayed loading
  # We use delayed loading in all cases, even when the data does not need to be fetched.
  # This has the benefit that when filtering the data takes a lot of time initially, the
  # Shiny app does not time out.
  splash_ui <- if (is_not_delayed_data) {
    h1("The teal app is starting up.")
  } else {
    message("App was initialized with delayed data loading.")
    data$get_ui("startapp_module")
  }

  return(list(
    # character(0) will make it a top-level module # todo: test
    ui = ui_teal(id = character(0), splash_ui = splash_ui, header = header, footer = footer),
    server = function(input, output, session) {
      # raw_data contains cdisc_data(), i.e. list of unfiltered data frames
      # reactive to get data through delayed loading
      # we must leave it inside the server because of callModule which needs to pick up the right session
      if (is_not_delayed_data) {
        raw_data <- reactiveVal(data) # will trigger by setting it
      } else {
        .log("fetching the data through delayed loading - showing start screen")
        raw_data <- callModule(data$get_server(), "startapp_module")
        stop_if_not(list(is.reactive(raw_data), "The delayed loading module has to return a reactive object."))
        # trick for faster testing to avoid waiting on module specific to delayed data
        # raw_data <- reactive(cdisc_data_global) # nolintr
      }

      # callModule not needed here as it has no parents
      srv_teal(
        input, output, session,
        modules = modules, raw_data = raw_data, initial_filter_states = initial_filter_states
      )
    }
  ))
}


# only react when the value of the expression changes and not each time
# the expression triggers (without always changing its value)
# we return the observer so you can cancel it when your module is dynamic
# expr must be a function, e.g. can be reactive
# sodo3: into utils.nest?
reactive_on_changes <- function(expr) {
  stopifnot(is.function(expr))

  rv <- reactiveVal()
  obs <- observe({
    rv(expr()) # only triggers rv on value updates
  })
  return(list(value = rv, observer = obs))
}
