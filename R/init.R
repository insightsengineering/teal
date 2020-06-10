# This is the main function from teal to be used by the end-users.


#' Create the Server and UI Function For the Shiny App
#'
#' End-users: This is the most important function for you to start a
#' teal app that is composed out of teal modules.
#'
#' **Notes for developers**:
#' This is a wrapper function around the `module_teal.R` functions.
#' It handles both delayed and non-delayed data and provides a default
#' splash screen for the latter.
#'
#' @md
#' @param data (`cdisc_data` or `DataConnector`)
#'   For `cdisc_data`: named list with datasets. Dataset names are case sensitive. The
#'   `ADSL` data is mandatory.
#' @param modules nested list with one list per module with the
#'   following named list elements:
#'   \tabular{ll}{
#'   \cr name \tab string with name shown in menu for the analysis item
#'   \cr server \tab required, shiny server module function, see
#'   `\link[shiny]{callModule}` for more information
#'   \cr ui \tab required, shiny ui module function, see
#'   `\link[shiny]{callModule}` for more information
#'   \cr data \tab required, vector with datasets names that are passed
#'   on (filtered) to the server function
#'   \cr options \tab optional, other arguments passed on to the server
#'   function
#'   }
#' @param initial_filter_states (`list`) You can define filters that show when
#'   the app starts.
#'   Pass in a named list to overwrite filters, e.g.
#'   `list(ADSL = list(SEX = NULL))`
#'   to have the SEX filter appear with nothing selected (i.e. 0 patients)
#'   `list(ADSL = list(SEX = list(choices = "M", keep_na = TRUE)))`
#'   to keep patients that are male or have unknown SEX.
#'   `list(ADSL = list(SEX = "default"))`
#'   to have the default filter that appears also when you select to add this
#'   filtering variable in the running app.
#'   A general example is:
#'   `list(
#'   ADSL = list(AGE = "default", SEX = list(choices = "M", keep_na = TRUE)),
#'   ADAE = list(AETOXGR = "default")
#'   )`
#'   Note that if the app is restored from a bookmarked state, the filters
#'   are overwritten.
#' @param header (`character` or `shiny.tag`) the header of the app
#' @param footer (`character` or `shiny.tag`) the footer of the app
#' @param id `character` Shiny module id, set it if you want to add this into
#'   another Shiny module
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
                 header = tags$p("Add Title Here"),
                 footer = tags$p("Add Footer Here"),
                 id = character(0)) {

  # currently not a Shiny module, but top-level app (module at top-level which cannot have any parents)
  # todo: make a module out of this as well
  stopifnot(
    is(data, "cdisc_data") || is(data, "DataConnector"),
    is(modules, "teal_modules"),
    all(names(initial_filter_states) %in% names(data)),
    is_character_single(id) || is_character_empty(id)
  )

  ns <- NS(id)

  is_not_delayed_data <- !is(data, "delayed_data") # `cdisc_data` or `delayed_data`
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

  # rather than using callModule and creating a submodule of this module, we directly modify
  # the ui and server, this can be achieved by passing it the same id as this module, i.e.
  # `ns(character(0))` and calling the server function directly rather than through `callModule`
  return(list(
    ui = ui_teal(id = ns(character(0)), splash_ui = splash_ui, header = header, footer = footer),
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

      srv_teal(
        input, output, session,
        modules = modules, raw_data = raw_data, initial_filter_states = initial_filter_states
      )
    }
  ))
}

# todo: example
#' Make a UI function bookmarkable
#'
#' To be bookmarkable, the Shiny UI function must have an
#' argument `request`. This function ensures this.
#'
#' @md
#' @param ui `function or shiny.tag` Shiny UI; either a
#'   `shiny.tag` or a function with no argument or
#'   one argument (`request`)
#' @return `function` Shiny UI function with one argument `request`
make_bookmarkable <- function(ui) {
  # we use the same logic as in `shiny:::uiHttpHandler`
  if (is.function(ui)) {
    if (length(formals(ui)) > 0) {
      stopifnot(length(formals(ui)) == 1)
      ui
    } else {
      function(request) ui()
    }
  } else {
    stopifnot(inherits(ui, "shiny.tag"))
    function(request) ui
  }
}
