#' Create the Server and Ui Function for the Shiny App
#'
#' Creates the server and ui part for a teal shiny app
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
#'   filter = list(ADSL = c("AGE")),
#'   header = tags$h1("Sample App"),
#'   footer = tags$p("Copyright 2017 - 2020")
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
init <- function(data,
                 modules,
                 initial_filter_states = list(),
                 header = tags$p("title here"),
                 footer = tags$p("footer here")
                 ) {
  if (!modules_depth(modules) %in% c(1, 2)) {
    # we don't support more nesting for clarity in the app
    stop("teal currently only supports module nesting of depth one or two.")
  }

  if (is_character_single(header)) {
    header <- tags$h1(header)
  }
  if (is_character_single(footer)) {
    footer <- tags$p(footer)
  }
  stopifnot(
    inherits(header, "shiny.tag"),
    inherits(footer, "shiny.tag"),
    is(data, "cdisc_data") || is(data, "DataConnector")
  )
  stopifnot(all(names(initial_filter_states) %in% names(data)))

  # once data is loaded, we will remove this element and add the real teal UI instead
  startapp_id <- "startapp_screen"
  startapp_selector <- paste0("#", startapp_id)

  skip_start_screen <- is(data, "cdisc_data")
  # Startup screen for delayed loading
  # We use delayed loading in all cases, even when the data does not need to be fetched.
  # This has the benefit that when filtering the data takes a lot of time initially, the
  # Shiny app does not time out.
  main_ui <- if (skip_start_screen) {
    div(id = startapp_id, div(
      h1("Hello, the teal app is starting up."),
      p("Here is a joke in the meantime:"),
      tags$b(get_random_joke())
    ))
  } else {
    message("App was initialized with delayed data loading.")
    div(id = startapp_id, data$get_ui("startapp_module"))
  }

  # show busy icon when shiny session is busy computing stuff
  # based on https://stackoverflow.com/questions/17325521/r-shiny-display-loading-message-while-function-is-running/22475216#22475216 #nolint
  shiny_busy_message_panel <- tagList(
    tags$style(
      type = "text/css",
      "#shinybusymessage {
          position: fixed;
          bottom: 0px;
          right: 0px;
          width: 140px;
          margin: 15px;
          padding: 5px 0px 5px 10px;
          text-align: left;
          font-weight: bold;
          font-size: 100%;
          color: #ffffff;
          background-color: #347ab7;
          z-index: 105;
      }"
    ),
    conditionalPanel(
      condition = "(($('html').hasClass('shiny-busy')) && (document.getElementById(\"shiny-notification-panel\") == null))", #nolint
      div(
        icon("sync", "spin fa-spin"),
        "Computing ...",
        id = "shinybusymessage"
      )
    )
  )

  # ui function
  # must be a function of request for bookmarking
  ui <- function(request) shinyUI(
    fluidPage(
      include_teal_css_js(),
      tags$header(header),
      tags$hr(style = "margin: 7px 0;"),
      shiny_busy_message_panel,
      main_ui,
      tags$hr(),
      tags$footer(footer)
    )
  )

  # server function
  server <- function(input, output, session) {
    # Javascript code to make the clipboard accessible
    run_js_files(files = "init.js")

    # the call to modules_with_filters_ui creates inputs that watch the tabs prefixed by teal_modules
    # we observe them and react whenever a tab is clicked by:
    # - displaying only the relevant datasets in the right hand filter in the
    # sections: filter info, filtering vars per dataname and add filter var per dataname
    call_filter_modules <- function(datasets) {
      # recursively goes down tabs to figure out the active module
      figure_out_active_module <- function(modules, idprefix) {
        id <- label_to_id(modules$label, idprefix)
        return(switch(
          class(modules)[[1]],
          teal_modules = {
            # id is the id of the tabset, the corresponding input element states which tab is selected
            active_submodule_label <- input[[id]]
            stopifnot(!is.null(active_submodule_label))
            figure_out_active_module(modules$children[[active_submodule_label]], idprefix = id)
          },
          teal_module = {
            stopifnot(is.null(input[[id]])) # id should not exist
            modules
          },
          stop("unknown module class ", class(modules))
        ))
      }

      active_datanames <- reactive_on_changes(reactive({
        # inputs may be NULL when UI hasn't loaded yet, but this expression still triggered
        req(!is.null(input[[label_to_id(modules$label, idprefix = "teal_modules")]]))

        active_datanames <- figure_out_active_module(modules, idprefix = "teal_modules")$filter
        if (identical(active_datanames, "all")) {
          active_datanames <- datasets$datanames()
        }
        # always add ADSL because the other datasets are filtered based on ADSL
        active_datanames <- union("ADSL", active_datanames)
        return(make_adsl_first(active_datanames))
      }))$value

      callModule(filter_panel_srv, "filter_panel", datasets, active_datanames)
    }

    # Get data through delayed loading ----

    # we construct a new dataset for each session
    # otherwise, tabs are not independent across users
    datasets <- FilteredData$new()
    # raw_data contains cdisc_data(), i.e. list of unfiltered data frames
    if (skip_start_screen) {
      raw_data <- reactiveVal(data) # will trigger by setting it
    } else {
      .log("fetching the data through delayed loading - showing start screen")
      raw_data <- callModule(data$get_server(), "startapp_module")
      stop_if_not(list(is.reactive(raw_data), "first app module has to return reactive object"))
      # trick for faster testing to avoid waiting on module specific to delayed data
      # raw_data <- reactive(cdisc_data_global) # nolintr
    }

    # Shiny bookmarking ----

    # The Shiny bookmarking functionality by default only stores inputs.
    # We need to add FilteredData to the state so we restore it as well.
    # To test bookmarking, include the `bookmark_module`, click on the bookmark
    # button and then get the link. Keep the Shiny app running and open the
    # obtained link in another browser tab.
    onBookmark(function(state) {
      # this function is isolated  by Shiny
      # We store the entire R6 class with reactive values in it, but set the data to NULL.
      # Note that we cannnot directly do this on datasets as this would trigger
      # reactivity to recompute the filtered datasets, which is not needed.
      state$values$datasets_state <- datasets$get_bookmark_state()
    })
    saved_datasets_state <- NULL # set when restored because data must already be populated
    onRestore(function(state) {
      # The saved datasets mainly contains the filter states as the data
      # was set to NULL before storing. The data should have been set again
      # by the user, so we just need to set the filters.
      saved_datasets_state <<- state$values$datasets_state
    })

    # Replace splash/welcome screen once data is loaded ----

    # ignoreNULL to not trigger at the beginning and just handle it once because data
    # obtained through delayed loading should usually not change afterwards
    # overwrite filter state if restored from bookmarked state
    observeEvent(raw_data(), ignoreNULL = TRUE, once = TRUE, {
      .log("data loaded successfully")
      data <- raw_data()

      set_datasets_data(datasets, data)
      set_datasets_filters(datasets, initial_filter_states)

      if (!is.null(saved_datasets_state)) {
        # actual thing to restore
        # cannot call this directly in onRestore because the data is not set at that time
        # for example, the data may only be loaded once a password is provided
        # however, onRestore only runs in the first flush and not in the flush when the
        # password was finally provided
        .log("restoring filter state from bookmarked state")
        tryCatch(
          datasets$set_from_bookmark_state(saved_datasets_state),
          error = function(cnd) {
            showModal(modalDialog(
              div(
                p("Could not restore the session: "),
                tags$pre(id = "error_msg", cnd$message),
              ),
              title = "Error restoring the bookmarked state",
              footer = tagList(
                actionButton("copy_code", "Copy to Clipboard", `data-clipboard-target` = "#error_msg"),
                modalButton("Dismiss")
              ),
              size = "l", easyClose = TRUE
            ))
          }
        )
      }

      # call server functions for teal modules and filter panel
      .log("initialize modules and filter panel")
      call_teal_modules(modules, datasets, idprefix = "teal_modules")
      # must make sure that this is only executed once as modules assume their observers are only
      # registered once (calling server functions twice would trigger observers twice each time)
      call_filter_modules(datasets)

      ui_teal_main <- modules_with_filters_ui(modules, datasets)
      insertUI(selector = startapp_selector, where = "afterEnd", ui = ui_teal_main)
      removeUI(startapp_selector)
    })
  }

  return(list(server = server, ui = ui))
}

# only react when the value of the expression changes and not each time
# the expression triggers (without always changing its value)
# we return the observer so you can cancel it when your module is dynamic
# expr must be a function, e.g. can be reactive
reactive_on_changes <- function(expr) {
  stopifnot(is.function(expr))

  rv <- reactiveVal()
  obs <- observe({
    rv(expr()) # only triggers rv on value updates
  })
  return(list(value = rv, observer = obs))
}
