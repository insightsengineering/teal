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
#' @param filter (\code{list}) You can pre-define filters for
#'   datasets that show up when the app starts up with the default filtering
#'   state.
#'   For instance, for filtering the dataset \code{ADSL},
#'   use \code{list(init = list(ADSL = ...))}.
#'   For each dataset you specify, you need to provide a vector which is a subset of
#'   the column names of the dataset. You can specify an ADSL filtering for the
#'   columns \code{SEX} and \code{BAGE} by:
#'
#'   \code{filter = list(init = list(ADSL = c("SEX", "BAGE")))}
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
#'           hist(datasets$get_data("ADSL", filtered = TRUE, reactive = TRUE)$AGE)
#'         )
#'       },
#'       ui = function(id, ...) {
#'         ns <- NS(id)
#'         plotOutput(ns("hist"))
#'       },
#'       filters = "ADSL"
#'     )
#'   ),
#'   filter = list(init = list(ADSL = c("AGE"))),
#'   header = tags$h1("Sample App"),
#'   footer = tags$p("Copyright 2017 - 2020")
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
init <- function(data,
                 modules,
                 filter = list(init = list()), # todo2: remove outer list
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

  skip_start_screen <- is(data, "cdisc_data")

  datasets <- FilteredData$new()

  # todo1: document why startapp_id is needed
  startapp_id <- paste0("startapp_", paste0(sample(1:10, 10, replace = TRUE), collapse = ""))
  startapp_selector <- paste0("#", startapp_id)

  # define main UI that contains all functionality of Teal
  main_ui <- if (skip_start_screen) {
    set_datasets_data(datasets, data)
    set_datasets_default_filter(datasets, vars_per_dataset = filter$init)
    modules_with_filters_ui(modules, datasets)
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
      shinyjs::useShinyjs(),
      include_css_files(package = "teal"),
      include_js_files(package = "teal", except = "init.js"),
      shinyjs::hidden(icon("cog")), # add hidden icon to load font-awesome css for icons
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
    # todo1: what is this javascript code doing
    run_js_files(files = "init.js", package = "teal")

    # todo: remove
    # options(shiny.suppressMissingContextError = TRUE)
    # on.exit(options(shiny.suppressMissingContextError = FALSE), add = TRUE)

    figure_out_active_module <- function(modules, idprefix) {
      id <- label_to_id(modules$label, idprefix)
      return(switch(
        class(modules)[[1]],
        teal_modules = {
          active_submodule_label <- input[[id]]
          figure_out_active_module(modules$children[[active_submodule_label]], idprefix = id)
        },
        teal_module = {
          modules
        },
        stop("unknown module class ", class(modules))
      ))
    }

    # the call to modules_with_filters_ui creates inputs that watch the tabs prefixed by teal_modules
    # we observe them and react whenever a tab is clicked by:
    # - displaying only the relevant datasets in the right hand filter in the
    # sections: filter info, filtering vars per dataname and add filter var per dataname
    call_filter_modules <- function(datasets) {
      active_datanames <- reactive_on_changes(reactive({
        active_datanames <- figure_out_active_module(modules, idprefix = "teal_modules")$filter
        if (identical(active_datanames, "all")) {
          active_datanames <- datasets$datanames()
        }
        # always add ADSL because the other datasets are filtered based on ADSL
        active_datanames <- union("ADSL", active_datanames)
        return(make_adsl_first(active_datanames))
      }))$value
      # when the set of active datasets changes, we update the right filter panel
      callModule(srv_filter_info, "teal_filters_info", datasets, datanames = reactive(active_datanames()))

      # rather than regenerating the UI dynamically for the dataset filtering,
      # we instead choose to hide/show the elements
      # the filters are just hidden from the UI, but still applied
      # use isolate because we assume that the number of datasets does not change over the course of the teal app
      # then hide/show relevant UI parts, see below
      datanames <- make_adsl_first(isolate(datasets$datanames()))
      # should not use for loop as variables are otherwise only bound by reference and last dataname would be used
      lapply(
        datanames,
        function(dataname) callModule(srv_filter_items, paste0("teal_filters_", dataname), datasets, dataname)
      )

      setBookmarkExclude(names = c(
        lapply(datanames, function(dataname) paste0("teal_filters_", dataname)),
        lapply(datanames, function(dataname) paste0("teal_add_", dataname, "_filter"))
      ))

      lapply(
        datanames,
        function(dataname) callModule(
          srv_add_filter_variable, paste0("teal_add_", dataname, "_filter"),
          datasets, dataname,
          omit_vars = reactive(if (dataname == "ADSL") character(0) else names(datasets$get_data("ADSL")))
        )
      )

      observeEvent(active_datanames(), {
        if (is.null(active_datanames())) {
          shinyjs::hide("teal_filter-panel")
        } else {
          shinyjs::show("teal_filter-panel")

          lapply(
            datasets$datanames(),
            function(dataname) {
              id_add_filter <- paste0("teal_add_", dataname, "_filter")
              id_filter_dataname <- paste0("teal_filters_", dataname)
              if (dataname %in% active_datanames()) {
                shinyjs::show(id_add_filter)
                shinyjs::show(id_filter_dataname)
              } else {
                shinyjs::hide(id_add_filter)
                shinyjs::hide(id_filter_dataname)
              }
            }
          )
        }
      })
    }

    # only inputs are stored and Shiny app is restored based on inputs
    # we need to add FilteredData to the state so we restore it as well
    # to test bookmarking, include the bookmarking module, click on the bookmark
    # button and then get the link. Keep the Shiny app running and open the
    # obtained link in another browser tab.
    # todo: lifecycle policy for bookmarked apps: when is the state deleted?
    onBookmark(function(state) {
      # store entire R6 class with reactive values in it
      state$values$datasets <- datasets
    })
    onRestore(function(state) {
      datasets$restore_from(state$values$datasets)
    })

    # todo: refactor this part
    if (skip_start_screen) {

      .log("init server - no start screen: initialize modules and filter panel")
      call_teal_modules(modules, datasets, idprefix = "teal_modules")
      call_filter_modules(datasets)

    } else {
      # todo: check this part
      stop("Currently not working")

      .log("init server - start screen: load screen")
      startapp_data <- callModule(data$get_server(), "startapp_module")
      stop_if_not(list(is.reactive(startapp_data), "first app module has to return reactive object"))

      # we need to run filter sub-modules after gui refreshes due to insertUI/removeUI
      # we are about to update GUI elements which are not visible in the initial screen
      # make use of invalidateLater to wait for a refresh and then proceed
      session$userData$has_initialized <- FALSE
      obs_filter_refresh <- observe({
        if (!session$userData$has_initialized) {
          session$userData$has_initialized <- TRUE
          invalidateLater(1) # reexecute this observe to fall in the else statement
        } else {
          .log("init server - start screen: initialize filter panel")
          obs_filter_panel$resume()
          call_filter_modules(datasets)
        }
      }, suspended = TRUE)

      ## now show or hide the filter panels based on active tab
      observeEvent(startapp_data(), {
        .log("init server - start screen: receive data from startup screen")

        set_datasets_data(datasets, startapp_data())
        set_datasets_default_filter(datasets, vars_per_dataset = filter$init)
        ui_teal_main <- modules_with_filters_ui(modules, datasets)

        insertUI(selector = startapp_selector, where = "afterEnd", ui = ui_teal_main)
        removeUI(startapp_selector)

        # evaluate the server functions
        call_teal_modules(modules, datasets, idprefix = "teal_modules")

        obs_filter_refresh$resume() # now update filter panel

      }, ignoreNULL = TRUE)

    }
  }

  return(list(server = server, ui = ui, datasets = datasets))
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
