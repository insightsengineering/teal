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
  check_module_names(modules)

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
    teal_with_filters_ui(modules, datasets)
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
  ui <- shinyUI(
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

    # the call to teal_with_filters_ui creates inputs that watch the tabs prefixed by teal_modules
    # we observe them and react whenever a tab is clicked
    # browser()
    #
    # tab_ids_per_module

    # returns a flattened (if nested modules) list of datasets needed for each module
    # in addition, gives ids by appending id of module to prefix (also works with nested
    # modules)
    datasets_per_module <- function(modules, idprefix) {
      id <- label_to_id(modules$label, idprefix)

      if (is(modules, "teal_module")) {
        setNames(list(modules$filters), id)
      } else if (is(modules, "teal_modules")) {
        unlist(
          Map(function(i) datasets_per_module(i, idprefix = id), modules$children),
          recursive = FALSE
        )
      } else {
        stop("Unknown class for x: ", class(modules))
      }
    }

    # named vector with module ids and active datasets for that module
    filters_tab_lookup <- datasets_per_module(modules, "teal_modules")

    recurse_modules <- function(modules, idprefix) {
      id <- label_to_id(modules$label, idprefix)

      if (is(modules, "teal_modules")) {
        c(id, lapply(modules$children, recurse_modules, idprefix = id))
      } else {
        NULL
      }
    }
    id_modules <- unlist(recurse_modules(modules, "teal_modules"))

    #browser()
    #
    # # contains list of datanames to show for filter in currently selected tab
    # active_dataname_filters <- reactiveVal()

    obs_filter_panel <- observe({

      #browser()

      # define reactivity dependence
      main_tab <- input[["teal_modules_root"]]
      secondary_tabs <- sapply(id_modules, function(id) input[[id]], USE.NAMES = TRUE)

      # figure out which is the active tab/module
      main_tab_id <- label_to_id(main_tab, "teal_modules_root")

      active_module_id <- if (main_tab_id %in% id_modules) {
        label_to_id(secondary_tabs[[main_tab_id]], main_tab_id)
      } else {
        main_tab_id
      }

      filters <- filters_tab_lookup[[active_module_id]]
      .log("Active filters for tab", active_module_id, "are", if_null(filters, "[empty]"))
      if (is.null(filters)) {
        shinyjs::hide("teal_filter-panel")
      } else {
        shinyjs::show("teal_filter-panel")

        if (identical(filters, "all")) {
          lapply(datasets$datanames(), function(dataname) {
            shinyjs::show(paste0("teal_add_", dataname, "_filters"))
            shinyjs::show(paste0("teal_filters_", dataname))
          })
        } else {
          # todo: the filters are just hidden from the UI, but still applied
          lapply(
            datasets$datanames(),
            function(dataname) {
              id <- paste0("teal_add_", dataname, "_filters")
              if (dataname == "ADSL" || dataname %in% filters) {
                shinyjs::show(id)
                shinyjs::show(paste0("teal_filters_", dataname))
              } else {
                shinyjs::hide(id)
                shinyjs::hide(paste0("teal_filters_", dataname))
              }
            }
          )
        }
      }
    }, suspended = TRUE)

    # todo: this whole function (`teal::init`) should be rewritten to make it reactive
    # the thing below is not reactive on the datasets which can be modified through set_data
    call_filter_modules <- function(datasets) {
      # -- filters Modules
      callModule(srv_filter_info, "teal_filters_info", datasets)
      # use isolate because we assume that the number of datasets does not change over the course of the teal app
      # otherwise need dynamic UI, should be dynamic as well
      datanames <- make_adsl_first(isolate(datasets$datanames()))
      # should not use for loop as variables are otherwise only bound by reference
      lapply(
        datanames,
        function(dataname) callModule(srv_filter_items, paste0("teal_filters_", dataname), datasets, dataname)
      )

      adsl_vars <- reactive(names(datasets$get_data("ADSL")))
      lapply(
        datanames,
        function(dataname) callModule(
          srv_add_filter_variable, paste0("teal_add_", dataname, "_filters"),
          datasets, dataname,
          omit_vars = if (dataname == "ADSL") character(0) else adsl_vars()
        )
      )
    }

    if (skip_start_screen) {

      .log("init server - no start screen: initialize modules and filter panel")
      call_teal_modules(modules, datasets, idprefix = "teal_modules")
      obs_filter_panel$resume()
      call_filter_modules(datasets)

    } else {
      # todo: check this part

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
        ui_teal_main <- teal_with_filters_ui(modules, datasets)

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
