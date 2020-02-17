#' Create the Server and Ui Function for the Shiny App
#'
#' Creates the server and ui part for a teal shiny app
#'
#' @param data named list with datasets. Dataset names are case sensitive. The
#'   `ADSL` data is mandatory.
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
#'   datasets inside this argument. Therefore you need to handover an
#'   \code{init} list. Please provide a named list inside \code{init}
#'   that contains the names of the datasets. E.g. for filtering
#'   the dataset \code{ADSL} use \code{list(init = list(ADSL = ...))}.
#'   For each datasets you need to provide a vector with column names that are
#'   relevant for the item. You can specify an ADSL filtering for the
#'   columns \code{SEX} and \code{BAGE} by:
#'
#'   \code{filter = list(init = list(ADSL = c("SEX", "BAGE")))}
#' @param header object of class `shiny.tag` to be used as the header of the app
#' @param footer object of class `shiny.tag` to be used as the footer of the app
#' @param start_screen (\code{shiny UI element}) that contains an
#'   \code{actionButton} with the \code{id} \code{start}. Anything else can
#'   be specified by the user. The element will be rendered before the
#'   start of the app.
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
#'     code = "
#'     ADSL <- radsl(seed = 1)
#'   "
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
#'   footer = tags$p("Copyright 2017")
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
init <- function(data,
                 modules,
                 filter = NULL,
                 header = tags$p("title here"),
                 footer = tags$p("footer here"),
                 start_screen = NULL
                 ) {
  if (modules_depth(modules) > 2) {
    stop("teal currently only supports module nesting of depth two.")
  }
  check_module_names(modules)

  stopifnot(inherits(header, "shiny.tag"))

  stopifnot(inherits(footer, "shiny.tag"))

  stopifnot(is.null(start_screen) || inherits(start_screen, "shiny.tag"))

  skip_start <- is.null(start_screen)
  if (!skip_start) {
    start_screen_char <- as.character(start_screen)

    if (!grepl("id=\"start\" type=\"button\"", start_screen_char)) {
      stop("Thes 'start screen' needs to contain an 'actionButton' with the ID 'start'")
    }
  }

  datasets <- NULL
  ui_internal <- start_screen

  if (skip_start) {
    datasets <- init_datasets(data, filter)
    ui_internal <- init_ui(datasets, modules)
  }


  # ui function
  ui <- shinyUI(
    fluidPage(
      shinyjs::useShinyjs(),
      include_css_files(package = "teal"),
      include_js_files(package = "teal", except = "init.js"),
      shinyjs::hidden(icon("cog")), # add hidden icon to load font-awesome css for icons
      tags$header(header),
      tags$hr(style = "margin: 7px 0;"),
      ui_internal,
      tags$hr(),
      tags$footer(footer)
    )
  )


  server <- function(input, output, session) {
    run_js_file(file = "init.js", package = "teal")

    ## hide-show filters based on module filter property
    recurse <- function(x, idprefix) {
      id <- label_to_id(x$label, idprefix)

      if (is(x, "teal_module")) {
        setNames(list(x$filters), id)
      } else if (is(x, "teal_modules")) {
        unlist(
          mapply(function(i) recurse(i, idprefix = id), x$modules, USE.NAMES = TRUE, SIMPLIFY = FALSE),
          recursive = FALSE
        )
      }
    }
    # named vector with ids and datasets
    filters_tab_lookup <- recurse(modules, "teal_modules")

    recurse_modules <- function(x, idprefix) {
      id <- label_to_id(x$label, idprefix)

      if (is(x, "teal_modules")) {
        c(id, lapply(x$modules, recurse_modules, idprefix = id))
      } else {
        NULL
      }
    }
    id_modules <- unlist(recurse_modules(modules, "teal_modules"))

    datasets_react <- reactiveVal(datasets)

    obs_filter_panel <- observe({
      # define reactivity dependence
      datasets <- datasets_react()
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
      .log("Active filter for tab", active_module_id, "is", if_null(filters, "[empty]"))

      if (is.null(filters)) {
        shinyjs::hide("teal_filter-panel")
      } else {
        shinyjs::show("teal_filter-panel")

        if ("all" %in% filters) {
          lapply(datasets$datanames(), function(dataname) {
            shinyjs::show(paste0("teal_filter_", dataname))
          })
        } else {
          lapply(
            datasets$datanames(),
            function(dataname) {
              id <- paste0("teal_filter_", dataname)
              if (dataname == "ADSL" || dataname %in% filters) {
                shinyjs::show(id)
              } else {
                shinyjs::hide(id)
              }
            }
          )
        }
      }
    }, suspended = TRUE)

    call_filter_modules <- function(datasets) {
      # -- filters Modules
      for (dataname in datasets$datanames()) {
        callModule(srv_filter_items, paste0("teal_filters_", dataname), datasets, dataname)
      }
      for (dataname in datasets$datanames()) {
        callModule(srv_filter_info, paste0("teal_filters_info_", dataname), datasets, dataname)
      }

      adsl_vars <- names(datasets$get_data("ADSL"))
      for (dataname in datasets$datanames()) {
        callModule(srv_add_filter_variable, paste0("teal_add_", dataname, "_filters"), datasets, dataname,
                   omit_vars = if (dataname == "ADSL") character(0) else adsl_vars
        )
      }
    }

    if (skip_start) {

      call_modules(modules, datasets, idprefix = "teal_modules")
      obs_filter_panel$resume()
      call_filter_modules(isolate(datasets_react()))

    } else {

      # we need to run filter sub-modules after gui refreshes due to insertUI/removeUI
      # we are about to update GUI elements which are not visible in the initial screen
      # make use of invalidateLater to wait for a refresh and then proceed
      filter_refresh <- reactiveVal(TRUE)
      obs_filter_refresh <- observe({
        refresh <- isolate(filter_refresh())
        if (refresh) {
          filter_refresh(FALSE)
          invalidateLater(1) # reexecute this observe to fall in the else statement
        } else {
          obs_filter_panel$resume()
          call_filter_modules(datasets_react())
        }
      }, suspended = TRUE)


      ## now show or hide the filter panels based on active tab
      observeEvent(input$start, {
        datasets <- init_datasets(data, filter)
        datasets_react(datasets)

        insertUI(selector = "#start", where = "afterEnd", ui = init_ui(datasets, modules))
        removeUI("#start")

        # evaluate the server functions
        call_modules(modules, datasets, idprefix = "teal_modules")

        obs_filter_refresh$resume()
      })

    }
  }

  list(server = server, ui = ui)
}
