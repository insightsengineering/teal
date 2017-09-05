

#' Create the Server and Ui Function for the Shiny App
#'
#' Creates the server and ui part for a teal shiny app
#'
#' @param data named list with datasets. Dataset names are case sensitive. The
#'   `ASL` data is mandatory.
#' @param analysis nested list with one list per analysis item with the
#'   following named list elements: \tabular{ll}{ name \tab string with name
#'   shown in menu for the analysis item \cr server \tab required, shiny server
#'   module function, see \code{\link[shiny]{callModule}} for more
#'   information\cr ui \tab required, shiny ui module function, see
#'   \code{\link[shiny]{callModule}} for more information\cr data \tab required,
#'   vector with datasets names that are passed on (filtered) to the server
#'   function\cr options \tab optional, other arguments passed on to the server
#'   function }
#' @param elements list with lists defining new pages (as for analysis) or one
#'   of the keywords \code{data_table}, \code{variable_browser},
#'   \code{analysis}.
#' @param filter filter settings. Nested named list, currently with \code{init}
#'   list element.
#' @param header object of class `shiny.tag` to be used as the header of the app
#' @param footer object of class `shiny.tag` to be used as the footer of the app
#'
#'
#'
#' @export
#'
#' @return named list with server and ui function
#'
#' @examples
#' \dontrun{
#' ASL <- generate_sample_data('ASL')
#' ARS <- generate_sample_data('ARS')
#' ATE <- generate_sample_data('ATE')
#'
#' x <- teal::init(
#'   data =  list(ASL = ASL, ARS = ARS, ATE = ATE),
#'   modules = root_modules(
#'     module(
#'       "data source",
#'       server = function(input, output, session, datasets) {},
#'       ui = function(id) div(p("data source")),
#'       filters = NULL,
#'       server_args = list(datasets = "teal_datasets")
#'     ),
#'     tm_data_table(),
#'     tm_variable_browser(),
#'     modules(
#'       label = "analysis items",
#'       module(
#'         label = "spaghetti plot",
#'         server = function(input, output, session, datasets) {},
#'         ui = function(id) div(p("spaghetti plot")),
#'         filters = 'ARS',
#'         server_args = list(datasets = "teal_datasets")
#'       ),
#'       module(
#'        label = "survival curves",
#'        server = function(input, output, session) {},
#'        ui = function(id) div(p("Kaplan Meier Curve")),
#'        filters = "ATE"
#'       )
#'     )
#'   ),
#'   header = tags$h1("Sample App"),
#'   footer = tags$p("Copyright 2017")
#' )
#'
#' shinyApp(x$ui, x$server)
#'
#' }
init <- function(data,
                 modules,
                 filter = NULL,
                 header = tags$p("title here"),
                 footer = tags$p("footer here")) {


  # initialize FilteredData object
  datasets <- FilteredData$new(names(data))

  Map(function(x, name) {
    datasets$set_data(name, x)
  }, data, names(data))

  # set default init filters
  if (!is.null(filter) && !is.null(filter$init)) {
    Map(function(vars, dataset) {
      lapply(vars, function(var) datasets$set_default_filter_state(dataset, var))
    }, filter$init, names(filter$init))
  }

  # ui function
  ui <- shinyUI(
      fluidPage(
        tags$head(
          tags$script(
            # show/hide see https://groups.google.com/forum/#!topic/shiny-discuss/yxFuGgDOIuM
            HTML("
                  Shiny.addCustomMessageHandler('tealShowHide', function(message) {
                        var els = $(message.selector);
                        if (message.action == 'show') {
                          els.trigger('show');
                          els.removeClass('hide');
                          els.trigger('shown');
                        } else {
                          els.trigger('hide');
                          els.addClass('hide');
                          els.trigger('hidden');
                        }
                  });"
            )
          )
        ),
        tags$header(header),
        tags$hr(style="margin: 7px 0;"),
        local({
          tp <- create_ui(modules, datasets, idprefix="teal_modules", is_root = TRUE)

          # separate the nested tabs
          tp$children <- list(
            tp$children[[1]],
            tags$hr(style="margin: 7px 0;"),
            fluidRow(
              column(9, tp$children[[2]]),
              column(3, div(id="teal_filter-panel", class="hide",
                            div(class="well",
                                tags$label("Active Filter Variables", class="text-primary", style="margin-bottom: 15px;"),
                                tagList(
                                  lapply(datasets$datanames(), function(dataname) {
                                    ui_filter_items(paste0("teal_filters_", dataname), dataname)
                                  })
                                )
                            ),
                            div(class="well",
                                tags$label("Add Filter Variables", class="text-primary", style="margin-bottom: 15px;"),
                                tagList(
                                  lapply(datasets$datanames(), function(dataname) {
                                    ui_add_filter_variable(paste0("teal_add_", dataname, "_filters"), dataname)
                                  })
                                )
                            )
              ))
            )
          )
          tp
        }),
        tags$hr(),
        tags$footer(footer)
      )
  )


  server <- function(input, output, session) {

    showFilterPanel <- function(bool=TRUE) {
      session$sendCustomMessage(type="setDisplayCss", list(selector="#teal_filter_panel", type=if(bool)'block'else'none'))
    }

    # evaluate the server functions
    call_modules(modules, datasets, idprefix="teal_modules")
#
#    cat("AAAAA\n")
#
#    # -- filters
#    lapply(datasets$datanames(), function(dataname) {
#      callModule(srv_filter_items, paste0("teal_filters_", dataname), datasets, dataname)
#    })
#
#    asl_vars <- names(datasets$get_data('ASL'))
#    lapply(datasets$datanames(), function(dataname) {
#      callModule(srv_add_filter_variable, paste0("teal_add_", dataname, "_filters"), datasets, dataname,
#                 omit_vars = if (dataname == "ASL") NULL else asl_vars)
#    })
#
#
#
#    ## hide-show filters based on tab-item filter property
#    tabs_ids <- unlist(Map(function(x) {
#
#      setNames(label_to_id(x$label, main_nav_id), x$label)
#    } , Filter(function(x) is(x, "teal_module"), modules)))
#
#    ## create a lookup list
#    filter_info <- list()
#
#    add_filter <- function(x, prefix=main_nav_id) {
#      if (is(x, "teal_tabs")) {
#        lapply(x, add_filter, prefix = prefix)
#      } else if (is(x, "teal_tab_item")) {
#        filter_info[[label_to_id(x$label, prefix)]] <<- x$filters
#      } else if (is(x, "teal_tabs_item")) {
#        .log(x$label)
#        add_filter(x$tabs, label_to_id(x$label, prefix))
#      } else {
#        stop("should not happen")
#      }
#    }
#    add_filter(tabs)
#
#
#
#
#    observe({
#      main_tab <- input[[main_nav_id]]
#      tabs_items <- sapply(tabs_ids, function(id) input[[id]],  USE.NAMES = TRUE)
#
#      main_tab_id <- label_to_id(main_tab, main_nav_id)
#      sub_tab_id <- label_to_id(tabs_items[main_tab], main_tab_id)
#
#      filters <- if (is.null(filter_info[[sub_tab_id]])) {
#        .log("main", main_tab_id, "filters:", filter_info[[main_tab_id]])
#        filter_info[[main_tab_id]]
#      }  else {
#        .log("subtab",  sub_tab_id, "filters:", filter_info[[sub_tab_id]])
#        filter_info[[sub_tab_id]]
#      }
#
#      if (is.null(filters)) {
#        session$sendCustomMessage(type="tealShowHide", list(selector = "#teal_filter-panel", action = "hide"))
#      } else {
#        as.global(session)
#        session$sendCustomMessage(type="tealShowHide", list(selector = "#teal_filter-panel", action = "show"))
#
#        if ("all" %in% filters) {
#          lapply(datasets$datanames(), function(dataname) {
#            session$sendCustomMessage(type="tealShowHide", list(selector = paste0(".teal_filter_",dataname),
#                                                                   action="show"))
#          })
#        } else {
#          Map(function(dataname) {
#            session$sendCustomMessage(
#              type="tealShowHide",
#              list(selector = paste0(".teal_filter_",dataname),
#                   action = if (dataname == "ASL" || dataname %in% filters) "show" else "hide"
#              )
#            )
#          },  datasets$datanames())
#        }
#      }
#
#
#    })

    # --


  }

  list(server = server, ui = ui, datasets = datasets)
}

