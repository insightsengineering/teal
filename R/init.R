#' Create the Server and Ui Function for the Shiny App
#'
#' Creates the server and ui part for a teal shiny app
#'
#' @param data named list with datasets. Dataset names are case sensitive. The
#'   `ASL` data is mandatory.
#' @param modules nested list with one list per module with the
#'   following named list elements: \tabular{ll}{ name \tab string with name
#'   shown in menu for the analysis item \cr server \tab required, shiny server
#'   module function, see \code{\link[shiny]{callModule}} for more
#'   information\cr ui \tab required, shiny ui module function, see
#'   \code{\link[shiny]{callModule}} for more information\cr data \tab required,
#'   vector with datasets names that are passed on (filtered) to the server
#'   function\cr options \tab optional, other arguments passed on to the server
#'   function }
#' @param filter filter settings. Nested named list, currently with \code{init}
#'   list element.
#' @param header object of class `shiny.tag` to be used as the header of the app
#' @param footer object of class `shiny.tag` to be used as the footer of the app
#'
#' @return named list with server and ui function
#'
#' @export
#'
#' @import shiny methods stats
#'
#' @examples
#' \dontrun{
#' library(teal.modules.general)
#' library(random.cdisc.data)
#'
#' ASL <- radsl(seed = 1)
#' ARS <- radrs(ASL, seed = 100)
#' ATE <- radtte(ASL, seed = 1000)
#'
#' # for reproducibility
#' attr(ASL, "source") <- "random.cdisc.data::radsl(seed = 1)"
#' attr(ARS, "source") <- "random.cdisc.data::radrs(ASL, seed = 100)"
#' attr(ATE, "source") <- "random.cdisc.data::radtte(ASL, seed = 1000)"
#'
#' app <- teal::init(
#'   data = list(ASL = ASL, ARS = ARS, ATE = ATE),
#'   modules = root_modules(
#'     module(
#'       "data source",
#'       server = function(input, output, session, datasets) {},
#'       ui = function(id) div(p("information about data source")),
#'       filters = NULL
#'     ),
#'     tm_data_table(),
#'     tm_variable_browser(),
#'     modules(
#'       label = "analysis items",
#'       tm_table(
#'          label = "demographic table",
#'          dataname = "ASL",
#'          xvar = choices_selected("SEX"),
#'          yvar = choices_selected(c("RACE", "BMRKR2", "COUNTRY"), "RACE")
#'       ),
#'       tm_scatterplot(
#'          label = "scatterplot",
#'          dataname = "ASL",
#'          xvar = "AGE",
#'          yvar = "BMRKR1",
#'          color_by = "_none_",
#'          color_by_choices = c("_none_", "STUDYID")
#'       ),
#'       # ad-hoc module
#'       module(
#'          label = "survival curves",
#'          server = function(input, output, session, datasets) {},
#'          ui = function(id) div(p("Kaplan Meier Curve")),
#'          filters = "ATE"
#'       )
#'     )
#'   ),
#'   header = tags$h1("Sample App"),
#'   footer = tags$p("Copyright 2017")
#' )
#'
#' shinyApp(app$ui, app$server)
#'
#' }
init <- function(data,
                 modules,
                 filter = NULL,
                 header = tags$p("title here"),
                 footer = tags$p("footer here")) {

  if (modules_depth(modules) > 2) {
    stop("teal currently only supports module nesting of depth two.")
  }

  # initialize FilteredData object
  datasets <- FilteredData$new(names(data))
  Map(function(x, name) {
    datasets$set_data(name, x)
  }, data, names(data))

  # including attributes of data object
  datasets$set_data_attrs(data)

  # set default init filters
  if (!is.null(filter) && !is.null(filter$init)) {
    Map(function(vars, dataset) {
      lapply(vars, function(var) datasets$set_default_filter_state(dataset, var))
    }, filter$init, names(filter$init))
  }

  # ui function
  ui <- shinyUI(
      fluidPage(
        shinyjs::useShinyjs(),
        includeScript(system.file("js/clipboard.js", package = "teal")),
        includeScript(system.file("js/initClipboard.js", package = "teal")),
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
        tags$hr(style = "margin: 7px 0;"),
        local({
          tp <- create_ui(modules, datasets, idprefix = "teal_modules", is_root = TRUE)

          # separate the nested tabs
          tp$children <- list(
            tp$children[[1]],
            tags$hr(style = "margin: 7px 0;"),
            fluidRow(
              column(9, tp$children[[2]]),
              column(3, div(id = "teal_filter-panel", class = "hide",
                            div(class = "well",
                                tags$label(
                                  "Active Filter Variables",
                                  class = "text-primary",
                                  style = "margin-bottom: 15px;"
                                ),
                                tagList(
                                  lapply(datasets$datanames(), function(dataname) {
                                    ui_filter_items(paste0("teal_filters_", dataname), dataname)
                                  })
                                )
                            ),
                            div(class = "well",
                                tags$label(
                                  "Add Filter Variables",
                                  class = "text-primary",
                                  style = "margin-bottom: 15px;"
                                ),
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

    show_filter_panel <- function(bool = TRUE) {
      session$sendCustomMessage(
        type = "setDisplayCss",
        list(selector = "#teal_filter_panel",
             type = ifelse(bool, "block", "none")
        )
      )
    }

    # evaluate the server functions
    call_modules(modules, datasets, idprefix = "teal_modules")

    # -- filters
    lapply(datasets$datanames(), function(dataname) {
      callModule(srv_filter_items, paste0("teal_filters_", dataname), datasets, dataname)
    })

    asl_vars <- names(datasets$get_data("ASL"))
    lapply(datasets$datanames(), function(dataname) {
      callModule(srv_add_filter_variable, paste0("teal_add_", dataname, "_filters"), datasets, dataname,
                 omit_vars = if (dataname == "ASL") NULL else asl_vars)
    })

    ## hide-show filters based on module filter property
    recurse <- function(x, idprefix) {
      id <- label_to_id(x$label, idprefix)

      if (is(x, "teal_module")) {
        setNames(list(if (is.null(x$filters)) NA else x$filters), id)
      } else if (is(x, "teal_modules")) {
        lapply(x$modules, recurse, idprefix = id)
      }
    }

    # named vector with ids and datasets
    filters_tab_lookup <- unlist(recurse(modules, "teal_modules"))

    recurse_modules <- function(x, idprefix) {
      id <- label_to_id(x$label, idprefix)

      if (is(x, "teal_modules")) {
        c(id, lapply(x$modules, recurse_modules, idprefix = id))
      } else {
        NULL
      }
    }
    id_modules <- unlist(recurse_modules(modules, "teal_modules"))

    ## now show or hide the filter panels based on active tab
    observe({
      # define reactivity dependence
      main_tab <- input[["teal_modules.root"]]
      secondary_tabs <- sapply(id_modules, function(id) input[[id]],  USE.NAMES = TRUE)

      # figure out which is the active tab/module
      main_tab_id <- label_to_id(main_tab, "teal_modules.root")

      active_module_id <- if (main_tab_id %in% id_modules) {
        label_to_id(secondary_tabs[[main_tab_id]], main_tab_id)
      } else {
        main_tab_id
      }

      filters <- filters_tab_lookup[[active_module_id]]
      .log("Active filter for tab", active_module_id, "is", filters)

      if (is.na(filters)) {
        session$sendCustomMessage(type = "tealShowHide", list(selector = "#teal_filter-panel", action = "hide"))
      } else {

        session$sendCustomMessage(type = "tealShowHide", list(selector = "#teal_filter-panel", action = "show"))

        if ("all" %in% filters) {
          lapply(datasets$datanames(), function(dataname) {
            session$sendCustomMessage(type = "tealShowHide", list(selector = paste0(".teal_filter_", dataname),
                                                                   action = "show"))
          })
        } else {
          Map(function(dataname) {
            session$sendCustomMessage(
              type = "tealShowHide",
              list(selector = paste0(".teal_filter_", dataname),
                   action = if (dataname == "ASL" || dataname %in% filters) "show" else "hide"
              )
            )
          },  datasets$datanames())
        }
      }

    })

  }

  list(server = server, ui = ui, datasets = datasets)
}
