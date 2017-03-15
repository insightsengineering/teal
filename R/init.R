

#' Create the Server and Ui Function for the Shiny App
#'
#' Creates the server and ui part for a teal shiny app
#'
#' @param data named list with datasets. Dataset names are converted to
#'   lowercase. The `asl` data is madatory.
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
#' ARS <- generate_sample_data('ars')
#'
#' x <- teal::init(
#'   data =  list(asl = ASL, ars = ARS),
#'   tabs = tabs(
#'     tab_item(
#'       "data source",
#'       server = function(input, output, session, datasets) {},
#'       ui = function(id) div(p("data source")),
#'       filter = c(),
#'       server_args = list(datasets = "teal_datasets")
#'     ),
#'     data_table_item(),
#'     variable_browser_item(),
#'     tabs_item(
#'       "analysis items",
#'       tabs = tabs(
#'         tab_item(
#'           "spaghetti plot",
#'           server = function(input, output, session, datasets) {},
#'           ui = function(id) div(p("spaghetti plot")),
#'           filters = c('ars'),
#'           server_args = list(datasets = "teal_datasets")
#'         ),
#'         tab_item(
#'           "survival curves",
#'           server = function(input, output, session) {},
#'           ui = function(id) div(p("Kaplan Meier Curve")),
#'           filters = c()
#'         )
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
                 tabs,
                 filter = NULL,
                 header = tags$p("title here"),
                 footer = tags$p("footer here")) {


  # initialize FilteredData object
  datasets <- FilteredData$new(tolower(names(data)))

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
            HTML("
                  Shiny.addCustomMessageHandler('setDisplayCss', function(message) {
                    $(message.selector).css('display', message.type);
                  });"
            )
          )
        ),
        tags$header(header),
        tags$hr(style="margin: 7px 0;"),
        local({
          tp <- ui_tabs(tabs, datasets)

          tp$children <- list(
            tp$children[[1]],
            tags$hr(style="margin: 7px 0;"),
            fluidRow(
              column(9, tp$children[[2]]),
              column(3, div(id="teal_filter_panel",
                            div(class="well",
                                tags$label("Active Filter Variables", class="text-primary", style="margin-bottom: 15px;"),
                                tagList(
                                  lapply(datasets$datanames(), function(dataname) {
                                    ui_filter_items(paste0("teal_filters_", dataname))
                                  })
                                )
                            ),
                            div(class="well",
                                tags$label("Add Filter Variables", class="text-primary", style="margin-bottom: 15px;"),
                                tagList(
                                  lapply(datasets$datanames(), function(dataname) {
                                    ui_add_filter_variable(paste0("teal_add_", dataname, "_filters"), toupper(dataname))
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
    server_tabs(tabs, datasets)


    # -- filters
    lapply(datasets$datanames(), function(dataname) {
      callModule(srv_filter_items, paste0("teal_filters_", dataname), datasets, dataname)
    })

    lapply(datasets$datanames(), function(dataname) {
      callModule(srv_add_filter_variable, paste0("teal_add_", dataname, "_filters"), datasets, dataname)
    })



    ## hide-show filters based on tab-item filter property
    tabs_ids <- unlist(Map(function(x) {
      setNames(paste(main_nav_id, label_to_id(x$label), sep="_"), x$label)
    } , Filter(function(x) is(x, "teal_tabs_item"), tabs)))


    .GlobalEnv$tabs_ids <- tabs_ids
    observe({

      main_tab <- input[[main_nav_id]]

      tabs_items <- sapply(tabs_ids, function(id) input[[id]],  USE.NAMES = TRUE)

      .GlobalEnv$tabs_items <- tabs_items


      .log("AAAAAAAAAAAAAAAAAAAAAAAA:", main_tab)
      .log("AAAAAAAAAAAAAAAAAAAAAAAA:", paste(tabs_items, collapse = ", "))


    })

    # --


  }

  list(server = server, ui = ui, datasets = datasets)
}

