

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
#'   analysis = list(
#'     list(
#'       name = "spaghetti plot",
#'       server = function(input, output, session, datasets) {},
#'       ui = function(id) div(p("spaghetti plot"))
#'     ),
#'     list(
#'       name = "survival curves",
#'       server = function(input, output, session, datasets) {},
#'       ui = function(id) div(p("Kaplan Meier Curve"))
#'     )
#'   ),
#'   elements = list(
#'       data = list(
#'         name = "data source",
#'         server = function(input, output, session) {},
#'         ui = function(id) {p(paste(id, "Information"))},
#'         id = "data"
#'       ),
#'       'data_table',
#'       'variable_browser',
#'       'analysis'
#'   ),
#'   header = tags$h1("Sample App"),
#'   footer = tags$p("Copyright 2017")
#' )
#'
#' shinyApp(x$ui, x$server)
#'
#' }
init <- function(data,
                 analysis,
                 elements = c('data_table', 'variable_browser', 'analysis'),
                 filter = NULL,
                 header = tags$p("title here"),
                 footer = tags$p("footer here")) {


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

  ui <- shinyUI(
      fluidPage(
        tags$header(header),
        tags$hr(style="margin: 7px 0;"),
        local({
          tp <- do.call(
            tabsetPanel,
            c(
              list(
                id = "teal_tabset",
                type = 'pills'
              ),
              unname(Map(function(x) {
                if (is.list(x)) {
                  tabPanel(x$name, x$ui(x$id))
                } else {
                  switch(
                    x,
                    analysis = {
                        tabPanel("analysis items", do.call(
                          tabsetPanel,
                          c(
                            list(id = "teal_analysis_items"),
                            unname(Map(function(x,i) tabPanel(x$name,
                                                              tagList(div(style="margin-top: 25px;"),
                                                                      x$ui(paste0("analysis_item_", i)))),
                                       analysis, seq_along(analysis)))
                          )
                        ))
                    },
                    variable_browser = {
                      tabPanel("variable browser", ui_page_variable_browser("teal_variable_browser", datasets))
                    },
                    data_table = {
                      tabPanel("data table", ui_page_data_table("teal_data_table", datasets))
                    },
                    stop(paste("element type:", x, "not known."))
                  )
                }
              }, elements))
            )
          )
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


  server <- function(input, output) {


    callModule(srv_page_data_table, "teal_data_table", datasets = datasets)
    callModule(srv_page_variable_browser, "teal_variable_browser", datasets = datasets)


    # --
    lapply(datasets$datanames(), function(dataname) {
      callModule(srv_filter_items, paste0("teal_filters_", dataname), datasets, dataname)
    })

    lapply(datasets$datanames(), function(dataname) {
      callModule(srv_add_filter_variable, paste0("teal_add_", dataname, "_filters"), datasets, dataname)
    })
    # --


    # enclosing function is a closure
    Map(function(x) {
      if (is.list(x)) {
        do.call(
          callModule,
          c(
            list(
              module = x$server,
              id = x$id
            ),
            Map(function(d) datasets$get_data(d, filtered = TRUE, reactive = FALSE), x$data),
            x$options
          )
        )
      }
    }, elements)



    # enclosing function is a closure
    Map(function(x, i) {
     do.call(
       callModule,
       c(
         list(
           module = x$server,
           id = paste0("analysis_item_", i),
           datasets = datasets
         ),
         x$options
       )
     )
    }, analysis, seq_along(analysis))
  }

  list(server = server, ui = ui)
}

