

#' Create the Server and Ui Function for the Shiny App
#'
#' Creates the server and ui part for a teal shiny app
#'
#' @param data named list with datasets. Dataset names are converted to
#'   lowercase. The `asl` data is madatory.
#' @param analysis nested list with one list per analysis item with the following named list elements:
#'   \tabular{ll}{
#'   name \tab string with name shown in menu for the analysis item \cr
#'   server \tab required, shiny server module function, see \code{\link[shiny]{callModule}} for more information\cr
#'   ui \tab required, shiny ui module function, see \code{\link[shiny]{callModule}} for more information\cr
#'   data \tab required, vector with datasets names that are passed on (filtered) to the server function\cr
#'   options \tab optional, other arguments passed on to the server function
#'  }
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
#'       server = function(input, output, session, ars) {},
#'       ui = function(id) div(p("spaghetti plot")),
#'       data = c(ars="ars")
#'     ),
#'     list(
#'       name = "survival curves",
#'       server = function(input, output, session, asl, ars) {},
#'       ui = function(id) div(p("Kaplan Meier Curve")),
#'       data = c(asl = 'asl', ars='ars')
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
                 analysis,
                 header = tags$p("title here"),
                 footer = tags$p("footer here")) {


  datasets <- FilteredData$new(tolower(names(data)))

  Map(function(x, name) {
    datasets$set_data(name, x)
  }, data, names(data))



  ui <- shinyUI(
      fluidPage(
        tags$header(header),
        tags$hr(),
        local({
          tp <- do.call(
            tabsetPanel,
            c(
              list(
                tabPanel("data source", p("data source")),
                tabPanel("overview", p("overview page")),
                tabPanel("data table", ui_page_data_table("teal_data_table", datasets)),
                tabPanel("variable browser", ui_page_variable_browser("teal_variable_browser", datasets))
              ),
              unname(Map(function(x,i) tabPanel(x$name, x$ui(paste0("analysis_item_", i))), analysis, seq_along(analysis))),
              list(
                id = "teal_tabset",
                type = 'pills'
              )
            )
          )
          tp$children <- list(
            tp$children[[1]],
            tags$hr(),
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
    Map(function(x, i) {
     do.call(
       callModule,
       c(
         list(
           module = x$server,
           id = paste0("analysis_item_", i)
         ),
         Map(function(d) datasets$get_data(d, filtered = TRUE, reactive = FALSE), x$data),
         x$options
       )
     )
    }, analysis, seq_along(analysis))
  }

  list(server = server, ui = ui)
}

