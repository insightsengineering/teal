

#' Create server and ui function for shiny app
#'
#' Creates the server and ui part for a teal shiny app
#'
#' @param data named list with datasets. Dataset names are converted to
#'   lowercase. The `asl` data is madatory.
#' @param analysis nested list with one list per analysis item. See details for
#'   more information.
#' @param header object of class `shiny.tag` to be used as the header of the app
#' @param footer object of class `shiny.tag` to be used as the footer of the app
#'
#'
#' @details Analysis item lists for the `analysis` argument need the following
#'   named elements
#'
#'
#' @export
#'
#' @return list with server and ui function
#'
#' @examples
#' \dontrun{
#'
#' x <- teal::init(
#'
#'
#' )
#'
#'
#' }
init <- function(data, analysis, filter, header = tags$p("title here"), footer = tags$p("footer here")) {

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
                tabPanel("data table", p("data table")),
                tabPanel("variable browser", p("variable-browser"))
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
              column(3, div(id="teal_filter_panel", class="well", style="height: 200px;", p("Filters")))
            )
          )
          tp
        }),
        tags$hr(),
        tags$footer(footer)
      )
  )


  datasets <- FilteredData$new(tolower(names(data)))
  Map(function(x, name) {
   datasets$load_data()
  }, data, names(data))

  server <- function(input, output) {


     #callModule(srv_page_data_table, "page_data_table")
     #callModule(srv_page_variable_browser, "page_variable_browser")

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

