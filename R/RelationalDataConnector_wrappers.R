# delayed relational data wrappers ----
#' Data connector for \code{random.cdisc.data}
#'
#' Build data connector for \code{random.cdisc.data} functions or datasets
#'
#' @export
#'
#' @param ... (\code{DatasetConnector}) dataset connectors created using \link{rcd_dataset_connector}
#' @param check optional, (\code{logical}) whether perform reproducibility check
#'
#' @return An object of class \code{RelationalDataConnector}
#'
#' @examples
#' library(random.cdisc.data)
#' x <- rcd_cdisc_data(
#'   rcd_cdisc_dataset_connector("ADSL", radsl, cached = TRUE),
#'   rcd_cdisc_dataset_connector("ADLB", radlb, cached = TRUE)
#' )
#'
#' # add the data to a proper teal app
#' app <- init(
#'   data = x,
#'   modules = root_modules(
#'     module(
#'       "ADSL AGE histogram",
#'       server = function(input, output, session, datasets) {
#'         output$hist <- renderPlot(
#'           hist(datasets$get_data("ADSL", filtered = TRUE)$AGE)
#'         )
#'       },
#'       ui = function(id, ...) {ns <- NS(id); plotOutput(ns('hist'))},
#'       filters = "ADSL"
#'     )
#'   ),
#'   header = tags$h1("Sample App")
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
rcd_cdisc_data <- function(..., check = TRUE) {
  connectors <- list(...)
  stopifnot(is_class_list("RelationalDatasetConnector")(connectors))

  con <- rcd_connection() # nolint

  x <- RelationalDataConnector$new()
  x$set_connection(con)
  x$set_connectors(connectors)
  x$set_check(check)
  x$set_ui(
    function(id) {
      ns <- NS(id)
      tagList(
        numericInput(ns("seed"), "Choose seed", min = 1, max = 1000, value = 1),
        actionButton(ns("submit"), "Submit")
      )
    }
  )
  x$set_server_helper(
    submit_id = "submit",
    fun_args_fixed = list(seed = quote(input$seed))
  )
  x$set_server_info(
    submit_id = "submit",
    fun_args_fixed = list(seed = quote(input$seed))
  )

  return(x)
}


#' Data connector for \code{RICE}
#'
#' Build data connector for \code{RICE} datasets
#'
#' @importFrom askpass askpass
#'
#' @export
#'
#' @param ... (\code{DatasetConnector}) dataset connectors created using \link{rice_dataset_connector}
#' @param additional_ui \code{shiny.tag} additional user interface to be visible over login panel
#'
#' @return An object of class \code{DataConnector}
#'
#' @examples
#' x <- rice_cdisc_data(
#'   rice_cdisc_dataset_connector("ADSL", "/path/to/ADSL"),
#'   rice_cdisc_dataset_connector("ADLB", "/path/to/ADLB")
#' )
#' app <- init(
#'   data = x,
#'   modules = root_modules(
#'     module(
#'       "ADSL AGE histogram",
#'       server = function(input, output, session, datasets) {
#'         output$hist <- renderPlot({
#'           hist(datasets$get_data("ADSL", filtered = TRUE)$AGE)
#'         })
#'       },
#'       ui = function(id, ...) {ns <- NS(id); plotOutput(ns('hist'))},
#'       filters = "ADSL"
#'     )
#'   ),
#'   header = tags$h1("Sample App")
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
rice_cdisc_data <- function(..., additional_ui = NULL) {
  connectors <- list(...)
  stopifnot(is_class_list("RelationalDatasetConnector")(connectors))

  con <- rice_connection() # nolint

  x <- RelationalDataConnector$new()
  x$set_connection(con)
  x$set_connectors(connectors)
  x$set_check(`attributes<-`(FALSE, list(quiet = TRUE)))
  x$set_ui(
    function(id) {
      ns <- NS(id)
      shinyjs::useShinyjs()
      fluidPage(
        fluidRow(
          column(
            8,
            offset = 2,
            ui_connectors("rice", connectors),
            br(),
            additional_ui,
            br(),
            textInput(ns("login"), "Login"),
            passwordInput(ns("pass"), "Password"),
            actionButton(ns("submit"), "Submit")
          )
        )
      )
    }
  )
  x$set_server_helper(
    submit_id = "submit",
    con_args_fixed = list(username = quote(input$login)),
    con_args_dynamic = list(password = quote(input$pass)),
    con_args_replacement = list(password = quote(askpass::askpass()))
  )
  x$set_server_info(
    submit_id = "submit",
    con_args_fixed = list(username = quote(input$login)),
    con_args_dynamic = list(password = quote(input$pass)),
    con_args_replacement = list(password = quote(askpass::askpass()))
  )

  return(x)
}

#' Data connector for \code{.rds} files
#'
#' Build data connector for RDS file connections
#'
#' @export
#'
#' @param ... (\code{DatasetConnector}) dataset connectors created using \link{rds_dataset_connector}
#' @param check optional, (\code{logical}) whether perform reproducibility check
#'
#' @return An object of class \code{DataConnector}
#'
#' @examples
#' \dontrun{
#' x <- rds_cdisc_dataset_connector("ADSL", "/path/to/file.rds")
#' app <- init(
#'   data = rds_cdisc_data(x, check = TRUE),
#'   modules = root_modules(
#'     module(
#'       "ADSL AGE histogram",
#'       server = function(input, output, session, datasets) {
#'         output$hist <- renderPlot(
#'           hist(datasets$get_data("ADSL", filtered = TRUE)$AGE)
#'         )
#'       },
#'       ui = function(id, ...) {ns <- NS(id); plotOutput(ns('hist'))},
#'       filters = "ADSL"
#'     )
#'   ),
#'   header = tags$h1("Sample App")
#' )
#' shinyApp(app$ui, app$server)
#' }
rds_cdisc_data <- function(...,  check = TRUE) {
  connectors <- list(...)
  stopifnot(is_class_list("RelationalDatasetConnector")(connectors))


  x <- RelationalDataConnector$new()
  x$set_connectors(connectors)
  x$set_check(check)
  x$set_ui(
    function(id) {
      ns <- NS(id)
      tagList(
        h4("RDS files to laod:"),
        do.call(
          tagList,
          lapply(x$get_connectors(), function(con) {
            tags$p(
              paste(
                con$get_dataname(),
                ":",
                con$get_pull_fun()$args$file
              )
            )
          })
        ),
        actionButton(ns("start"), "Start")
      )
    }
  )
  x$set_server_helper(
    submit_id = "start",
    fun_args_fixed = NULL
  )
  x$set_server_info(
    submit_id = "start",
    fun_args_fixed = NULL
  )

  return(x)
}

#' Creates UI from \code{DatasetConnector} objects
#'
#' @param type \code{character} giving the type of connection.
#' @param connectors \code{list} of \code{DatasetConnector} objects.
#'
#' @return \code{shiny.tag} UI describing the connectors
#'
ui_connectors <- function(type, connectors) {

  stopifnot(is_character_single(type))
  stopifnot(is_class_list("RelationalDatasetConnector")(connectors))

  out <- div(
    h1("TEAL - Access data on entimICE using", toupper(type)),
    br(),
    h5("Data access requested for:"),
    fluidRow(
      column(
        11,
        offset = 1,
        lapply(seq_along(connectors), function(i) {
          tags$li(paste0(connectors[[i]]$get_dataname(),
                         ": ",
                         connectors[[i]]$args$node))
        })
      )
    )
  )

  return(out)
}
