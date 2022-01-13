#' Stop app.R evaluation with a Jumbotron Shiny App
#'
#' @description `r lifecycle::badge("maturing")`
#' This will start a separate shiny app with a message
#'
#' @param title string with title
#' @param body either a string or an object of class \code{shiny.tag}
#' @export
#' @examples
#' \dontrun{
#' stop_shiny("No data access", "You do not seem to have data access")
#' stop_shiny("No data access", tagList(
#'   tags$h2("Hello World"), tags$p("paragraph"),
#'   tags$footer("Please contact", tags$a(href = "mailto:john@doe.com", "John Doe"))
#' ))
#' }
stop_shiny <- function(title, body) {
  if (!inherits(body, c("shiny.tag", "shiny.tag.list", "html"))) {
    body <- tags$p(body)
  }
  shinyApp(
    ui = fixedPage(
      div(
        class = "jumbotron",
        tags$h1(title),
        body
      )
    ),
    server = function(input, output, session) {

    }
  )
}
