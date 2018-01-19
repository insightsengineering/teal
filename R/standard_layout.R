


#' Create a standard ui layout with output on the right and an encoding panel on
#' the left
#'
#' This is the layout used for the teal modules in the \code{beam} package
#'
#' @param output \code{shiny.tag} object with the output element (table, plot,
#'   listing) such as for example returned by \code{\link[shiny]{plotOutput}}.
#' @param encoding a \code{shiny.tag} object containing the encoding elements.
#'   If this element is \code{NULL} then no encoding side panel on the right is
#'   created.
#' @param forms a \code{\link[shiny]{tagList}} of forms (e.g.
#'   \code{\link[shiny]{actionButton}}) that are placed below the encodings panel
#' @param pre_output optional, \code{shiny.tag} with text placed before the
#'   output to put the output into context. For example the
#'   \code{\link[shiny]{helpText}} elements are useful.
#' @param post_output optional, \code{shiny.tag} with text placed before the
#'   output to put the output into context. For example the
#'   \code{\link[shiny]{helpText}} elements are useful.
#'
#' @return an object of class \code{shiny.tag} with the ui code.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' ui_test <- function(id) {
#'  ns <- NS(id)
#'  standard_layout(
#'    output = plotOutput(ns("plot")),
#'    encoding = helpText("Ecoding Panel"),
#'    forms = tags$div(
#'       actionButton(ns("show_r_code"), "Show R Code"),
#'       actionButton(ns("export_plot"), "Export Plot")
#'    ),
#'    pre_output = helpText("This is a plot of the", tags$code("iris"), "data"),
#'    post_output = helpText("More information can be added here.")
#'  )
#' }
#'
#' srv_test <- function(input, output, session) {
#'    output$plot <- renderPlot({
#'       with(iris, plot(Sepal.Length, Petal.Length, col = Species))
#'    })
#' }
#'
#' ui_test2 <- function(id) {
#'  ns <- NS(id)
#'  standard_layout(
#'    output = plotOutput(ns("plot")),
#'    encoding = NULL,
#'    forms = tags$div(
#'       actionButton(ns("show_r_code"), "Show R Code"),
#'       actionButton(ns("export_plot"), "Export Plot")
#'    ),
#'    pre_output = helpText("This is a plot of the", tags$code("iris"), "data"),
#'    post_output = helpText("More information can be added here.")
#'  )
#' }
#'
#' srv_test2 <- function(input, output, session) {
#'    output$plot <- renderPlot({
#'       with(iris, plot(Sepal.Length, Petal.Length, col = Species))
#'    })
#' }
#'
#' x <- teal::init(
#'    data = list(ASL = generate_sample_data('ASL')),
#'    modules = root_modules(
#'       module(
#'          "example",
#'          ui = ui_test,
#'          server = srv_test,
#'          filters = "ASL"
#'       ),
#'       module(
#'          "example no encoding",
#'          ui = ui_test2,
#'          server = srv_test2,
#'          filters = "ASL"
#'       )
#'    )
#' )
#'
#' shinyApp(x$ui, x$server)
#'
#' }
#'
standard_layout <- function(output,
                            encoding=NULL, forms=NULL,
                            pre_output=NULL, post_output=NULL) {

  # checking arguments
  if (!is(output, "shiny.tag")) stop("output is supposed to be of class shiny.tag")
  for (el in c("encoding", "pre_output", "post_output")) {
    x_el <- get(el)
    if (!is.null(x_el) && !is(x_el, "shiny.tag")) stop(paste(el, "is supposed to be of class shiny.tag"))
  }

  # if encoding=NULL then forms is placed below output

  tag_output <- div(
    class="well",
    div(id = "pre-output", pre_output),
    div(id = "output", output),
    div(id = "post-output", post_output)
  )

  tag_enc_out <- if (!is.null(encoding)) {
    div(
      div(
        class="col-md-3",
        div(class="well", encoding),
        if (is.null(forms)) NULL else div(class="form-group", forms)
      ),
      div(class = "col-md-9", tag_output)
    )
  } else {
    div(class="col-md-12",
      tag_output,
      if (is.null(forms)) NULL else div(class="form-group", forms)
    )
  }

  fluidRow(tag_enc_out)

}
