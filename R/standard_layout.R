


#' Create a standard ui layout with output on the right and an encoding panel on
#' the left
#'
#'
#' This is the layout used for the teal modules in the \code{beam} package
#'
#'
#' @export
#'
standard_layout <- function(output,
                            encoding=NULL, forms=NULL,
                            pre_output=NULL, post_output=NULL) {

  if (!is(output, "shiny.tag")) stop("output is supposed to be of class shiny.tag")
  for (el in c("encoding", "pre_output", "post_output")) {
    x_el <- get(el)
    if (!is.null(x_el) && !is(x_el, "shiny.tag")) stop(paste(el, "is supposed to be of class shiny.tag"))
  }


  fluidRow(
    div(
      class="col-md-3",
      if (is.null(encoding)) NULL else div(class="well", encoding),
      if (is.null(forms)) NULL else div(class="form-group", forms)
    ),
    div(
      class = "col-md-9",
      div(
        class="well",
        div(id = "pre-output", pre_output),
        div(id = "output", output),
        div(id = "post-output", post_output)
      )
    )
  )

}
