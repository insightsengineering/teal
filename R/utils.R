
# are elements unique
is.unique <- function(x) length(x) == length(unique(x))


as.global <- function(x) {
  var <- deparse(substitute(x))
  .GlobalEnv[[var]] <- x
}


# add hidden class
hidden <- function(x) {
  if(!is(x, "shiny.tag")) stop("x needs to be of class shiny.tag")
  x$attribs$class <- paste(x$attribs$class, "hidden")
  x
}


# this control widget does not show the selectInput if there is nothing to
# select from
optionalSelectInput <- function(inputId, label, choices, selected, ...) {

  selIn <- selectInput(inputId, label, choices, selected, ...)

  if (is.null(choices)) {
    hidden(selIn)
  } else if (length(choices) <= 1) {
    div(
      hidden(selIn),
      tags$span(tags$label(paste0(sub(":[[:space:]]+$", "", label), ":")), selected)
    )
  } else {
    selIn
  }
}
