
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
