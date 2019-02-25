# are elements unique
is.unique <- function(x) length(x) == length(unique(x))


#' Copy a variable to the global environment
#'
#' This function is uselful for debugging shiny apps
#'
#' @param x variable to write to the GlobalEnv. Not non-standard evaluation is
#'   applied for getting the variable name and variable content
#'
as.global <- function(x) {
  var <- deparse(substitute(x))
  .GlobalEnv[[var]] <- x
}
