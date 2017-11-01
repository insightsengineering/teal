
#' Check if list or data.frame has elements/variables
#' 
#' Checks if list names exist and throws an error otherwise
#' 
#' @param data a \code{data.frame} or named list
#' @param names vector with names
#' 
#' @return \code{TRUE} if all variables exist and an appropriate error if not.
#' 
#' @author Adrian Waddell (waddella), \email{adrian.waddell@roche.com}
#' 
#' @examples 
#' \dontrun{
#' # function is not exported
#' `%needs%` <- teal.oncology:::`%needs%`
#' 
#' iris %needs% c("Sepal.Length", "Petal.Width")
#' 
#' iris %needs% "ABC"
#' 
#' }
#' 
`%needs%` <- function(data, names) {
  
  i <- is.na(match(names, names(data)))

  if (any(i)) {
    msg <- if (sum(i) == 1) {
      paste("variable ", names[i], " does not exist")
    } else {
      paste("variables", paste(names[i], collapse = ", "), "do not exist")
    }
    stop(msg)
  }
  
  invisible(TRUE)
}


#' return a vector with the variable labels and names if variable labels do not exist
#' 
#' 
#' @param df data.frame object
#' 
#' @export
#' 
#' @examples 
#' 
#' X <- data.frame(
#'  a = structure(1:4, label = "label for a"),
#'  b = 3:6,
#'  d = structure(letters[1:4], label = "label for d"),
#'  stringsAsFactors = FALSE
#' )
#' 
#' View(X)
#' 
#' names(X)
#' 
labels_over_names <- function(df) {
  
  as.vector(unlist(Map(function(var, varname) {
    label <- attr(var, "label")
    if (!is.null(label)) label else varname
  }, df, names(df))))
  
}

whiteSmallWell <- function(...) {
  shiny::tags$div(class = "well well-sm", style = "background-color: white;", ...)
}


         

