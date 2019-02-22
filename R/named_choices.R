#' Set the names of a data.frame or vector
#'
#' @param x either data.frame or character vector
#' @param xnames vector containing names to be applied to \code{x}
#' 
#' @details If either \code{x} or \code{xnames} are factors, they are coerced 
#'   to character.
#'   
#'   This function is useful to create the vectors passed on to 
#'   the \code{choices_selected} arguments in \code{teal} modules.
#' 
#' @return a named character vector 
#' 
#' @importFrom stats setNames
#' 
#' @export
#'
#' @examples
#' library(random.cdisc.data)
#' library(tern)
#' ADSL <- radsl(N=10, seed = 1)
#' ADTTE <- radtte(ADSL, seed = 1)
#' 
#' choices1 <- named_choices(ADSL, tern::var_labels(ADSL))
#' choices2 <- named_choices(ADTTE$PARAMCD, ADTTE$PARAM)
#' 
#' \dontrun{
#' 
#' library(shiny)
#' 
#' shinyApp(
#' ui = fluidPage(selectInput("c1", label = "Choices from ADSL",
#'                 choices = choices1,
#'                 selected = choices1[1]),
#'                  
#'                selectInput("c2", label = "Choices from ADTTE",
#'                 choices = choices2,
#'                 selected = choices2[1])),
#'                   
#' server = function(input, output) {}
#' )
#' 
#' }

named_choices <- function(x, xnames){
  
  if(is.factor(x)){
    x <- as.character(x)
  }
  
  if(is.factor(xnames)){
    xnames <- as.character(xnames)
  }
  
  (is.data.frame(x)||is.character(x)) || stop("x must be data.frame or character vector.")
  
  if (is.data.frame(x)){
    
    nam <- names(x)
    ncol(x) == length(xnames) || stop("length of xnames must match the number of columns in x")
    vl <- xnames
    
  }else if(is.character(x)){
    
    length(x) == length(xnames) || stop("length of xnames must be the same as x")
    df <- unique(data.frame(xnames, x, stringsAsFactors = FALSE))
    
    nam <- df$x 
    vl <- df$xnames
  
  }
  
  nam_vl <- paste0(nam, ": ", vl)
  named_x <- stats::setNames(nam, nam_vl)
  
  named_x
}



