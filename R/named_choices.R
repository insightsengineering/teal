#' Set "<choice>: <label>" type of Names
#' 
#' This is often useful for \code{\link[teal]{choices_selected}} as it marks up the dropdown boxes
#' \code{\link[shiny]{selectInput}}. Duplicate Choices are removed.
#' 
#' @param choices a character vector
#' @param labels vector containing label to be applied to \code{choices}
#' 
#' @details If either \code{choices} or \code{labels} are factors, they are coerced 
#'   to character.
#'   
#'   Duplicated choices get removed.
#' 
#' @return a named character vector 
#' 
#' @importFrom stats setNames
#' 
#' @export
#'
#' @examples
#' 
#' library(random.cdisc.data)
#' library(tern)
#' ADSL <- radsl(N=10, seed = 1)
#' ADTTE <- radtte(ADSL, seed = 1)
#' 
#' choices1 <- named_choices(names(ADSL), tern::var_labels(ADSL))
#' choices2 <- named_choices(ADTTE$PARAMCD, ADTTE$PARAM)
#' 
#' # if only a subset of variables are of intereste then the following code will work
#' vl_ADSL <- tern::var_labels(ADSL)
#' vl_arm <- vl_ADSL[c("ARM", "ARMCD")]
#' choices3 <- named_choices(names(vl_arm), vl_arm)
#' 
#' 
#' \dontrun{
#' 
#' library(shiny)
#' 
#' shinyApp(
#'   ui = fluidPage(selectInput("c1", label = "Choices from ADSL",
#'                              choices = choices1,
#'                              selected = choices1[1]),
#'                  
#'                  selectInput("c2", label = "Choices from ADTTE",
#'                              choices = choices2,
#'                              selected = choices2[1])),
#'   
#'   server = function(input, output) {}
#' )
#' 
#' }

named_choices <- function(choices, labels){
  
  if(is.factor(choices)){
    choices <- as.character(choices)
  }
  
  if(is.factor(labels)){
    labels <- as.character(labels)
  }
  
  stopifnot(is.character(choices))
  length(choices) == length(labels) || stop("length of xnames must be the same as x")
  
  is_dupl <- duplicated(choices)
  
  stats::setNames(choices[!is_dupl], paste0(choices[!is_dupl], ": ", labels[!is_dupl]))
}


