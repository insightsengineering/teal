
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






#' Replace empty string and "." as NA in a vector
#' 
#' Convert "", "   " and "." to NA in a vector
#' 
#' @param input.vec  input vector
#' 
#' @export
#' 
#' @importFrom stringr str_trim
#' 
NaReplaceVec <- function(input.vec){
  ifelse(is.na(input.vec), NA, 
         ifelse(!(str_trim(input.vec) %in% c("", ".")), input.vec, NA))
}


#' Replace empty string as NA in a data table
#' 
#' Convert factor to character, convert empty string and "." for all columns of a data table
#' 
#' @param data.tbl input data table
#' 
#' @export
#' 
#' @import dplyr
#' 
NaReplace <- function(data.tbl){    
  if (!(is.tbl(data.tbl))){
    data.tbl <- as_tibble(data.tbl)
  }
  for (vec in colnames(data.tbl)){
    if (data.tbl[ , vec] %>% pull() %>% is.factor()){
      data.tbl[ , vec] <- data.tbl %>% mutate_at(vec, as.character) %>% mutate_at(vec, NaReplaceVec)
    }else if (data.tbl[ , vec] %>% pull() %>% is.numeric()){
      data.tbl[ , vec] <- data.tbl[ , vec]
    }else{
      data.tbl[ , vec] <- data.tbl %>% mutate_at(vec, NaReplaceVec)
    }
  }
  return(data.tbl)
}