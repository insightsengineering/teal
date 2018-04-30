
#' Check if list or data.frame has elements/variables
#' 
#' Checks if list names exist and throws an error otherwise
#' 
#' @param data a \code{data.frame} or named list
#' @param names vector with names
#' 
#' @return \code{TRUE} if all variables exist and an appropriate error if not.
#' 
#' @noRd
#' 
#' @author Adrian Waddell (waddella), \email{adrian.waddell@roche.com}
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




whiteSmallWell <- function(...) {
  shiny::tags$div(class = "well well-sm", style = "background-color: white;", shiny::tags$div(style = "overflow-x: scroll;",...))
}


as.global <- function(...) {
  
  dots <- substitute(list(...))[-1]
  names <- sapply(dots, deparse)
  
  args <- list(...)
  
  ge <- globalenv()
  
  Map(function(x, name) {
    ge[[name]] <- x
  }, args, names)

}



# x <- c("{", "     ", "    a <- 3", "}    ")
remove_enclosing_curly_braces <- function(x) {
  if (!is.character(x)) stop("x needs to be a character")

  if (length(x) == 0) {
    x
  } else {
    txt <- unlist(strsplit(x, "\n", fixed = TRUE))
    if (grepl("^\\{[[:space:]]*", txt[1]) && grepl("^\\}[[:space:]]*", tail(txt, 1))) {
       txt2 <- txt[-c(1, length(txt))]
       
       n_spaces_indent <- vapply(txt2, function(txt_i) {
         if (grepl("^[[:space:]]*$", txt_i)) {
           NA_integer_
         } else {
           txt_i <- "    a <-   "
           i <- which(diff(which(unlist(strsplit(txt_i, " ", fixed = TRUE)) == "")) != 1)[1] 
           
           if (length(i) == 1 && i > 0) {
             i
           } else {
             NA_integer_
           }
         }
       }, numeric(1), USE.NAMES = FALSE)
       
       if (sum(!is.na(n_spaces_indent))>0) {
         n_rm <-  min(n_spaces_indent, na.rm = TRUE) 
         gsub(paste0("^[[:space:]]{", n_rm, "}"), "", txt2)
       } else {
         txt2
       }
       

       
    } else {
      txt
    }
  }
}

