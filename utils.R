
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

#' re-attach labels to variables
#' 
#' @param df a data.frame
#' @param labels a named vector with the labels. names are variable names
#' 
#' 
#' @examples
#' 
#' df <- data.frame(c = c(1, 2), b = c("a", "b"), a = c(3,4))
#' 
#' labels <- setNames(c("a var", "b var"), c("a", "b"))
#' 
#' 
#' X <- add_labels(df, labels)
#' 
#' View(X)
#' 
add_labels <- function(df, labels) {
  for (name in names(df)) {
    
    lab <- labels[name]
    if (!is.na(lab[1]) && length(lab) == 1) {
      attr(df[[name]], "label") <- lab
    }
  }
  df
}



whiteSmallWell <- function(...) {
  shiny::tags$div(class = "well well-sm", style = "background-color: white;", ...)
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

