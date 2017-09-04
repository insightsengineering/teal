
# are elements unique
is.unique <- function(x) length(x) == length(unique(x))


as.global <- function(x) {
  var <- deparse(substitute(x))
  .GlobalEnv[[var]] <- x
}



