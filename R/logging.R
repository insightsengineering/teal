.log_depth <- 0

# Teal Internally Used Logger functions

#' @export
.log <- function(..., sep = " ", type = "debug") {
  if (!isTRUE(getOption("teal_logging"))) {
    return()
  }

  ## force the evaluation of arguments
  args <- unlist(Map(function(x) if (length(x) > 1) paste(x, collapse = ", ") else x, list(...)))

  cat(paste0(paste(rep(" ", times = 2 * .log_depth), collapse = ""), "|"))
  cat(paste(args, collapse = sep))
  cat("\n")
}

logger_in <- function() {

  if (!isTRUE(getOption(x = "teal_logging"))) {
    return()
  }

  .log_depth <<- .log_depth + 1
}

logger_out <- function() {

  if (!isTRUE(getOption(x = "teal_logging"))) {
    return()
  }

  .log_depth <<- .log_depth - 1

  if (.log_depth < 0) {
    .log_depth <<- 0
    cat("logger depth is < 0 !!! \n")
  }

}
