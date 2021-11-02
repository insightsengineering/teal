.log_depth <- 0

# Teal Internally Used Logger functions

#' @export
.log <- function(..., sep = " ", type = "debug") {
  lifecycle::deprecate_soft(
    when = "0.10.1",
    what = ".log()",
    details = "teal now uses the package `logger`. Please see `teal::register_logger` for more information."
  )

  if (!isTRUE(getOption("teal_logging"))) {
    return()
  }

  ## force the evaluation of arguments
  args <- unlist(Map(function(x) if (length(x) > 1) paste(x, collapse = ", ") else x, list(...)))

  cat(paste0(paste(rep(" ", times = 2 * .log_depth), collapse = ""), "|"))
  cat(paste(args, collapse = sep))
  cat("\n")
}
