#' Registers a logger instance in a given logging namespace.
#'
#' @note It's a thin wrapper around the `logger` package.
#'
#' @details Creates a new logging namespace specified by the `namespace` argument.
#' Sets the logging level to `logger::INFO` and updates the logging layout.
#' Uses `getOption(teal_logging_layout)` to determine the layout of logs and
#' passes it to \code{\link[logger:layout_glue_generator]{logger::layout_glue_generator}}.
#' The logs are output to `stdout` by default. Check `logger` for more information
#' about layouts and how to use `logger`.
#'
#' @param namespace (`character` or `NA`) the name of the logging namespace
#'
#' @return `invisible(NULL)`
#' @export
#'
#' @examples
#' register_logger(namespace = "new_namespace")
#' \dontrun{
#' logger::log_info("Hello from new_namespace")
#' }
#'
register_logger <- function(namespace = NA_character_) {
  if (!((is.character(namespace) && length(namespace) == 1) || is.na(namespace))) {
    stop("namespace argument to register_logger must be a scalar character or NA.")
  }
  logger::log_threshold(logger::INFO, namespace = namespace)
  tryCatch({
    logger::log_layout(logger::layout_glue_generator(getOption("teal_logging_layout")), namespace = namespace)
    logger::log_appender(logger::appender_file(nullfile()), namespace = namespace)
    logger::log_success("Set up the logger", namespace = namespace)
    logger::log_appender(logger::appender_stdout, namespace = namespace)
  },
    error = function(condition)
      stop("Error setting the layout of the logger. Check that the layout is correct in `getOption(teal_logging_layout)`.")
  )

  invisible(NULL)
}

#' Logs the basic information about the session.
#'
#' @return `invisible(NULL)`
#'
#' @noRd
#'
log_system_info <- function() {
  info <- sessionInfo()

  logger::log_trace("Platform: { info$platform }")
  logger::log_trace("Running under: { info$running }")
  logger::log_trace("{ info$R.version$version.string }")
  logger::log_trace("Base packages: { paste(info$basePkgs, collapse = ' ') }")

  # Paste package names and descriptions
  pasted_names_and_versions <- paste(paste_pkgs_name_with_version(names(info$otherPkgs)), collapse = ", ")
  logger::log_trace("Other attached packages: { pasted_names_and_versions }")

  pasted_names_and_versions <- paste(paste_pkgs_name_with_version(names(info$loadedOnly)), collapse = ", ")
  logger::log_trace("Loaded packages: { pasted_names_and_versions }")
}

#' @noRd
#'
paste_pkgs_name_with_version <- function(names) {
  vapply(
    names,
    function(name) paste(name, packageVersion(name)),
    FUN.VALUE = character(1),
    USE.NAMES = FALSE
  )
}
