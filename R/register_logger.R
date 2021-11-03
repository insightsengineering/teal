#' Registers a logger instance in a given logging namespace.
#'
#' @note It's a thin wrapper around the `logger` package.
#'
#' @details Creates a new logging namespace specified by the `namespace` argument.
#' When the `layout` and `level` arguments are set to `NULL` (default), the functions
#' gets the values for them from system variables or R options.
#' When deciding what to use (either argument, an R option or system variable), the function
#' picks the first non `NULL` value, checking in order:
#' 1. Function argument.
#' 2. System variable.
#' 3. R option.
#'
#' The function uses the following R options:
#' * `options(teal.log_layout)`, which is passed to
#' \code{\link[logger:layout_glue_generator]{logger::layout_glue_generator}},
#' * `options(teal.log_level)`, which is passed to
#' \code{\link[logger:log_threshold]{logger::log_threshold}}.
#'
#' `layout` and `level` can also be set as system environment variables, respectively:
#' * `teal.log_layout` as `TEAL.LOG_LAYOUT`,
#' * `teal.log_level` as `TEAL.LOG_LAYOUT`.
#'
#' The logs are output to `stdout` by default. Check `logger` for more information
#' about layouts and how to use `logger`.
#'
#' @param namespace (`character(1)` or `NA`) the name of the logging namespace
#' @param layout (`character(1)`) the log layout
#' @param level (`character(1)` or `call`) the log level. Can be passed as
#'   character or one of the `logger`'s objects.
#'   See \code{\link[logger:log_threshold]{logger::log_threshold}} for more information.
#'
#' @return `invisible(NULL)`
#' @export
#'
#' @examples
#' options(teal.log_layout = "{msg}")
#' options(teal.log_level = logger::INFO)
#' register_logger(namespace = "new_namespace")
#' \dontrun{
#' logger::log_info("Hello from new_namespace", namespace = "new_namespace")
#' }
#'
register_logger <- function(namespace = NA_character_,
                            layout = NULL,
                            level = NULL) {
  if (!((is.character(namespace) && length(namespace) == 1) || is.na(namespace))) {
    stop("namespace argument to register_logger must be a scalar character or NA.")
  }

  if (is.null(level)) level <- Sys.getenv("TEAL.LOG_LEVEL")
  if (is.null(level) || level == "") level <- getOption("teal.log_level")
  tryCatch(
    logger::log_threshold(level, namespace = namespace),
    error = function(condition) {
      stop(paste(
        "The log level passed to logger::log_threshold was invalid.",
        "Make sure you pass or set the correct log level.",
        "See `logger::log_threshold` for more information"
      ))
    }
  )

  if (is.null(layout)) layout <- Sys.getenv("TEAL.LOG_LAYOUT")
  if (is.null(layout) || layout == "") layout <- getOption("teal.log_layout")
  tryCatch({
    logger::log_layout(logger::layout_glue_generator(layout), namespace = namespace)
    logger::log_appender(logger::appender_file(nullfile()), namespace = namespace)
    logger::log_success("Set up the logger", namespace = namespace)
    logger::log_appender(logger::appender_stdout, namespace = namespace)
  },
    error = function(condition) {
      stop(paste(
        "Error setting the layout of the logger.",
        "Make sure you pass or set the correct log layout.",
        "See `logger::layout` for more information."
      ))
    }
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
  paste_pkgs_name_with_version <- function(names) {
    vapply(
      names,
      function(name) paste(name, utils::packageVersion(name)),
      FUN.VALUE = character(1),
      USE.NAMES = FALSE
    )
  }

  info <- utils::sessionInfo()

  logger::log_trace("Platform: { info$platform }")
  logger::log_trace("Running under: { info$running }")
  logger::log_trace("{ info$R.version$version.string }")
  logger::log_trace("Base packages: { paste(info$basePkgs, collapse = ' ') }")

  # Paste package names and versions
  pasted_names_and_versions <- paste(paste_pkgs_name_with_version(names(info$otherPkgs)), collapse = ", ")
  logger::log_trace("Other attached packages: { pasted_names_and_versions }")

  pasted_names_and_versions <- paste(paste_pkgs_name_with_version(names(info$loadedOnly)), collapse = ", ")
  logger::log_trace("Loaded packages: { pasted_names_and_versions }")
}
