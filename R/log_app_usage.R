#' Teal Application Usage Logging.
#'
#' @md
#' @description `r lifecycle::badge("maturing")`
#' Analysis of Teal application utilization is a key component towards ongoing enhancements
#' of the framework. The utilization logs provide source data for better understanding
#' frequency and extent of use.
#'
#' @details Each Teal application can implement usage logging by calling this
#' function in the application startup file(s) like app.R. The following data are
#' captured by default: UNIX ID, System Date, Application Directory, Therapeutic Area, Indication,
#' Package Name, Package Title, Package Version and Remotes Definition. Capture frequency is
#' currently per user at session start only.
#' Suggested values for ta: "Oncology" or "I2ON"
#' Suggested values for anl_type: "Exploratory", "Interim Analysis", "CSR"
#'
#' @param ta therapeutic area name. required argument. e.g. \code{"I2ON"}.
#' @param molecule molecule name. required argument. e.g. \code{"Lucentis"}.
#' @param indication indication. required argument. e.g. \code{"AMD"}.
#' @param anl_type analysis type. required argument. e.g. \code{"Interim Analysis"}.
#' @param pkg_meta package metadata fields. default values: \code{c("Package", "Title", "Version", "RemoteRef")}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' log_app_usage(ta = "Oncology", molecule = "Tecentriq", ind = "NSCLC", anl_type = "Exploratory")
#' readLines(file.path("logs", "utilization.log"), warn = FALSE)
#' }
log_app_usage <- function(ta,
                          molecule,
                          indication,
                          anl_type,
                          pkg_meta = c("Package", "Title", "Version", "RemoteRef")) {


  # conditionally create logs directory
  if (!dir.exists("./logs")) {
    dir.create("./logs")
    # set permisions so all users can write to the utilization log directory
    Sys.chmod("./logs", mode = "0777", use_umask = FALSE)
  }

  # conditionally initialize log file
  if (!file.exists("./logs/utilization.log")) {
    file.create("./logs/utilization.log")
    # set permisions so all users can write to the utilization log file
    Sys.chmod("./logs/utilization.log", mode = "0666", use_umask = FALSE)

    # add header record
    log_handle <- file("./logs/utilization.log")
    writeLines(c("UNIXID|SESSIONDTM|APP_DIR|TA|MOLECULE|INDICATION|ANL_TYPE|PKGS"), log_handle)
    close(log_handle)
  }

  # assign app usage data fields
  log_usage <- line_usage_log(ta, molecule, indication, anl_type) #nolint

  # retrieve and assign package metadata
  log_pkgs <- line_pkg_log(fields = pkg_meta)  #nolint

  # save usage and package metadata to log file as single record per session
  cat(paste(log_usage, log_pkgs, sep = "|"), file = "./logs/utilization.log", append = TRUE)
  cat("\n", file = "./logs/utilization.log", append = TRUE)
}

#' app usage data fields to add to log file
#'
#' @noRd
#' @examples
#' \dontrun{
#' teal.utils:::line_usage_log("Oncology", "Tecentriq", "NSCLC", "Exploratory")
#' }
line_usage_log <- function(...) {
  args <- c(...)

  if (!all(vapply(args, is.character, logical(1)))) {
    stop("all arguments of log_app_usage are required to be of type character")
  }

  has_pipe <- grepl("\\|", args)
  if (any(has_pipe)) {
    args_with_pipe <- names(args)[has_pipe]
    stop("the arguments ", paste(args_with_pipe, collapse = ","), " can not contain pipe character.")
  }

  paste(Sys.info()["user"], Sys.time(), getwd(), ..., sep = "|")
}

#' Package metadata to add to log file
#'
#' Metadata included in logs relates to loaded NEST packages
#' @param fields package metadata to be retrieved
#' @noRd
#'
#' @importFrom utils capture.output sessionInfo
#' @examples
#' \dontrun{
#' teal.utils:::line_pkg_log(pkgs = c("rtables", "tern", "teal"),
#' fields = c("Package", "Title", "Version", "RemoteRef"))
#' }
#' @importFrom utils sessionInfo
line_pkg_log <- function(fields) {
  nest_packages <- c(
    "test.nest", "utils.nest", "devtools.nest", "random.cdisc.data", "rtables",
    "tern", "teal", "teal.devel", "teal.modules.general", "teal.modules.clinical",
    "oosprey", "teal.osprey", "goshawk", "teal.goshawk", "tlgdown")
  pkg_desc <- sessionInfo()$otherPkgs
  pkg_desc <- lapply(pkg_desc, function(x) if (x$Package %in% nest_packages) x[fields] else NULL)
  pkg_desc <- Filter(Negate(is.null), pkg_desc)
  pkg_desc_no_pipe <- lapply(pkg_desc, function(x) sub("|", "/", x, fixed = TRUE))
  pkg_desc_save <- lapply(pkg_desc_no_pipe, setNames, fields)

  paste(capture.output(dput(pkg_desc_save, file = "")), collapse = "")
}
