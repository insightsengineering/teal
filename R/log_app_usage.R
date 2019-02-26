#' Teal Application Usage Logging.
#'
#' Analysis of Teal application utilization is a key component towards ongoing enhancements
#' of the framework. The utilization logs provide source data for better understanding
#' frequency and extent of use.
#'
#' @details Each Teal application can implement usage logging by calling this
#' function in the application startup file(s) like app.R. The following data are 
#' captured by default: UNIX ID, System Date, Application Directory, Therapeutic Area, Indication, Package Name,
#' Package Title, Package Version and Remotes Definition. Capture frequency is currently per user at session
#' start only.
#' Suggested values for ta: "Oncology" or "I2ON"
#' Suggested values for anl_type: "Exploratory", "Interim Analysis", "CSR"
#'
#' @param ta therapeutic area name. required argument. e.g. "I2ON".
#' @param molecule molecule name. required argument. e.g. "Lucentis".
#' @param indication indication. required argument. e.g. "AMD".
#' @param anl_type analysis type. required argument. e.g. "Interim Analysis".
#' @param pkg_meta package metadata fields. default values: c("Package", "Title", "Version", "RemoteRef").
#'
#' @import dplyr
#' @importFrom utils packageDescription
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' wd <- getwd()
#' d <- tempfile(); dir.create(d); dir.create(file.path(d, "libs"))
#' lapply(file.path(d, "libs", c("rtables", "tern", "teal")), dir.create)
#' setwd(d)
#' log_app_usage(ta = "Oncology", molecule = "Tecentriq", ind = "NSCLC", anl_type = "Exploratory")
#' readLines(file.path(d, "logs", "utilization.log"))
#' setwd(wd)
#' unlink(d, recursive = TRUE)
#' }
#' 
log_app_usage <- function(ta,
                          molecule,
                          indication,
                          anl_type,
                          pkg_meta = c("Package", "Title", "Version", "RemoteRef")) {
  

  if (!dir.exists("./libs")) {
    stop("<your app dir>/libs directory does not exist.\n",
         "Please install R Packages required for your app in ./libs.",
         "This is required by this function to log package metadata.")
  }
  
  
  # get packages installed with app
  app_packages <- list.dirs("./libs", full.names = FALSE, recursive = FALSE)
  log_pkgs <- line_pkg_log(app_packages, fields = pkg_meta)
  
  log_usage <- line_usage_log(ta, molecule, indication, anl_type)
  
  # Save log to file

  # conditionally create logs directory
  if (!dir.exists("./logs")) {
    dir.create("./logs")
    # set permisions so all users can write to the utilization log directory
    Sys.chmod("./logs", mode = "0777", use_umask = FALSE)
  } 
  
  # conditionally initialize log file
  if (!file.exists("./logs/utilization.log")){
    file.create("./logs/utilization.log")
    # set permisions so all users can write to the utilization log file
    Sys.chmod("./logs/utilization.log", mode = "0666", use_umask = FALSE)
    
    # add header record      
    logHandle <- file("./logs/utilization.log")
    writeLines(c("UNIXID|SESSIONDTM|APP_DIR|TA|MOLECULE|INDICATION|ANL_TYPE|PKGS"),
               logHandle)
    close(logHandle)
  }
  

  cat(paste(log_usage, log_pkgs, sep = "|"), file="./logs/utilization.log", append=TRUE)

}



#' usage log line
#' 
#' @examples 
#' 
#' \dontrun{
#' line_usage_log("AAA", "BBB", "CCC")
#' }
line_usage_log <- function(...) {
  args <- list(...)
  
  if (!all(vapply(args, is.character, logical(1)))) {
    stop("all arguments of log_app_usage are required to be of type character")
  }
  
  has_pipe <- grepl("\\|", args)
  if (any(has_pipe)) {
    args_with_pipe <- names(args)[has_pipe]
    stop("the arguments ", paste(args_with_pipe, collapse = ","), " can not contain pipe character.")
  }
  
  paste(Sys.info()['user'], Sys.time(), getwd(), ..., sep = "|")
}

#' package versions line
#' 
#' @examples 
#' \dontrun{
#' line_pkg_log(pkgs = c("rtables", "tern", "teal"), fields = c("Package", "Title", "Version", "RemoteRef") )
#' }
line_pkg_log <- function(pkgs, fields) {
  pkg_desc <- lapply(pkgs, packageDescription, fields = fields)
  pkg_desc_no_pipe <- lapply(pkg_desc, function(x) sub("|", "/", x, fixed = TRUE))
  pkg_desc_save <- lapply(pkg_desc_no_pipe, setNames, fields)
  
  paste(capture.output(dput(pkg_desc_save, file = "")), collapse = "")
}
