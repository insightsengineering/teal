#' Teal application usage logging.
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
#'\dontrun{
#' log_app_usage(ta = "Oncology", molecule = "Tecentriq", ind = "NSCLC", anl_type = "Exploratory")
#' }
#' 
log_app_usage <- function(ta = NULL,
                          molecule = NULL,
                          indication = NULL,
                          anl_type = NULL,
                          pkg_meta = c("Package", "Title", "Version", "RemoteRef")) {
  
  # collect argument values into list for verification
  args <- as.list(environment())

  # # BEGIN verify requirements
  if (!dir.exists("./libs")){
    stop(cat(match.call()[[1]], "function exception: <your app>/libs directory does not exist.\n",
             "Please install R Packages required for your app in ./libs. Required by this function to log package 
             metadata."))
    # processing stops here
  }
  for (n in 1:length(args)){
    if (args[n] == "NULL" | args[n] == ""){
      stop(paste(match.call()[[1]], "function exception: Argument values can not be NULL.\n",
                 "Exception is:", args[n]))
      # processing stops here
    }
  }
  for (p in 1:length(args)){
    if (grepl("\\|", args[p])){
      stop(paste(match.call()[[1]], "function exception: argument values can not contain pipe character.\n",
                 "Exception is:", args[p]))
    # processing stops here
    }
  }
  # END verify requirement

    # log user activity
    # conditionally create logs directory
    dir.exists("./logs") || dir.create("./logs")
    # set permisions so all users can write to the utilization log directory
    Sys.chmod("./logs", mode = "0777", use_umask = FALSE)
    
    # conditionally initialize log file
    if (!file.exists("./logs/utilization.log")){
      file.create("./logs/utilization.log")
      # set permisions so all users can write to the utilization log file
      Sys.chmod("./logs/utilization.log", mode = "0666", use_umask = FALSE)
      
      # add header record      
      logHandle <- file("./logs/utilization.log")
      writeLines(c("UNIXID|SESSIONDTM|APP_DIR|TA|MOLECULE|INDICATION|ANL_TYPE|VALUE_NAME|VALUE_CAT|VALUE|"),
                 logHandle)
      close(logHandle)
    }
    
    # get packages installed with app
    app_packages <- system("ls ./libs", intern = TRUE)

    # initialize the description collector and collect metadata for all packages 
    pkg_desc <- NULL
    for (i in 1:length(app_packages)){
      pkg_desc <- append(pkg_desc, packageDescription(app_packages[i], fields = pkg_meta))
    }
    
    # create data frame to output normalized values to log file
    pkg_cat_value <- data.frame(VALUE_CAT = names(pkg_desc), VALUE = unlist(pkg_desc, use.names = FALSE))
    
    # identify package names to add to every record. this allows each metatdata record to be associated to a package.
    pkg_names <- pkg_cat_value %>%
      subset(VALUE_CAT == "Package") %>%
      mutate(package = VALUE) %>%
      select(package)
    VALUE_NAME <- pkg_names[rep(seq_len(nrow(pkg_names)), each=length(pkg_meta)),]
    
    # create metadata string to write to log
    to_log <- cbind(VALUE_NAME, pkg_cat_value) %>% 
      mutate(TO_LOG = paste0(VALUE_NAME, "|", VALUE_CAT, "|", VALUE))
    
    
    # log app user activity and meta data
    for (i in 1:nrow(to_log)){
      cat(paste(Sys.info()['user'], Sys.time(), getwd(), ta, molecule, indication, anl_type,
                to_log[i, 4:4], "\n",
                sep = "|"), file="./logs/utilization.log", append=TRUE)
    }
}
