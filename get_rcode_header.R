
#' Used to get the header for "Show R-Code"
#' 
#' 
#' @importFrom utils packageDescription
#' @noRd
#' 
#' @examples 
#' 
#' \dontrun{
#' library(random.cdisc.data)
#' 
#' ASL <- radam('ASL')
#' ATE <- radam('ATE')
#' 
#' attr(ASL, "source") <- "radam('ASL')"
#' attr(ATE, "source") <- "radam('ATE')"
#' 
#' d <- teal:::FilteredData$new()
#' d$set_data("ASL", ASL)
#' d$set_data("ATE", ATE)
#' 
#' cat(teal.tern:::get_rcode_header(
#'   title = "Hello World\nsubtitle",
#'   dataname = c("ATE"),
#'   datasets = d
#' )); cat("\n")
#' 
#' }
#' 
get_rcode_header <- function(title, datanames, datasets, code_data_processing = NULL) {

  datanames <- c("ASL", setdiff(datanames, "ASL"))
  
  datanames_import <- if (is.null(code_data_processing)) {
    datanames
  } else {
    datasets$datanames() # because the code_data_processing might have references to all datasets
  }
  
  data <- lapply(datanames_import, function(x)datasets$get_data(x, reactive = FALSE, filtered=FALSE))
  names(data) <- datanames_import
  
  comment <- function(txt) {
    paste0("# ", gsub("\n", "\n# ", txt, fixed = TRUE))  
  }
  
  if (!has_source_attribute(data)) {
    "# not all datasets have the 'source' attribute, please contact the app maintainer"
  } else {
    
    info <- Sys.info()
    
    txt_location <- if (info['nodename'] == 'rkaub00459.kau.roche.com') {
      sub("/opt/bee_tools/shiny/",  "https://shiny.roche.com/", getwd(), fixed = TRUE)
    } else {
      getwd()
    }
    
    

    

    
    
    txt_data <- paste(
      unlist(Map(function(x, name) {
        txt <- paste(name, "<-", attr(x, "source"))
        md5 <- attr(x, "md5sum")
        if (is.null(md5)) txt else paste(txt, "# md5sum:", md5)
      }, data, names(data))),
      collapse = "\n"
    )
    
    txt_data_processing <- if (is.null(code_data_processing)) {
      ""
    } else {
      paste(c(code_data_processing, ""), collapse = "\n")
    }
    
    txt_filter <- teal::get_filter_txt(datanames, datasets)    
    
    
    ## header
    txt_inst_pkgs <- c(
      paste0('devtools::install_github("Roche/rtables", ref="v',
             packageDescription("rtables")$Version,'")'),
      paste0('devtools::install_github("Rpackages/tern", ref="v',
             packageDescription("tern")$Version,'", host="https://github.roche.com/api/v3")')
    )
    
    needs_rcd <- any(grepl("radam\\(", txt_data))
    if (needs_rcd) {
      txt_inst_pkgs <- c(
        paste0('devtools::install_github("Rpackages/random.cdisc.data", ref="v',
               packageDescription("random.cdisc.data")$Version,'", host="https://github.roche.com/api/v3")'),
        txt_inst_pkgs
      )
    }  

    
    txt_comment <- paste(c(
      title,
      "",
      paste("Output Created on", format(Sys.time(), "%b %d, %Y"), "by", info['user']),
      paste("with teal app under", txt_location), 
      "",
      paste(R.version$version.string, "on", info['nodename']), 
      "",
      txt_inst_pkgs
    ), collapse = "\n")
    
    
    paste(
      comment(txt_comment),
      "",
      "library(tern)",
      if (needs_rcd) "library(random.cdisc.data)" else NULL,
      "",
      txt_data,
      "",
      txt_data_processing,
      txt_filter,
      "",
      sep = "\n"
    )
  }
  


}


has_source_attribute <- function(x) {
  if (is.data.frame(x)) x <- list(x)
  all(vapply(x, function(dat) !is.null(attr(dat, "source")), logical(1)))
}
