#' Teal data
#'
#' @description `r lifecycle::badge("experimental")`
#' Universal function to pass data to teal application
#'
#' @param ... (`RelationalDataConnector`, `Dataset`, `DatasetConnector`)\cr
#'   objects
#' @param join_keys (`JoinKeys`) or a single (`JoinKeySet`)\cr
#'   (optional) object with dataset column relationships used for joining.
#'   If empty then no joins between pairs of objects
#'
#' @return (\code{RelationalData})
#'
#' @export
#'
#' @examples
#' # RelationalData
#' library(random.cdisc.data)
#' x1 <- dataset(
#'   x = radsl(cached = TRUE),
#'   dataname = "ADSL",
#'   keys = get_cdisc_keys("ADSL"),
#'   code = "ADSL <- radsl(cached = TRUE)",
#'   label = "ADTTE dataset"
#' )
#'
#' x2 <- dataset(
#'   x = radtte(cached = TRUE),
#'   dataname = "ADTTE",
#'   keys = get_cdisc_keys("ADTTE"),
#'   code = "ADTTE <- radtte(cached = TRUE)",
#'   label = "ADTTE dataset"
#' )
#'
#' data <- teal_data(x1, x2)
#' get_raw_data(data)
#'
#' # RelationalData with connectors
#' x3 <- rcd_data(
#'   rcd_cdisc_dataset_connector("ADSL", radsl, cached = TRUE),
#'   rcd_cdisc_dataset_connector("ADLB", radlb, cached = TRUE)
#' )
#'
#' x4 <- rcd_data(
#'   rcd_cdisc_dataset_connector("ADRS", radrs, cached = TRUE)
#' )
#'
#' data_list <- teal_data(x3, x2, x4)
#' \dontrun{
#' data_list$launch()
#' get_raw_data(data_list)
#' }
teal_data <- function(..., join_keys) {
  RelationalData$new(..., join_keys = join_keys)
}


#' Load \code{RelationalData} object from a file
#'
#' @description `r lifecycle::badge("experimental")`
#' Please note that the script has to end with a call creating desired object. The error will be raised otherwise.
#'
#' @param path A (`connection`) or a (`character`)\cr
#'   string giving the pathname of the file or URL to read from. "" indicates the connection `stdin`.
#' @param code (`character`)\cr
#'   reproducible code to re-create object
#'
#' @return \code{RelationalData} object
#'
#'
#' @export
#'
#' @examples
#' # simple example
#' file_example <- tempfile(fileext = ".R")
#' writeLines(
#'   text = c(
#'     "library(teal)
#'      library(random.cdisc.data)
#'
#'      adsl <- cdisc_dataset(dataname = \"ADSL\",
#'                            x = radsl(cached = TRUE),
#'                            code = \"library(random.cdisc.data)\nADSL <- radsl(cached = TRUE)\")
#'
#'      adtte <- cdisc_dataset(dataname = \"ADTTE\",
#'                             x = radtte(cached = TRUE),
#'                             code = \"library(random.cdisc.data)\nADTTE <- radtte(cached = TRUE)\")
#'
#'      cdisc_data(adsl, adtte)"
#'   ),
#'   con = file_example
#' )
#' x <- teal_data_file(file_example, code = character(0))
#' get_code(x)
#'
#' # exaample with custom datasets
#' file_example <- tempfile(fileext = ".R")
#' writeLines(
#'   text = c(
#'     "library(teal)
#'     library(random.cdisc.data)
#'
#'     # code>
#'     ADSL <- radsl(cached = TRUE)
#'     ADTTE <- radtte(cached = TRUE)
#'
#'     ADSL$n <- nrow(ADTTE)
#'     ADTTE$n <- nrow(ADSL)
#'     # <code
#'
#'     ADSL <- cdisc_dataset(dataname = \"ADSL\", x = ADSL)
#'     ADTTE <- cdisc_dataset(dataname = \"ADTTE\", x = ADTTE)
#'
#'     cdisc_data(ADSL, ADTTE)"
#'   ),
#'   con = file_example
#' )
#' x <- teal_data_file(file_example)
#' get_code(x)
teal_data_file <- function(path, code = get_code(path)) {
  object <- object_file(path, "RelationalData")
  object$mutate(code)
  return(object)
}
