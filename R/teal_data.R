#' Teal data
#'
#' Universal function to pass data to teal application
#'
#' @export
#'
#' @param ... (\code{RelationalData}, \code{RelationalDataConnector}, \code{RelationalDataset} or
#'   \code{RelationalDatasetConnector}) elements to include into teal data object
#'
#' @return \code{RelationalDataList}
#'
#' @examples
#' # RelationalData
#' library(random.cdisc.data)
#' x1 <- relational_dataset(
#'   x = radsl(cached = TRUE),
#'   dataname = "ADSL",
#'   keys = get_cdisc_keys("ADSL"),
#'   code = "ADSL <- radsl(cached = TRUE)",
#'   label = "ADTTE dataset"
#' )
#'
#' x2 <- relational_dataset(
#'   x = radtte(cached = TRUE),
#'   dataname = "ADTTE",
#'   keys = get_cdisc_keys("ADTTE"),
#'   code = "ADTTE <- radtte(cached = TRUE)",
#'   label = "ADTTE dataset"
#' )
#'
#' data <- cdisc_data(x1, x2)
#' get_raw_data(data)
#'
#' # RelationalDataList
#' x3 <- rcd_data( # RelationalDataConnector
#'   rcd_cdisc_dataset_connector("ADSL", radsl, cached = TRUE),
#'   rcd_cdisc_dataset_connector("ADLB", radlb, cached = TRUE)
#' )
#'
#' x4 <- rcd_data( # RelationalDataConnector
#'   rcd_cdisc_dataset_connector("ADRS", radrs, cached = TRUE)
#' )
#'
#' data_list <- cdisc_data(x3, x2, x4)
#' \dontrun{
#' data_list$launch()
#' get_raw_data(data_list)
#' }
teal_data <- function(...) {
  datasets <- list(...)
  possible_classes <- c("RelationalData", "RelationalDataConnector", "RelationalDataset", "RelationalDatasetConnector")

  is_teal_data <- is_any_class_list(datasets, possible_classes)
  if (!all(is_teal_data)) {
    stop("All arguments should be of RelationalData(set) or RelationalData(set)Connector class")
  }

  teal_data <- RelationalDataList$new(...)


  return(teal_data)
}


#' Load \code{RelationalData} object from a file
#'
#' Please note that the script has to end with a call creating desired object. The error will be raised otherwise.
#'
#' @param x (\code{character}) string giving the pathname of the file to read from.
#' @param code (\code{character}) reproducible code to re-create object
#'
#' @return \code{RelationalData} object
#'
#' @importFrom methods is
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
#'                            data = radsl(cached = TRUE),
#'                            code = \"library(random.cdisc.data)\nADSL <- radsl(cached = TRUE)\")
#'
#'      adtte <- cdisc_dataset(dataname = \"ADTTE\",
#'                             data = radtte(cached = TRUE),
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
#'     ADSL <- cdisc_dataset(dataname = \"ADSL\", data = ADSL)
#'     ADTTE <- cdisc_dataset(dataname = \"ADTTE\", data = ADTTE)
#'
#'     cdisc_data(ADSL, ADTTE)"
#'   ),
#'   con = file_example
#' )
#' x <- teal_data_file(file_example)
#' get_code(x)
teal_data_file <- function(x, code = get_code(x)) {
  stopifnot(is_character_single(x))
  stopifnot(file.exists(x))

  lines <- paste0(readLines(x), collapse = "\n")
  object <- eval(parse(text = lines))

  if (is(object, "RelationalDataList")) {
    object$mutate(code)
    return(object)
  } else {
    stop("The object returned from the file is not RelationalDataList object.")
  }
}
