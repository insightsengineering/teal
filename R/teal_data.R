#' Teal data
#'
#' Universal function to pass data to teal application
#'
#' @export
#'
#' @param ... (\code{RelationalData}, \code{RelationalDataConnector}, \code{RelationalDataset} or
#'   \code{RelationalDatasetConnector}) elements to include into teal data object
#'
#' @return \code{RelationalData} if all of the elements are of  \code{RelationalDataset} class, else
#'   \code{RelationalDataList}
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
#' data <- teal_data(x1, x2)
#' data$get_cdisc_data()
#'
#' # DelayedDataConnector
#' x3 <- rcd_cdisc_data( # RelationalDataConnector
#'   rcd_cdisc_dataset_connector("ADSL", radsl, cached = TRUE),
#'   rcd_cdisc_dataset_connector("ADLB", radlb, cached = TRUE)
#' )
#'
#' x4 <- rcd_cdisc_data( # RelationalDataConnector
#'   rcd_cdisc_dataset_connector("ADRS", radrs, cached = TRUE)
#' )
#'
#' delayed_data <- teal_data(x1, x2, x3, x4)
#' \dontrun{
#' delayed_data$launch()
#' delayed_data$get_cdisc_data()
#' }
teal_data <- function(...) {
  datasets <- list(...)
  possible_classes <- c("RelationalDataConnector", "RelationalDataset", "RelationalDatasetConnector")

  d <- list(...)

  is_teal_data <- is_any_class_list(datasets, possible_classes)
  if (!all(is_teal_data)) {
    stop("All arguments should be of RelationalData(set) or RelationalData(set)Connector class")
  }

  teal_data <- if (all(vapply(d, FUN = is, FUN.VALUE = logical(1), class2 = "RelationalDataset"))) {
    RelationalData$new(...)
  } else {
    RelationalDataList$new(...)
  }

  return(teal_data)
}


all_relational_dataset <- function(x) {
  all(
    vapply(x, FUN = is, FUN.VALUE = logical(1), class2 = "RelationalDataset")
  )
}

#' Load \code{RelationalData} object from a file
#'
#' @param x A (\code{connection}) or a (\code{character}) string giving the pathname
#'   of the file or URL to read from. "" indicates the connection \code{stdin}.
#'
#' @return \code{RelationalData} object if file returns a \code{RelationalData}
#'   object.
#' @export
#'
#' @examples
#' file_example <- tempfile(fileext = ".R")
#' writeLines(
#'   text = c(
#'     "library(teal)
#'      library(random.cdisc.data)
#'      library(dplyr)
#'
#'      adsl <- cdisc_dataset(dataname = \"ADSL\", # RelationalDataset
#'                            data = radsl(cached = TRUE),
#'                            code = \"library(random.cdisc.data)\nADSL <- radsl(cached = TRUE)\")
#'
#'      adtte <- cdisc_dataset(dataname = \"ADTTE\", # RelationalDataset
#'                             data = radtte(cached = TRUE),
#'                             code = \"library(random.cdisc.data)\nADTTE <- radtte(cached = TRUE)\")
#'
#'      teal_data(adsl, adtte)"
#'   ),
#'   con = file_example
#' )
#'
#' teal_data_file(file_example)
#'
#' @importFrom methods is
teal_data_file <- function(x) {

  code <- readLines(x)
  object <- eval(parse(text = code))

  if (is(object, "RelationalData")) {
    return(object)
  } else {
    stop("The object returned from the file is not a RelationalData object.")
  }
}
