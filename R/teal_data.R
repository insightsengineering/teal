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
#'   \code{DelayedRelationalData}
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
    DelayedRelationalData$new(...)
  }

  return(teal_data)
}
