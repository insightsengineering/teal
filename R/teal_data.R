#' Teal data
#'
#' Universal function to pass data to teal application
#' @param ... (\code{RelationalData, RelationalDataConnector,
#' RelationalDataset or RelationalDatasetConnector})\cr
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
#' @return cdisc_data
#' @export
teal_data <- function(...) {
  datasets <- list(...)
  delayed_classes <- c("RelationalDataConnector", "RelationalDataset", "RelationalDatasetConnector")

  d <- list(...)

  is_teal_data <- is_any_class_list(datasets, delayed_classes)
  if (!all(is_teal_data)) {
    stop("All arguments should be RelationalData(set) or RelationalData(set)Connector")
  }

  if (all_relational_dataset(d)) {
    teal_data <- RelationalData$new(...)
  } else {
    teal_data <- DelayedRelationalData$new(...)
  }

  return(teal_data)
}


all_relational_dataset <- function(x) {
  all(
    vapply(x, FUN = is, FUN.VALUE = logical(1), class2 = "RelationalDataset")
  )
}
