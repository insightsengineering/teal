#' `TealDataConnector` connector for `RICE`
#'
#' @description `r lifecycle::badge("defunct")`
#' Build data connector for `RICE` datasets
#'
#' @export
#'
#' @param ... (`TealDatasetConnector` objects)\cr
#'  dataset connectors created using `rice_dataset_connector`
#' @param connection (`TealDataConnection`) object returned from `rice_connection`.
#' @param additional_ui (`shiny.tag`)\cr
#'  additional user interface to be visible over login panel
#'
#' @return An object of class `TealDataConnector`
#'
rice_data <- function(..., connection = rice_connection(), additional_ui = NULL) {
  lifecycle::deprecate_stop(
    when = "0.10.1",
    what = "teal::rice_data()",
    details = "Please use teal.connectors.rice::rice_data()."
  )
}

#' `TealDataConnector` connector for `TERADATA`
#'
#' @description `r lifecycle::badge("defunct")`
#' Build data connector for `TERADATA` functions or datasets
#'
#' @export
#'
#' @param ... (`TealDatasetConnector`) dataset connectors created using \code{\link{teradata_dataset_connector}}
#' @param connection (`TealDataConnection`) object returned from `teradata_connection`.
#'
#' @return An object of class `TealDataConnector`
teradata_data <- function(..., connection = teradata_connection()) {
  lifecycle::deprecate_stop(
    when = "0.10.1",
    what = "teal::teradata_data()",
    details = "Please use teal.connectors.teradata::teradata_data()."
  )
}

#' `TealDataConnector` connector for `SNOWFLAKE`
#'
#' @description `r lifecycle::badge("defunct")`
#' Build data connector for `SNOWFLAKE` functions or datasets
#'
#' @export
#'
#' @param ... (`TealDatasetConnector`) dataset connectors created using \code{\link{snowflake_dataset_connector}}
#' @param connection (`TealDataConnection`) object returned from `snowflake_connection`.
#'
#' @return An object of class `TealDataConnector`
#'
#' @details Note the server location and token_provider must be provided as arguments to
#'   the snowflake_connection function, see example below.
#' @export
snowflake_data <- function(..., connection) {
  lifecycle::deprecate_stop(
    when = "0.10.1",
    what = "teal::snowflake_data()",
    details = "Please use teal.connectors.snowflake::snowflake_data()."
  )
}

#' `TealDataConnector` connector for `CDSE`
#'
#' @description `r lifecycle::badge("defunct")`
#' Build data connector for `CDSE` datasets
#'
#' @export
#'
#' @param ... (`TealDatasetConnector` objects)\cr
#'  dataset connectors created using \code{\link{cdse_dataset_connector}}
#' @param connection (`TealDataConnection`) object returned from `cdse_connection`.
#'
#' @return An object of class `TealDataConnector`
cdse_data <- function(..., connection = cdse_connection()) {
  lifecycle::deprecate_stop(
    when = "0.10.1",
    what = "teal::cdse_data()",
    details = "Please use teal.connectors.cdse::cdse_data()."
  )
}

#' `TealDataConnector` connector for `DataSetDB`
#'
#' @description `r lifecycle::badge("defunct")`
#' Build data connector for `DataSetDB` datasets
#'
#' @export
#'
#' @param ... (`TealDatasetConnector` objects)\cr
#'  dataset connectors created using \code{\link{datasetdb_dataset_connector}}
#' @param connection (`TealDataConnection`) object returned from `datasetdb_connection`.
#'
#' @return An object of class `TealDataConnector`
#'
datasetdb_data <- function(..., connection = datasetdb_connection()) {
  lifecycle::deprecate_stop(
    when = "0.10.1",
    what = "teal::datasetdb_data()",
    details = "Please use teal.connectors.datasetdb::datasetdb_data()."
  )
}
