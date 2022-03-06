#' Superseded (moved into separate package): `TealDataConnector` connector for `RICE`
#'
#' @description `r lifecycle::badge("superseded")`
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
#' @importFrom lifecycle deprecate_stop
#'
#' @return An object of class `TealDataConnector`
#'
rice_data <- function(..., connection = rice_connection(), additional_ui = NULL) {
  lifecycle::deprecate_stop(
    when = "0.10.1",
    what = "rice_data()",
    details = "Please use teal.connectors.rice::rice_data()."
  )
}

#' Superseded (moved into separate package): `TealDataConnector` connector for `TERADATA`
#'
#' @description `r lifecycle::badge("superseded")`
#' Build data connector for `TERADATA` functions or datasets
#'
#' @export
#'
#' @param ... (`TealDatasetConnector`) dataset connectors created using [teradata_dataset_connector]
#' @param connection (`TealDataConnection`) object returned from `teradata_connection`.
#'
#' @return An object of class `TealDataConnector`
teradata_data <- function(..., connection = teradata_connection()) {
  lifecycle::deprecate_stop(
    when = "0.10.1",
    what = "teradata_data()",
    details = "Please use teal.connectors.teradata::teradata_data()."
  )
}

#' Superseded (moved into separate package): `TealDataConnector` connector for `SNOWFLAKE`
#'
#' @description `r lifecycle::badge("superseded")`
#' Build data connector for `SNOWFLAKE` functions or datasets
#'
#' @export
#'
#' @param ... (`TealDatasetConnector`) dataset connectors created using [snowflake_dataset_connector]
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
    what = "snowflake_data()",
    details = "Please use teal.connectors.snowflake::snowflake_data()."
  )
}

#' Superseded (moved into separate package): `TealDataConnector` connector for `CDSE`
#'
#' @description `r lifecycle::badge("superseded")`
#' Build data connector for `CDSE` datasets
#'
#' @export
#'
#' @param ... (`TealDatasetConnector` objects)\cr
#'  dataset connectors created using [cdse_dataset_connector]
#' @param connection (`TealDataConnection`) object returned from `cdse_connection`.
#'
#' @return An object of class `TealDataConnector`
cdse_data <- function(..., connection = cdse_connection()) {
  lifecycle::deprecate_stop(
    when = "0.10.1",
    what = "cdse_data()",
    details = "Please use teal.connectors.cdse::cdse_data()."
  )
}

#' Superseded (moved into separate package): `TealDataConnector` connector for `DataSetDB`
#'
#' @description `r lifecycle::badge("superseded")`
#' Build data connector for `DataSetDB` datasets
#'
#' @export
#'
#' @param ... (`TealDatasetConnector` objects)\cr
#'  dataset connectors created using [datasetdb_dataset_connector]
#' @param connection (`TealDataConnection`) object returned from `datasetdb_connection`.
#'
#' @return An object of class `TealDataConnector`
#'
datasetdb_data <- function(..., connection = datasetdb_connection()) {
  lifecycle::deprecate_stop(
    when = "0.10.1",
    what = "datasetdb_data()",
    details = "Please use teal.connectors.datasetdb::datasetdb_data()."
  )
}
