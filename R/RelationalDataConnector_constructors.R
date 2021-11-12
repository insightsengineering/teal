#' \code{RelationalDataConnector} connector for \code{RICE}
#'
#' @description `r lifecycle::badge("defunct")`
#' Build data connector for \code{RICE} datasets
#'
#' @export
#'
#' @param ... (\code{DatasetConnector} objects)\cr
#'  dataset connectors created using \code{rice_dataset_connector}
#' @param connection (\code{DataConnection}) object returned from \code{rice_connection}.
#' @param additional_ui (\code{shiny.tag})\cr
#'  additional user interface to be visible over login panel
#'
#' @return An object of class \code{RelationalDataConnector}
#'
rice_data <- function(..., connection = rice_connection(), additional_ui = NULL) {
  lifecycle::deprecate_stop(
    when = "0.10.1",
    what = "teal::rice_data()",
    details = "Please use teal.connectors.rice::rice_data()."
  )
}

#' \code{RelationalDataConnector} connector for \code{TERADATA}
#'
#' @description `r lifecycle::badge("defunct")`
#' Build data connector for \code{TERADATA} functions or datasets
#'
#' @export
#'
#' @param ... (\code{DatasetConnector}) dataset connectors created using \code{\link{teradata_dataset_connector}}
#' @param connection (\code{DataConnection}) object returned from \code{teradata_connection}.
#'
#' @return An object of class \code{RelationalDataConnector}
teradata_data <- function(..., connection = teradata_connection()) {
  lifecycle::deprecate_stop(
    when = "0.10.1",
    what = "teal::teradata_data()",
    details = "Please use teal.connectors.teradata::teradata_data()."
  )
}

#' \code{RelationalDataConnector} connector for \code{SNOWFLAKE}
#'
#' @description `r lifecycle::badge("defunct")`
#' Build data connector for \code{SNOWFLAKE} functions or datasets
#'
#' @export
#'
#' @param ... (\code{DatasetConnector}) dataset connectors created using \code{\link{snowflake_dataset_connector}}
#' @param connection (\code{DataConnection}) object returned from \code{snowflake_connection}.
#'
#' @return An object of class \code{RelationalDataConnector}
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

#' \code{RelationalDataConnector} connector for \code{CDSE}
#'
#' @description `r lifecycle::badge("defunct")`
#' Build data connector for \code{CDSE} datasets
#'
#' @export
#'
#' @param ... (\code{DatasetConnector} objects)\cr
#'  dataset connectors created using \code{\link{cdse_dataset_connector}}
#' @param connection (\code{DataConnection}) object returned from \code{cdse_connection}.
#'
#' @return An object of class \code{RelationalDataConnector}
cdse_data <- function(..., connection = cdse_connection()) {
  lifecycle::deprecate_stop(
    when = "0.10.1",
    what = "teal::cdse_data()",
    details = "Please use teal.connectors.cdse::cdse_data()."
  )
}

#' \code{RelationalDataConnector} connector for \code{DataSetDB}
#'
#' @description `r lifecycle::badge("defunct")`
#' Build data connector for \code{DataSetDB} datasets
#'
#' @export
#'
#' @param ... (\code{DatasetConnector} objects)\cr
#'  dataset connectors created using \code{\link{datasetdb_dataset_connector}}
#' @param connection (\code{DataConnection}) object returned from \code{datasetdb_connection}.
#'
#' @return An object of class \code{RelationalDataConnector}
#'
datasetdb_data <- function(..., connection = datasetdb_connection()) {
  lifecycle::deprecate_stop(
    when = "0.10.1",
    what = "teal::datasetdb_data()",
    details = "Please use teal.connectors.datasetdb::datasetdb_data()."
  )
}
