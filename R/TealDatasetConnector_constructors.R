# RICE ====
#' Superseded (moved into separate package): Rice `TealDatasetConnector`
#'
#' `r lifecycle::badge("superseded")`
#'
#' Create a `TealDatasetConnector` from `RICE`.
#'
#' @inheritParams teal.data::dataset_connector
#'
#' @param path (`character`)\cr
#'   path to the file
#'
#' @param ... (`optional`)\cr
#'   additional arguments applied to pull function
#'
#' @export
#'
#' @rdname rice_dataset_connector
#'
rice_dataset_connector <- function(dataname,
                                   path,
                                   keys = character(0),
                                   label = character(0),
                                   code = character(0),
                                   script = character(0),
                                   ...) {
  lifecycle::deprecate_stop(
    when = "0.10.1",
    what = "teal::rice_dataset_connector()",
    details = "Please use teal.connectors.rice::rice_dataset_connector()."
  )
}

#' Superseded (moved into separate package): Rice `CDISCTealDatasetConnector`
#'
#' `r lifecycle::badge("superseded")`
#'
#' Create a `CDISCTealDatasetConnector` from `RICE` dataset with keys and parent name assigned
#' automatically by `dataname`.
#'
#' @inheritParams rice_dataset_connector
#' @inheritParams teal.data::cdisc_dataset_connector
#'
#' @rdname rice_dataset_connector
#'
#' @export
rice_cdisc_dataset_connector <- function(dataname,
                                         path,
                                         keys = get_cdisc_keys(dataname),
                                         parent = `if`(identical(dataname, "ADSL"), character(0L), "ADSL"),
                                         label = character(0),
                                         code = character(0),
                                         script = character(0),
                                         ...) {
  lifecycle::deprecate_stop(
    when = "0.10.1",
    what = "teal::rice_cdisc_dataset_connector()",
    details = "Please use teal.connectors.rice::rice_cdisc_dataset_connector()."
  )
}


# TERADATA ====
#' Superseded (moved into separate package): `Teradata` `TealDatasetConnector`
#'
#' `r lifecycle::badge("superseded")`
#'
#' Create a `TealDatasetConnector` from `Teradata`.
#'
#' @inheritParams teal.data::dataset_connector
#' @inheritParams teal.data::fun_dataset_connector
#' @param table (`character`) table name
#'
#' @rdname teradata_dataset_connector
#'
#' @export
teradata_dataset_connector <- function(dataname,
                                       table,
                                       keys = character(0),
                                       label = character(0),
                                       code = character(0),
                                       script = character(0),
                                       ...) {
  lifecycle::deprecate_stop(
    when = "0.10.1",
    what = "teal::teradata_dataset_connector()",
    details = "Please use teal.connectors.teradata::teradata_dataset_connector()."
  )
}

#' Superseded (moved into separate package): `Teradata` `CDISCTealDatasetConnector`
#'
#' `r lifecycle::badge("superseded")`
#'
#' Create a `CDISCTealDatasetConnector` from `Teradata` with keys and parent name assigned
#' automatically by `dataname`.
#'
#' @inheritParams teradata_dataset_connector
#' @inheritParams teal.data::cdisc_dataset_connector
#'
#' @rdname teradata_dataset_connector
#'
#' @export
teradata_cdisc_dataset_connector <- function(dataname, # nolint
                                             table,
                                             keys = get_cdisc_keys(dataname),
                                             parent = `if`(identical(dataname, "ADSL"), character(0L), "ADSL"),
                                             label = character(0),
                                             code = character(0),
                                             script = character(0),
                                             ...) {
  lifecycle::deprecate_stop(
    when = "0.10.1",
    what = "teal::teradata_cdisc_dataset_connector()",
    details = "Please use teal.connectors.teradata::teradata_cdisc_dataset_connector()."
  )
}


# SNOWFLAKE ====
#' Superseded (moved into separate package): `Snowflake` `TealDatasetConnector`
#'
#' `r lifecycle::badge("superseded")`
#'
#' Create a `TealDatasetConnector` from `Snowflake`.
#'
#' @inheritParams teal.data::dataset_connector
#' @inheritParams teal.data::fun_dataset_connector
#' @param sql_query (`character`) SQL statement to extract data from snowflake
#'
#' @rdname snowflake_dataset_connector
#'
#' @export
snowflake_dataset_connector <- function(dataname,
                                        sql_query,
                                        keys = character(0),
                                        label = character(0),
                                        code = character(0),
                                        script = character(0),
                                        ...) {
  lifecycle::deprecate_stop(
    when = "0.10.1",
    what = "teal::snowflake_dataset_connector()",
    details = "Please use teal.connectors.snowflake::snowflake_dataset_connector()."
  )
}

#' Superseded (moved into separate package): `Snowflake` `CDISCTealDatasetConnector`
#'
#' `r lifecycle::badge("superseded")`
#'
#' Create a `CDISCTealDatasetConnector` from `Snowflake` with keys and parent name assigned
#' automatically by `dataname`.
#'
#' @inheritParams snowflake_dataset_connector
#' @inheritParams teal.data::cdisc_dataset_connector
#'
#' @rdname snowflake_dataset_connector
#'
#' @export
snowflake_cdisc_dataset_connector <- function(dataname, # nolint
                                              sql_query,
                                              keys = get_cdisc_keys(dataname),
                                              parent = `if`(identical(dataname, "ADSL"), character(0L), "ADSL"),
                                              label = character(0),
                                              code = character(0),
                                              script = character(0),
                                              ...) {
  lifecycle::deprecate_stop(
    when = "0.10.1",
    what = "teal::snowflake_cdisc_dataset_connector()",
    details = "Please use teal.connectors.snowflake::snowflake_cdisc_dataset_connector()."
  )
}


# CDSE ====
#' Superseded (moved into separate package): `CDSE` `TealDatasetConnector`
#'
#' @description `r lifecycle::badge("superseded")`
#'
#' Create a `TealDatasetConnector` from `CDSE`.
#'
#' @inheritParams teal.data::dataset_connector
#' @inheritParams teal.data::fun_dataset_connector
#' @param cid (`character`) ID of dataset
#'
#' @export
#'
#' @rdname cdse_dataset_connector
cdse_dataset_connector <- function(dataname,
                                   cid,
                                   keys = character(0),
                                   label = character(0),
                                   code = character(0),
                                   script = character(0),
                                   ...) {
  lifecycle::deprecate_stop(
    when = "0.10.1",
    what = "teal::cdse_dataset_connector()",
    details = "Please use teal.connectors.cdse::cdse_dataset_connector()."
  )
}

#' Superseded (moved into separate package): `CDSE` `CDISCTealDatasetConnector`
#'
#' `r lifecycle::badge("superseded")`
#'
#' Create a `CDISCTealDatasetConnector` from `CDSE` with keys and parent name assigned
#' automatically by `dataname`.
#'
#' @inheritParams cdse_dataset_connector
#' @inheritParams teal.data::cdisc_dataset_connector
#'
#' @rdname cdse_dataset_connector
#'
#' @export
cdse_cdisc_dataset_connector <- function(dataname,
                                         cid,
                                         keys = get_cdisc_keys(dataname),
                                         parent = `if`(identical(dataname, "ADSL"), character(0L), "ADSL"),
                                         label = character(0),
                                         code = character(0),
                                         script = character(0),
                                         ...) {
  lifecycle::deprecate_stop(
    when = "0.10.1",
    what = "teal::cdse_cdisc_dataset_connector()",
    details = "Please use teal.connectors.cdse::cdse_cdisc_dataset_connector()."
  )
}

# DataSetDB ====
#' Superseded (moved into separate package): `DataSetDB` `TealDatasetConnector`
#'
#' `r lifecycle::badge("superseded")`
#'
#' Create a `TealDatasetConnector` from `DataSetDB`.
#'
#' @inheritParams teal.data::dataset_connector
#'
#' @param id (`character`)\cr
#'   identifier of the dataset in `DataSetDB`
#'
#' @param ... (`optional`)\cr
#'   additional arguments applied to pull function
#'
#' @export
datasetdb_dataset_connector <- function(dataname,
                                        id,
                                        keys = character(0),
                                        label = character(0),
                                        code = character(0),
                                        script = character(0),
                                        ...) {
  lifecycle::deprecate_stop(
    when = "0.10.1",
    what = "teal::datasetdb_dataset_connector()",
    details = "Please use teal.connectors.datasetdb::datasetdb_dataset_connector()."
  )
}
