
# TealDataConnection wrappers ----
#' Superseded (moved into separate package): Open connection to `entimICE` via `rice`
#'
#' @description `r lifecycle::badge("superseded")`
#'
#' @param open_args optional, named (`list`) of additional parameters for the connection's
#'   `rice_session_open` open function. Please note that the `password` argument will be
#'   overwritten with `askpass::askpass`.
#' @param close_args optional, named (`list`) of additional parameters for the connection's
#'   `rice_session_close` close function. Please note that the `message` argument
#'   will be overwritten with `FALSE`.
#' @param ping_args optional, named (`list`) of additional parameters for the connection's
#'   `rice_session_active` ping function.
#'
#' @return (`TealDataConnection`) type of object
#'
#' @export
rice_connection <- function(open_args = list(), close_args = list(), ping_args = list()) {
  lifecycle::deprecate_stop(
    when = "0.10.1",
    what = "rice_connection()",
    details = "Please use teal.connectors.rice::rice_connection()."
  )
}

#' Superseded (moved into separate package) Open connection to `Teradata`
#'
#' @description `r lifecycle::badge("superseded")`
#'
#' @param open_args optional, named (`list`) of additional parameters for the connection's
#'   `RocheTeradata::connect_teradata` open function. Please note that the `type`
#'   argument will be overwritten with `ODBC`.
#' @param close_args optional, named (`list`) of additional parameters for the connection's
#'   [DBI::dbDisconnect()] close function.
#' @param ping_args optional, named (`list`) of additional parameters for the connection's
#'   [DBI::dbIsValid()] ping function.
#'
#' @return (`TealDataConnection`) type of object
#'
#' @export
teradata_connection <- function(open_args = list(), close_args = list(), ping_args = list()) {
  lifecycle::deprecate_stop(
    when = "0.10.1",
    what = "teradata_connection()",
    details = "Please use teal.connectors.teradata::teradata_connection()."
  )
}

#' Superseded (moved into separate package): Helper function to connect to `Snowflake`
#'
#' @description `r lifecycle::badge("superseded")`
#'
#' This is used by `snowflake_connection` and does not need to be called directly
#' @param username the username used to collect the auth token to connect to snowflake.
#' @param password the password used to collect the auth token to connect to snowflake
#' @param role the user role used to connect to `Snowflake`.
#' @param database the `Snowflake` database to connect to.
#' @param schema the `Snowflake` schema to connect to.
#' @param warehouse the `Snowflake` warehouse to connect to.
#' @param server the `Snowflake`server to connect to.
#' @param port the port to connect to the `Snowflake` instance.
#' @param driver the driver to use to connect to the `Snowflake` instance.
#' @param token_provider location of the auth token provider needed to access `Snowflake`.
#' @export
snowflake_connection_function <- function(username = askpass::askpass("Please enter your username"),
                                          password = askpass::askpass("Please enter your password"),
                                          role,
                                          database,
                                          schema,
                                          warehouse,
                                          server,
                                          port = 443,
                                          driver = "SnowflakeDSIIDriver",
                                          token_provider) {
  lifecycle::deprecate_stop(
    when = "0.10.1",
    what = "snowflake_connection_function()",
    details = "Please use teal.connectors.snowflake::snowflake_connection_function()."
  )
}

#' Superseded (moved into separate package): Open connection to `Snowflake`
#'
#' @description `r lifecycle::badge("superseded")`
#'
#' @param open_args optional, named (`list`) of additional parameters for the connection's
#'   `snowflake_connection_function` open function.
#' @param close_args optional, named (`list`) of additional parameters for the connection's
#'   [DBI::dbDisconnect()] close function.
#' @param ping_args optional, named (`list`) of additional parameters for the connection's
#'   [DBI::dbIsValid()] ping function.
#'
#' @return (`TealDataConnection`) type of object
#'
#' @export
snowflake_connection <- function(open_args = list(), close_args = list(), ping_args = list()) {
  lifecycle::deprecate_stop(
    when = "0.10.1",
    what = "snowflake_connection()",
    details = "Please use teal.connectors.snowflake::snowflake_connection()."
  )
}

#' Superseded (moved into separate package): Open connection to `CDSE`
#'
#' @description `r lifecycle::badge("superseded")`
#'
#' @param env optional, `CDSE` environment name.
#'
#' @return (`TealDataConnection`) type of object
#'
#' @export
cdse_connection <- function(env = "prod") {
  lifecycle::deprecate_stop(
    when = "0.10.1",
    what = "cdse_connection()",
    details = "Please use teal.connectors.cdse::cdse_connection()."
  )
}

#' Superseded (moved into separate package): Open connection to `DataSetDB`
#'
#' @description `r lifecycle::badge("superseded")`
#'
#' @return (`TealDataConnection`) type of object
#'
#' @export
datasetdb_connection <- function() {
  lifecycle::deprecate_stop(
    when = "0.10.1",
    what = "datasetdb_connection()",
    details = "Please use teal.connectors.datasetdb::datasetdb_connection()."
  )
}

#' Superseded (moved into separate package): Open connection to `entimICE` via `ricepass`
#'
#' @description `r lifecycle::badge("superseded")`
#'
#' @return (`TealDataConnection`) type of object
#'
#' @export
ricepass_connection <- function() {
  lifecycle::deprecate_stop(
    when = "0.10.1",
    what = "ricepass_connection()",
    details = "Please use teal.connectors.rice::ricepass_connection()."
  )
}