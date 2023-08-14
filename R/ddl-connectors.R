scda_connector <- function(datanames) {
  x <- ddl(
    # code to be run when app user presses submit
    code = paste(
      sprintf(
        "%1$s <- scda::synthetic_cdisc_data({ version })$%1$s",
        datanames
      ),
      collapse = "\n"
    ),

    # ui they wish to use for the loading data
    ui = function(id) {
      ns <- NS(id)
      tagList(
        textInput(ns("version"), label = "SCDA version", value = "latest"),
        actionButton(ns("submit"), label = "Submit")
      )
    },

    # function returning data objects
    postprocess_fun = function(env_list, code) {
      do.call(teal.data::cdisc_data, args = c(env_list, code = code))
    }
  )
}

custom_password_connector <- function(datanames) {
  x <- ddl(
    # code to be run when app user presses submit
    code = paste(
      c(
        "conn <- open_dummy_conn(username = {username}, password = {password})",
        sprintf(
          "%1$s <- scda::synthetic_cdisc_data('latest')$%1$s",
          datanames
        ),
        "close_dummy_conn()"
      ),
      collapse = "\n"
    ),

    # arguments used for show R code
    offline_args = username_password_args(),

    # ui they wish to use for the loading data
    ui = username_password_ui,
    server = username_password_server,

    # function returning data objects
    postprocess_fun = function(env_list, code) {
      do.call(teal.data::cdisc_data, args = c(env_list, code = code))
    }
  )
}

rice_connector <- function(datanames) {

}
