#' @name username_password_module
#'
#' @inheritParams ddl
#' @param id (`character`) `shiny` module id.
NULL

#' @rdname username_password_module
#' @export
username_password_server <- function(id, offline_args, code, postprocess_fun) {
  moduleServer(id, function(input, output, session) {
    # todo:
    #  - disable submit button
    #  - validate password and username
    #  - think about hashing password
    #  - enable submit only when username and password are not empty
    # create a tdata object when submit is pressed
    tdata <- eventReactive(input$submit, {
      ddl_run(offline_args = offline_args, code = code, postprocess_fun = postprocess_fun, input = input)
    })

    # would need to make sure we handle reactivity correctly here as teal::init expects not reactive tdata...
    return(tdata)
  })
}

#' @rdname username_password_module
#' @export
username_password_ui <- function(id) {
  ns <- NS(id)
  tagList(
    textInput(ns("username"), label = "Username"),
    passwordInput(ns("password"), label = "Password"),
    actionButton(ns("submit"), label = "Submit")
  )
}

#' offline_args for username_password_module
#'
#' @rdname username_password_module
#' @export
username_password_args <- function() {
  list(
    username = quote(askpass::askpass("Please enter username")),
    password = quote(askpass::askpass("Please enter password"))
  )
}



# todo: to remove
open_dummy_conn <- function(username, password) {
  if (password != "pass") stop("Invalid credentials") else TRUE
}
close_dummy_conn <- function(conn) {
  return(NULL)
}
