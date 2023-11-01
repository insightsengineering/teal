#' Run code and mask inputs
#'
#' Delayed Data Loading module with login and password input.
#'
#' @name submit_button_module
#'
#'
#' @param id (`character`) `shiny` module id.
#' @param ... (`list`) arguments passed to [eval_and_mask()].
#' @return `shiny` module
NULL

#' @rdname submit_button_module
#' @export
submit_button_ui <- function(id) {
  ns <- NS(id)
  actionButton(inputId = ns("submit"), label = "Submit")
}

#' @rdname submit_button_module
#' @export
submit_button_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    tdata <- eventReactive(input$submit, {
      eval_and_mask(input = input, ...)
    })

    # would need to make sure we handle reactivity correctly here as teal::init expects not reactive teal_data...
    return(tdata)
  })
}

# todo: to remove before merge -------------
#' @export
open_conn <- function(username, password) {
  if (password != "pass") stop("Invalid credentials. 'pass' is the password") else TRUE
}
#' @export
close_conn <- function(conn) {
  message("closed")
  return(NULL)
}
