#' Run code and mask inputs
#'
#' Delayed Data Loading module with login and password input.
#'
#' @name submit_button_module
#'
#'
#' @param id (`character`) `shiny` module id.
#' @param ... (`list`) arguments passed to `ddl_run` function.
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
submit_button_server <- function(id, ...) {
  moduleServer(id, function(input, output, session) {
    tdata <- eventReactive(input$submit, {
      ddl_run(input = input, ...)
    })

    # would need to make sure we handle reactivity correctly here as teal::init expects not reactive teal_data...
    return(tdata)
  })
}

#' Wrapper for `ui` and `server` in `ddl` object
#'
#' Convenience wrapper for `ui` and `server` functions in `ddl` object.
#' On the `server` side, function calls `shiny` module and adjusts the arguments
#' to the formals of the `server` function.
#' @param id (`character`) `shiny` module id.
#' @param x (`ddl`) object.
#' @name ddl_module
#' @return `shiny` module
#' @keywords internal
NULL

#' @rdname ddl_module
#' @keywords internal
ddl_server <- function(id, x) {
  # subset attributes to only those that are arguments of the server function
  args <- names(formals(x$server))
  attrs <- attributes(x)
  attrs <- attrs[setdiff(names(attrs), c("id", "class", "names"))]
  do.call(x$server, c(list(id = id), attrs))
}

#' @rdname ddl_module
#' @keywords internal
ddl_ui <- function(id, x) {
  x$ui(id = id)
}
