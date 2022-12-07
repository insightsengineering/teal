
#' send input validation messages to output
#'
#' Captures messages from \code{InputValidator} objects and collates them
#' into one message passed to \code{validate}.
#'
#' \code{shiny::validate} is used to withhold rendering of an output element until
#' certain conditions are met and to print a validation message in place
#' of the output element.
#' \code{shinyvalidate::InputValidator} allows to validate input elements
#' and to display specific messages in their respective input widgets.
#' This function is a hybrid solution. Given an \code{InputValidator} object,
#' it extracts messages from inputs that fail validation and places them all in one
#' validation message that is passed to a \code{validate}\code{need} call.
#' This way the input validator messages are repeated in the output.
#'
#' \code{validate_inputs} accepts an arbitrary number of \code{InputValidator}s
#' and prints all messages together, adding one (optional) header.
#' \code{validate_inputs_segregated} accepts a list of \code{InputValidator}s
#' and prints messages grouped by validator. If elements of \code{validators} are named,
#' the names are used as headers for their respective message groups.
#'
#'
#' @name validate_inputs
#'
#' @param ... for \code{validate_inputs} any number of \code{InputValidator} objects \cr
#'            for \code{validate_inputs_segregated} arguments passed to \code{validate}
#' @param header \code{character(1)} optional generic validation message
#' @param validators optionally named \code{list} of \code{InputValidator} objects, see \code{Details}
#'
#' @return
#' Returns NULL if the final validation call passes and a \code{shiny.silent.error} if it fails.
#'
#' @seealso [`shinyvalidate::InputValidator`] [`shiny::validate`]
#'
#' @examples
#' library(shiny)
#' library(shinyvalidate)
#'
#' ui <- fluidPage(
#'   selectInput("method", "validation method", c("sequential", "combined", "grouped")),
#'   sidebarLayout(
#'     sidebarPanel(
#'       selectInput("letter", "select a letter:", c(letters[1:3], LETTERS[4:6])),
#'       selectInput("number", "select a number:", 1:6),
#'       br(),
#'       selectInput("color", "select a color:",
#'         c("black", "indianred2", "springgreen2", "cornflowerblue"),
#'         multiple = TRUE
#'       ),
#'       sliderInput("size", "select point size:",
#'         min = 0.1, max = 4, value = 0.25
#'       )
#'     ),
#'     mainPanel(plotOutput("plot"))
#'   )
#' )
#'
#' server <- function(input, output) {
#'   # set up input validation
#'   iv <- InputValidator$new()
#'   iv$add_rule("letter", sv_in_set(LETTERS, "choose a capital letter"))
#'   iv$add_rule("number", ~ if (as.integer(.) %% 2L == 1L) "choose an even number")
#'   iv$enable()
#'   # more input validation
#'   iv_par <- InputValidator$new()
#'   iv_par$add_rule("color", sv_required(message = "choose a color"))
#'   iv_par$add_rule("color", ~ if (length(.) > 1L) "choose only one color")
#'   iv_par$add_rule(
#'     "size",
#'     sv_between(
#'       left = 0.5, right = 3,
#'       message_fmt = "choose a value between {left} and {right}"
#'     )
#'   )
#'   iv_par$enable()
#'
#'   output$plot <- renderPlot({
#'     # validate output
#'     switch(input[["method"]],
#'       "sequential" = {
#'         validate_inputs(iv)
#'         validate_inputs(iv_par, header = "Set proper graphical parameters")
#'       },
#'       "combined" = validate_inputs(iv, iv_par),
#'       "grouped" = validate_inputs_segregated(list(
#'         "Some inputs require attention" = iv,
#'         "Set proper graphical parameters" = iv_par
#'       ))
#'     )
#'
#'     plot(eruptions ~ waiting, faithful,
#'       las = 1, pch = 16,
#'       col = input[["color"]], cex = input[["size"]]
#'     )
#'   })
#' }
#'
#' if (interactive()) {
#'   shinyApp(ui, server)
#' }

#' @rdname validate_inputs
#' @export
validate_inputs <- function(..., header = "Some inputs require attention") {
  vals <- list(...)
  lapply(vals, checkmate::assert_class, "InputValidator")
  checkmate::assert_string(header, null.ok = TRUE)

  fail_messages <- unlist(lapply(vals, gather_messages))
  failings <- add_header(fail_messages, header)

  shiny::validate(shiny::need(is.null(failings), failings))
}


#' @rdname validate_inputs
#' @export
validate_inputs_segregated <- function(validators, ...) {
  checkmate::assert_list(validators, types = "InputValidator")

  # Since some or all names may be NULL, mapply cannot be used here, a loop is required.
  fail_messages <- vector("list", length(validators))
  for (v in seq_along(validators)) {
    fail_messages[[v]] <- gather_and_add(validators[[v]], names(validators)[v])
  }

  failings <- unlist(fail_messages)

  shiny::validate(shiny::need(is.null(failings), failings), ...)
}


### internal functions

#' @keywords internal
# collate failing messages from validator
gather_messages <- function(iv) {
  status <- iv$validate()
  failing_inputs <- Filter(Negate(is.null), status)
  unique(lapply(failing_inputs, function(x) x[["message"]]))
}


#' @keywords internal
# format failing messages with optional header message
add_header <- function(messages, header) {
  if (length(messages) > 0L) {
    c(paste0(header, "\n"), unlist(messages), "\n")
  } else {
    NULL
  }
}

#' @keywords internal
# collate failing messages with optional header message
# used by segregated method
gather_and_add <- function(iv, header) {
  fail_messages <- gather_messages(iv)
  failings <- add_header(fail_messages, header)
  failings
}
