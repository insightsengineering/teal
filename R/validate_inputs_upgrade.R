
#' Send input validation messages to output.
#'
#' Captures messages from `InputValidator` objects and collates them
#' into one message passed to `validate`.
#'
#' `shiny::validate` is used to withhold rendering of an output element until
#' certain conditions are met and to print a validation message in place
#' of the output element.
#' `shinyvalidate::InputValidator` allows to validate input elements
#' and to display specific messages in their respective input widgets.
#' `validate_inputs_upgrade` provides a hybrid solution.
#' Given an `InputValidator` object, messages corresponding to inputs that fail validation
#' are extracted and placed in one validation message that is passed to a `validate`/`need` call.
#' This way the input validator messages are repeated in the output.
#'
#' The `...` argument accepts `InputValidators` directly or wrapped in a list.
#' If validators are passed directly, all their messages are printed together
#' under one (optional) header message specified by `header`. If a list is passed,
#' messages are grouped by validator. The list's names are used as headers
#' for their respective message groups.
#'
#' @param ... either any number of `InputValidator` objects
#'            or an optionally named `list` of `InputValidator` objects, see `Details`
#' @param header `character(1)` generic validation message; set to NULL to omit
#'
#' @return
#' Returns NULL if the final validation call passes and a `shiny.silent.error` if it fails.
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
#'         validate_inputs_upgrade(iv)
#'         validate_inputs_upgrade(iv_par, header = "Set proper graphical parameters")
#'       },
#'       "combined" = validate_inputs_upgrade(iv, iv_par),
#'       "grouped" = validate_inputs_upgrade(list(
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

#' @export
validate_inputs_upgrade <- function(..., header = "Some inputs require attention") {
  dots <- list(...)

  # check arguments and determine input type
  if (all(vapply(dots, inherits, logical(1L), what = "InputValidator"))) {
    input <- "validators"
  } else if (all(vapply(unlist(dots), inherits, logical(1L), what = "InputValidator"))) {
    input <- "list"
  } else {
    stop("... must be InputValidator objects or a list thereof")
  }
  checkmate::assert_string(header, null.ok = TRUE)

  vals <- switch(
    input,
    "validators" = dots,
    "list" = unlist(dots)
  )

  if (!all(vapply(vals, validator_enabled, logical(1L)))) {
    logger::log_warn("Some validators are disabled and will be omitted.")
    vals <- Filter(validator_enabled, vals)
  }

  failings <- switch(
    input,
    "validators" = wrap_together(vals, header),
    "list" = wrap_separately(vals)
  )

  shiny::validate(shiny::need(is.null(failings), failings))
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

#' @keywords internal
# collates messages from multiple validators under common header
wrap_together <- function(iv_list, header) {
  add_header(unlist(lapply(iv_list, gather_messages)), header)
}

#' @keywords internal
# prints messages from multiple validators under separate headers
wrap_separately <- function(iv_list) {
  fail_messages <- vector("list", length(iv_list))
  for (v in seq_along(iv_list)) {
    fail_messages[[v]] <- gather_and_add(iv_list[[v]], names(iv_list)[v])
  }
  unlist(fail_messages)
}

#' @keywords internal
# test if an InputValidator object is enabled
# returns logical of length 1
validator_enabled <- function(x) {
  x$.__enclos_env__$private$enabled
}
