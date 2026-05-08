#' Send input validation messages to output
#'
#' Captures messages from `InputValidator` objects and collates them
#' into one message passed to `validate`.
#'
#' `shiny::validate` is used to withhold rendering of an output element until
#' certain conditions are met and to print a validation message in place
#' of the output element.
#' `shinyvalidate::InputValidator` allows to validate input elements
#' and to display specific messages in their respective input widgets.
#' `validate_inputs` provides a hybrid solution.
#' Given an `InputValidator` object, messages corresponding to inputs that fail validation
#' are extracted and placed in one validation message that is passed to a `validate`/`need` call.
#' This way the input `validator` messages are repeated in the output.
#'
#' The `...` argument accepts any number of `InputValidator` objects
#' or a nested list of such objects.
#' If `validators` are passed directly, all their messages are printed together
#' under one (optional) header message specified by `header`. If a list is passed,
#' messages are grouped by `validator`. The list's names are used as headers
#' for their respective message groups.
#' If neither of the nested list elements is named, a header message is taken from `header`.
#'
#' @param ... either any number of `InputValidator` objects
#'            or an optionally named, possibly nested `list` of `InputValidator`
#'            objects, see `Details`
#' @param header (`character(1)`) generic validation message; set to NULL to omit
#'
#' @return
#' Returns NULL if the final validation call passes and a `shiny.silent.error` if it fails.
#'
#' @seealso [`shinyvalidate::InputValidator`], [`shiny::validate`]
#'
#'
#' @examplesShinylive
#' library(teal)
#' interactive <- function() TRUE
#' {{ next_example }}
#' @examplesIf require("shinyvalidate")
#' library(shiny)
#' library(shinyvalidate)
#'
#' ui <- fluidPage(
#'   selectInput("method", "validation method", c("sequential", "combined", "grouped")),
#'   sidebarLayout(
#'     sidebarPanel(
#'       selectInput("letter", "select a letter:", c(letters[1:3], LETTERS[4:6])),
#'       selectInput("number", "select a number:", 1:6),
#'       tags$br(),
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
#'   iv$add_rule("number", function(x) {
#'     if (as.integer(x) %% 2L == 1L) "choose an even number"
#'   })
#'   iv$enable()
#'   # more input validation
#'   iv_par <- InputValidator$new()
#'   iv_par$add_rule("color", sv_required(message = "choose a color"))
#'   iv_par$add_rule("color", function(x) {
#'     if (length(x) > 1L) "choose only one color"
#'   })
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
#'       "grouped" = validate_inputs(list(
#'         "Some inputs require attention" = iv,
#'         "Set proper graphical parameters" = iv_par
#'       ))
#'     )
#'
#'     plot(faithful$eruptions ~ faithful$waiting,
#'       las = 1, pch = 16,
#'       col = input[["color"]], cex = input[["size"]]
#'     )
#'   })
#' }
#'
#' if (interactive()) {
#'   shinyApp(ui, server)
#' }
#'
#' @export
#'
validate_inputs <- function(..., header = "Some inputs require attention") {
  dots <- list(...)
  if (!is_validators(dots)) stop("validate_inputs accepts validators or a list thereof")

  messages <- extract_validator(dots, header)
  failings <- if (!any_names(dots)) {
    add_header(messages, header)
  } else {
    unlist(messages)
  }

  shiny::validate(shiny::need(is.null(failings), failings))
}

### internal functions

#' @noRd
#' @keywords internal
# recursive object type test
# returns logical of length 1
is_validators <- function(x) {
  all(if (is.list(x)) unlist(lapply(x, is_validators)) else inherits(x, "InputValidator"))
}

#' @noRd
#' @keywords internal
# test if an InputValidator object is enabled
# returns logical of length 1
# official method requested at https://github.com/rstudio/shinyvalidate/issues/64
validator_enabled <- function(x) {
  x$.__enclos_env__$private$enabled
}

#' Recursively extract messages from validator list
#' @return A character vector or a list of character vectors, possibly nested and named.
#' @noRd
#' @keywords internal
extract_validator <- function(iv, header) {
  if (inherits(iv, "InputValidator")) {
    add_header(gather_messages(iv), header)
  } else {
    if (is.null(names(iv))) names(iv) <- rep("", length(iv))
    mapply(extract_validator, iv = iv, header = names(iv), SIMPLIFY = FALSE)
  }
}

#' Collate failing messages from validator.
#' @return `list`
#' @noRd
#' @keywords internal
gather_messages <- function(iv) {
  if (validator_enabled(iv)) {
    status <- iv$validate()
    failing_inputs <- Filter(Negate(is.null), status)
    unique(lapply(failing_inputs, function(x) x[["message"]]))
  } else {
    warning("Validator is disabled and will be omitted.")
    list()
  }
}

#' Add optional header to failing messages
#' @noRd
#' @keywords internal
add_header <- function(messages, header = "") {
  ans <- unlist(messages)
  if (length(ans) != 0L && isTRUE(nchar(header) > 0L)) {
    ans <- c(paste0(header, "\n"), ans, "\n")
  }
  ans
}

#' Recursively check if the object contains a named list
#' @noRd
#' @keywords internal
any_names <- function(x) {
  any(
    if (is.list(x)) {
      if (!is.null(names(x)) && any(names(x) != "")) TRUE else unlist(lapply(x, any_names))
    } else {
      FALSE
    }
  )
}

#' Validate input
#'
#' Validate a Shiny input and send validation messages to the client.
#' The message appears both in the input widget and in the output element
#' that calls `validate_input`.
#' @details
#' - `validate_input():`
#'
#'     Validate a Shiny input and send validation messages to the client.
#'     The message appears both in the input widget and in the output element
#'     that calls `validate_input`.
#'
#' @param inputId (`character`) Character of input ID(s) to validate
#' @param condition (`logical(1)`, `function(x)`) Logical value or function returning logical value.
#'  Condition should determine expected state, `FALSE` throws.
#' @param message (`character(1)`) Character string of validation message to display.
#' @param add (`validation_collection`) Optional validation collection to store the message in,
#' see `make_validation_collection()` details.
#' @param session Shiny session object
#'
#' @return `NULL` or `shiny.silent.error` when condition is not met
#'
#' @examples
#' # Only run examples in interactive R sessions
#' options(device.ask.default = FALSE)
#'
#' ui <- fluidPage(
#'   checkboxGroupInput("in1", "Check some letters", choices = head(LETTERS)),
#'   selectizeInput("in2", "Select a state", choices = c("", state.name)),
#'   use_validate_input_js(),
#'   plotOutput("plot")
#' )
#'
#' server <- function(input, output) {
#'   output$plot <- renderPlot({
#'     validate_input("in1", condition = function(x) length(x) > 0, "Check at least one letter!")
#'     validate_input("in2", condition = function(x) x != "", "Please choose a state.")
#'     plot(1:10, main = paste(c(input$in1, input$in2), collapse = ", "))
#'   })
#' }
#'
#' if (interactive()) {
#'   shinyApp(ui, server)
#' }
#'
#' my_module <- module(
#'   label = "My Module",
#'   datanames = NULL,
#'   ui = function(id) {
#'     ns <- NS(id)
#'     tagList(
#'       checkboxGroupInput(ns("letters"), "Select letters:", choices = head(LETTERS), inline = TRUE),
#'       checkboxGroupInput(ns("letters2"), "Select letters:", choices = head(LETTERS), inline = TRUE),
#'       tags$h3("Sample plot"),
#'       plotOutput(ns("plot"))
#'     )
#'   },
#'   server = function(id, data) {
#'     moduleServer(id, function(input, output, session) {
#'       output$plot <- renderPlot({
#'         validate_input(
#'           "letters",
#'           condition = function(x) length(x) > 0,
#'           message = "Select at least one letter."
#'         )
#'         validate_input(
#'           c("letters", "letters2"),
#'           condition = function(x, y) all(!x %in% y),
#'           message = "Letters in the first group should not be in the second group."
#'         )
#'         tab <- rbind(
#'           Group1 = as.integer(head(LETTERS) %in% input$letters),
#'           Group2 = as.integer(head(LETTERS) %in% input$letters2)
#'         )
#'         colnames(tab) <- head(LETTERS)
#'         barplot(
#'           tab,
#'           beside = TRUE, legend.text = TRUE, main = "Selected letters per group",
#'           col = c("steelblue", "tomato")
#'         )
#'       })
#'     })
#'   }
#' )
#'
#' app <- init(
#'   data = within(teal_data(), iris <- iris),
#'   modules = my_module
#' )
#'
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#' @export
validate_input <- function(inputId, # nolint
                           condition = function(x) TRUE,
                           message = "Check the input",
                           add = NULL,
                           session = shiny::getDefaultReactiveDomain()) {
  checkmate::assert_character(inputId, min.len = 1)
  checkmate::assert(
    checkmate::check_flag(condition),
    checkmate::check_function(condition, nargs = length(inputId))
  )
  checkmate::assert_string(message)
  checkmate::assert_class(add, classes = "validation_collection", null.ok = TRUE)

  # Evaluate condition if it's a function
  condition_result <- if (is.function(condition)) {
    input_value <- lapply(inputId, function(id) session$input[[id]])
    checkmate::assert_flag(do.call(condition, input_value))
  } else {
    condition
  }

  # Send custom message to JavaScript handler
  lapply(inputId, function(id) {
    session$sendCustomMessage("validateInput", list(
      inputId = session$ns(id),
      isValid = condition_result,
      message = message
    ))
  })

  need_result <- need(condition_result, message)
  if (is.null(add)) {
    if (!is.null(need_result)) {
      stop(
        structure(
          list(message = need_result),
          class = c("error", "condition", "shiny.silent.error", "validation-input", "validation"),
          inputId = vapply(inputId, session$ns, character(1L))
        )
      )
    }
    validate(need_result)
  } else {
    add$push(need_result)
    invisible(NULL)
  }
}

#' @rdname validate_input
#' @details
#' - `use_validate_input_js()`:
#'
#'     Include JavaScript for client-side input validation.
#' @export
use_validate_input_js <- function() {
  addResourcePath("js", system.file("js", package = "teal"))
  # system.file("js", "input-validator.js", package = "teal"))
  singleton(tags$script(src = "js/input-validator.js"))
}

#' @rdname validate_input
#' @details
#' - `make_validation_collection()`:
#'
#'     Create a validation collection to store multiple validation messages.
#'
#'     This can be used to collect messages from multiple `validate_input`
#'     calls and validate them together with `<validation_collection>$validate()`.
make_validation_collection <- function() {
  msgs <- character(0L)
  x <- list(
    push = function(msg) msgs <<- c(msgs, msg),
    getMessages = function() msgs,
    isEmpty = function() length(msgs) == 0L,
    validate = function() do.call(validate, as.list(msgs))
  )
  class(x) <- "validation_collection"
  x
}
