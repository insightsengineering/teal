#' Factory to build validate modules
#'
#' This function is used to create a module that validates the reactive data
#' passed to it.
#'
#' Dynamically generation of an `ui` and `server` function that can be used
#' internally in teal or in a teal module.
#'
#' @param module_id (`character(1)`) The module id.
#' @param ... (`function`) 1 or more [`shiny::moduleServer()`] functions that
#' return a [`shiny::reactive()`] with `TRUE` or a character string detailing
#' the excpetion.
#' It can be a named function, a character string or an anonymous function.
#'
#' @returns A list with `ui` and `server` functions with code generated from the
#' arguments.
#'
#' @examples
#'
#' check_error <- function(x, skip_on_empty_message = TRUE) {
#'   moduleServer("check_error", function(input, output, session) {
#'     reactive({
#'       if (inherits(x(), "error") && (!skip_on_empty_message || !identical(x()$message, ""))) {
#'         c("Error detected", x()$message)
#'       } else {
#'         TRUE
#'       }
#'     })
#'   })
#' }
#'
#' module_validate_factory(check_error)
#'
#' check_numeric <- function(x, skip = FALSE) {
#'   moduleServer("check_numeric", function(input, output, session) {
#'     reactive(if (inherits(x(), numeric) || skip) TRUE else "Error: is not numeric")
#'   })
#' }
#'
#' module_validate_factory(check_error, check_numeric)
#' @export
module_validate_factory <- function(..., stop_on_first = TRUE) {
  dots <- rlang::list2(...)
  checkmate::check_list(dots, min.len = 1)
  checkmate::assert_flag(stop_on_first)

  fun_names <- match.call(expand.dots = FALSE)[["..."]] # Capture function names in arguments
  check_calls <- lapply( # Generate calls to each of the check functions
    seq_along(dots),
    function(fun_ix) {
      substitute(
        collection <- append(collection, check_call),
        list(check_call = rlang::call2(
          fun_names[[fun_ix]], !!!lapply(names(formals(dots[[fun_ix]])), as.name))
        )
      )
    }
  )

  new_server_fun = function(id) TRUE # Empty server template
  server_formals <- .join_formals(formals(new_server_fun), dots)
  if (stop_on_first) {
    server_formals <- c(server_formals, pairlist(stop_on_first = stop_on_first))
  }
  server_body <- .validate_module_server(check_calls, stop_on_first = stop_on_first)
  formals(new_server_fun) <- server_formals # update function formals
  body(new_server_fun) <- server_body # set the new generated body
  new_server_fun
}

ui_module_validate <- function(id) {
  div(
    id = NS(id, "validate_messages"),
    class = "teal_validated",
    tags$div(class = "messages", uiOutput(NS(id, "errors")))
  )
}

#' @keywords internal
.validate_module_server <- function(check_calls, stop_on_first) {
  condition <- if (stop_on_first) quote(length(u) > 0 || isTRUE(v()) || is.null(v())) else quote(isTRUE(v()) || is.null(v()))
  module_server_body <- bquote(
    { # Template moduleServer that supports multiple checks
      collection <- list()
      ..(check_calls) # collection <- append(collection, srv_module_check_condition(x))

      fun <- function(u, v) {
        if (.(condition)) u else append(u, list(v()))
      }
      validate_r <- reactive(Reduce(fun, x = collection, init = list()))
      has_errors <- reactiveVal(TRUE)

      output$errors <- renderUI({
        error_class <- c("shiny.silent.error", "validation", "error", "condition")
        if (length(validate_r()) > 0) {
          has_errors(FALSE)
          tagList(!!!lapply(validate_r(), .render_output_condition))
        } else {
          has_errors(TRUE)
          NULL
        }
      })

      has_errors
    },
    splice = TRUE
  )

  substitute({
    checkmate::assert_string(id) # Mandatory id parameter
    moduleServer(id, function(input, output, session) server_body)
  }, list(server_body = module_server_body))

}

#' @keywords internal
.render_output_condition <- function(cond) {
  checkmate::assert_multi_class(cond, c("shiny.tag", "shiny.tag.list", "character"))
  is_warning <- isTRUE(attr(cond[1], "is_warning")) || isTRUE(attr(cond, "is_warning"))

  html_class <- sprintf(
    "teal-output-condition %s",
    ifelse(is_warning, "teal-output-warning", "shiny-output-error")
  )

  if (!checkmate::test_multi_class(cond, c("shiny.tag", "shiny.tag.list"))) {
    html_class <- c(html_class, "prewrap-ws")
    cond <- lapply(cond, tags$p)
  }
  tags$div(class = html_class, tags$div(cond))
}

#' @keywords internal
.join_formals <- function(current_formals, call_list) {
  checkmate::assert(
    checkmate::check_list(current_formals),
    checkmate::check_class(current_formals, "pairlist")
  )
  Reduce( # Union of formals for all check functions (order of arguments is kept)
    function(u, v) {
      new_formals <- formals(v)
      vapply(intersect(names(new_formals), names(u)), function(x_name) {
        identical(new_formals[[x_name]], u[[x_name]]) || # Conflicting name/default pair will throw an exception.
          stop("Arguments for check function have conflicting definitions (different defaults)")
      }, FUN.VALUE = logical(1L))
      append(u, new_formals[setdiff(names(new_formals), names(u))])
    },
    init = current_formals,
    x = call_list
  )

}

#' @keywords internal
srv_module_check_datanames <- function(x, modules) {
  moduleServer("check_datanames", function(input, output, session) {
    reactive({
      if (!is.null(modules) && inherits(x(), "teal_data")) {
        is_modules_ok <- check_modules_datanames_html(
          modules = modules, datanames = names(x())
        )
        attr(is_modules_ok, "is_warning") <- TRUE
        is_modules_ok
      } else {
        TRUE
      }
    })
  })
}

#' @keywords internal
srv_module_check_reactive <- function(x, null.ok = FALSE) {
  reactive_message <- check_reactive(x, null.ok = null.ok)
  moduleServer("check_reactive", function(input, output, session) {
    reactive({
      if (isTRUE(reactive_message)) {
      reactive_message
      } else {
        TRUE
      }
    })
  })
}

#' @keywords internal
srv_module_check_validation_error <- function(x) {
  moduleServer("check_validation_error", function(input, output, session) {
    reactive({
      if (checkmate::test_class(x(), c("shiny.silent.error", "validation")) && !identical(x()$message, "")) {
        tagList(
          tags$span("Shiny validation error was raised:"),
          tags$blockquote(tags$em(x()$message))
        )
      } else {
        TRUE
      }
    })
  })
}

#' @keywords internal
srv_module_check_shinysilenterror <- function(x, validate_shiny_silent_error = TRUE) {
  moduleServer("check_shinysilenterror", function(input, output, session) {
    reactive({
      if (validate_shiny_silent_error && inherits(x(), "shiny.silent.error" && !identical(x()$message, ""))) {
        "Shiny silent error was raised"
      } else {
        TRUE
      }
    })
  })
}

#' @keywords internal
srv_module_check_teal_data <- function(x) {
  moduleServer("check_teal_data", function(input, output, session) {
    reactive({
      if (inherits(x(), "qenv.error")) { # TODO: remove qenv.error
        details <- attr(x(), "details", exact = TRUE)
        if (is.null(details)) {
          c(
            "Error when executing the `data` module:",
            cli::ansi_strip(x()$message),
            "",
            "Check your inputs or contact app developer if error persists."
          )
        } else {
          tagList(
            tags$span("Error when executing the", tags$code("data"), "module:"),
            tags$blockquote(tags$em(cli::ansi_strip(details$condition_message))),
            tags$span("from code:"),
            tags$code(class = "code-error", details$current_code)
          )
        }
      } else if (!inherits(x(), c("teal_data", "error"))) {
        tags$span(
          "Did not receive", tags$code("teal_data"), "object. Cannot proceed further."
        )
      } else {
        TRUE
      }
    })
  })
}

#' @keywords internal
srv_module_check_condition <- function(x) {
  moduleServer("check_error", function(input, output, session) {
    reactive({ # shiny.silent.errors are handled in a different module
      if (inherits(x(), "error") && !inherits(x(), "shiny.silent.error")) {
        tagList(
          tags$span("Error detected:"),
          tags$blockquote(tags$em(trimws(x()$message)))
        )
      } else {
        TRUE
      }
    })
  })
}

#' @keywords internal
srv_module_check_previous_state_warn <- function(x, show_warn = reactive(FALSE), message_warn = "not defined") {
  assert_reactive(show_warn)
  checkmate::assert(
    checkmate::check_string(message_warn),
    checkmate::check_class(message_warn, "shiny.tag"),
    checkmate::check_class(message_warn, "shiny.tag.list")
  )

  attr(message_warn, "is_warning") <- TRUE
  moduleServer("check_shinysilenterror", function(input, output, session) {
    reactive(if (show_warn()) message_warn else TRUE)
  })
}

srv_module_validate_teal_module <- module_validate_factory(
  srv_module_check_previous_state_warn,
  srv_module_check_shinysilenterror,
  srv_module_check_validation_error,
  srv_module_check_condition,
  srv_module_check_reactive,
  srv_module_check_teal_data,
  srv_module_check_datanames
)

srv_module_validate_transform <- module_validate_factory(
  srv_module_check_previous_state_warn,
  srv_module_check_shinysilenterror,
  srv_module_check_validation_error,
  srv_module_check_condition,
  srv_module_check_reactive,
  srv_module_check_teal_data
)

srv_module_validate_datanames <- module_validate_factory(
  srv_module_check_previous_state_warn,
  srv_module_check_datanames
)
