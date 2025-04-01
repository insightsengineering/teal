#' Factory to build validate modules
#'
#' This function is used to create a module that validates the reactive data
#' passed to it.
#'
#' Dynamically generation of an `ui` and `server` function that can be used
#' internally in teal or in a teal module.
#'
#'
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
module_validate_factory <- function(...) {
  dots <- rlang::list2(...)
  checkmate::check_list(dots, min.len = 1)

  # Capture function names in arguments
  fun_names <- match.call(expand.dots = FALSE)[["..."]]

  # Generate calls to each of the check functions
  # TODO: extract from here
  check_calls <- lapply(
    seq_len(length(dots)),
    function(fun_ix) {
      fun_name <- fun_names[[fun_ix]]
      fun_formals <- formals(dots[[fun_ix]])

      substitute(
        expr,
        list(
          expr = substitute(
            collection <- append(collection, check_call),
            list(check_call = rlang::call2(fun_name, !!!lapply(names(fun_formals), as.name)))
          )
        )
      )
    }
  )

  # Empty server template
  new_server_fun = function(id) TRUE

  # Union of formals for all check functions (order of arguments is kept)
  # Conflicting argument name/default will throw an exception.
  top_level_formals <- Reduce(
    function(u, v) {
      new_formals <- formals(v)
      common <- intersect(names(new_formals), names(u))
      vapply(common, function(x_name) {
        if (identical(new_formals[[x_name]], u[[x_name]])) {
          TRUE
        } else {
          stop("Arguments for check function have conflicting definitions (different defaults)")
        }
      }, FUN.VALUE = logical(1L))
      append(u, new_formals[setdiff(names(new_formals), names(u))])
    },
    init = formals(new_server_fun),
    x = dots
  )

  template_str = "check_calls"

  # Template moduleServer that supports multiple checks
  module_server_body <- substitute({
    collection <- list()
    check_calls

    validate_r <- reactive({
      message_collection <- Reduce(
        function(u, v) if (isTRUE(v()) || is.null(v())) u else append(u, list(v())),
        x = collection,
        init = list()
      )
      message_collection
    })

    output$errors <- renderUI({
      error_class <- c("shiny.silent.error", "validation", "error", "condition")
      if (length(validate_r()) > 0) {
        # Custom rendering of errors instead of validate
        #  this allows for more control over the output (as some show errors in
        # html)
        tagList(
          !!!lapply(
            validate_r(),
            function(.x) {
              html_class <- if (isTRUE(attr(.x[1], "is_warning")) || isTRUE(attr(.x, "is_warning"))) {
                "teal-output-warning teal-output-condition"
              } else {
                "shiny-output-error teal-output-condition"
              }
              if (!checkmate::test_multi_class(.x, c("shiny.tag", "shiny.tag.list"))) {
                html_class <- c(html_class, "prewrap-ws")
                .x <- lapply(.x, tags$p)
              }
              tags$div(class = html_class, tags$div(.x))
            }
          )
        )
      }
    })

    x
  }, list(check_calls == as.name(template_str)))

  # Replace template string with check function calls
  new_body_list <- .substitute_template(template_str, module_server_body, check_calls)

  # Generate top-level moduleServer function with default assertions
  server_body <- substitute({
    checkmate::assert_string(id) # Mandatory id parameter
    moduleServer(id, function(input, output, session) server_body)
  }, list(server_body = new_body_list))

  formals(new_server_fun) <- top_level_formals # update function formals
  body(new_server_fun) <- server_body # set the new generated body

  # ui function contains a simple "error" element
  new_ui_fun <- function(id) uiOutput(NS(id, "errors"))

  list(ui = new_ui_fun, server = new_server_fun)
}

#' Custom substitute function that injects multiple lines to an expression
#'
#' It must contain the `template_str` on the first level of the expression.
#'
#' @param template_str (`character(1)`) The call in the expression to be replaced.
#' @param module_server_body (`expression`) Any syntactically valid R expression.
#' @param check_calls (`list`) A list of expressions to be injected.
#'
#' @returns An expression with the `template_str` replaced by the `check_calls`.
#'
#' @keywords internal
.substitute_template <- function(template_str, module_server_body, check_calls) {
  # Create server body with expressions for multiple checks
  # note: using substitute directly will add curly braces around body
  # TODO: discuss this approach vs. having curly braces
  body_list <- as.list(module_server_body)[-1]
  ix <- which(body_list == as.name(template_str))

  as.call(
    c(
      quote(`{`),
      body_list[seq(1, ix - 1)],
      check_calls,
      body_list[seq(ix + 1, length(body_list))]
    )
  )
}

#' @keywords internal
srv_module_check_datanames <- function(id, x, modules) {
  checkmate::assert_string(id)
  moduleServer(id, function(input, output, session) {
    reactive({
      if (inherits(x(), "teal_data")) {
        is_modules_ok <- check_modules_datanames_html(
          modules = modules, datanames = names(x())
        )
        attr(is_modules_ok, "is_warning") <- TRUE
        is_modules_ok
      } else {
        TRUE # Error handled elsewhere (avoids showing)
      }
    })
  })
}

module_validate_datanames <- module_validate_factory(srv_module_check_datanames)

#' Validate if an argument is a reactive
#'
#' @param x (`reactive`) A reactive value.
#' @param types (`character`) A character vector with the types that the reactive.
#' @param null.ok (`logical`) If `TRUE`, the `x` argument can be `NULL`.
#'
#' @name module_validate_reactive
#' @seealso [module_validate_factory()]
#'
#' @returns A module that validates the reactive value.
#'
#' @export
srv_module_check_reactive <- function(x, types = character(0L), null.ok = FALSE) {
  reactive_message <- check_reactive(x, null.ok = null.ok)
  moduleServer("check_reactive", function(input, output, session) {

    reactive({
      if (isTRUE(reactive_message)) {
        if (length(types) > 0 && !inherits(x(), types)) {
          sprintf(
            "Reactive value's class may only of the following types: %s, but it is '%s'",
            paste("{", types, "}", sep = "", collapse = ", "),
            paste("{", class(x()), "}", sep = "", collapse = ", ")
          )
        } else {
          TRUE
        }
      } else {
        paste0("NEW:: ", reactive_message)
      }
    })
  })
}

#' @rdname module_validate_reactive
#' @param id (`character`) The module id.
#' @usage module_validate_reactive$server(x, types = character(0L), null.ok = FALSE)
#' module_validate_reactive$ui(id)
#' @examples
#' module_validate_reactive$ui("validate_reactive")
#'
#' # Show the generated server function
#' print(module_validate_reactive$server)
#' @export
module_validate_reactive <- module_validate_factory(srv_module_check_reactive)

#' Validate if an argument contains a `shiny.silent.error` validation error
#'
#' @param x (`reactive`) A reactive value.
#'
#' @name module_validate_shinysilenterror
#' @seealso [module_validate_factory()]
#'
#' @returns A module that validates the reactive value.
#'
#' @export
srv_module_check_validation_error <- function(x) {
  moduleServer("check_validation_error", function(input, output, session) {
    reactive({
      if (checkmate::test_class(x(), c("shiny.silent.error", "validation")) && !identical(x()$message, "")) {
        tagList(
          tags$span("NEW:: Shiny validation error was raised:"),
          tags$blockquote(tags$em(x()$message))
        )
      } else {
        TRUE
      }
    })
  })
}

#' @rdname module_validate_shinysilenterror
#' @param id (`character`) The module id.
#' @usage module_validate_shinysilenterror$ui(id)
#' module_validate_shinysilenterror$server(x)
#' @examples
#' module_validate_shinysilenterror$ui("validate_reactive")
#'
#' # Show the generated server function
#' print(module_validate_shinysilenterror$server)
#' @export
module_validate_validation_error <- module_validate_factory(srv_module_check_validation_error)

#' Validate if an argument contains a `shiny.silent.error`
#'
#' @param x (`reactive`) A reactive value.
#'
#' @name module_validate_shinysilenterror
#' @seealso [module_validate_factory()]
#'
#' @returns A module that validates the reactive value.
#'
#' @export
srv_module_check_shinysilenterror <- function(x, validate_shiny_silent_error = TRUE) {
  moduleServer("check_shinysilenterror", function(input, output, session) {
    reactive({
      if (validate_shiny_silent_error && inherits(x(), "shiny.silent.error") && identical(x()$message, "")) {
        "NEW:: Shiny silent error was raised"
      } else {
        TRUE
      }
    })
  })
}

#' @rdname module_validate_shinysilenterror
#' @param id (`character`) The module id.
#' @usage module_validate_shinysilenterror$ui(id)
#' module_validate_shinysilenterror$server(x)
#' @examples
#' module_validate_shinysilenterror$ui("validate_reactive")
#'
#' # Show the generated server function
#' print(module_validate_shinysilenterror$server)
#' @export
module_validate_shinysilenterror <- module_validate_factory(srv_module_check_shinysilenterror)

srv_module_check_teal_data <- function(x) {
  moduleServer("check_teal_data", function(input, output, session) {

    reactive({
      if (inherits(x(), "qenv.error")) { # TODO: remove qenv.error
        details <- attr(x(), "details", exact = TRUE)
        if (is.null(details)) {
          c(
            "NEW:: Error when executing the `data` module:",
            cli::ansi_strip(x()$message),
            "",
            "Check your inputs or contact app developer if error persists."
          )
        } else {
          tagList(
            tags$span("NEW:: Error when executing the", tags$code("data"), "module:"),
            tags$blockquote(tags$em(cli::ansi_strip(details$condition_message))),
            tags$span("from code:"),
            tags$code(class = "code-error", details$current_code)
          )
        }
      } else if (!inherits(x(), c("teal_data", "error"))) {
        tags$span(
          "NEW:: Did not receive", tags$code("teal_data"), "object. Cannot proceed further."
        )
      } else {
        TRUE
      }
    })
  })
}

module_validate_teal_data <- module_validate_factory(srv_module_check_teal_data)

srv_module_check_condition <- function(x) {
  moduleServer("check_error", function(input, output, session) {

    reactive({
      # TODO: remove qenv.error
      # shiny.silent.errors are handled in a different module
      if (inherits(x(), "error") && !inherits(x(), c("qenv.error", "shiny.silent.error"))) {
        tagList(
          tags$span("NEW:: Error detected:"),
          tags$blockquote(tags$em(trimws(x()$message)))
        )
      } else {
        TRUE
      }
    })
  })
}

module_validate_condition <- module_validate_factory(srv_module_check_condition)

module_validate_error <- module_validate_factory(
  srv_module_check_shinysilenterror,
  srv_module_check_validation_error,
  srv_module_check_reactive,
  srv_module_check_condition
)
