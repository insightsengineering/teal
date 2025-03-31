


#
#   _       _                        _
#  (_)     | |                      | |
#   _ _ __ | |_ ___ _ __ _ __   __ _| |
#  | | '_ \| __/ _ \ '__| '_ \ / _` | |
#  | | | | | ||  __/ |  | | | | (_| | |
#  |_|_| |_|\__\___|_|  |_| |_|\__,_|_|
#
#
#
#  internal

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

  fun_names <- match.call(expand.dots = FALSE)[["..."]]

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

  new_server_fun = function(id) TRUE

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
                "teal-output-warning"
              } else {
                "shiny-output-error"
              }
              tags$div(class = html_class, tags$div(lapply(.x, tags$p)))
            }
          )
        )
      }
    })

    x
  }, list(check_calls == as.name(template_str)))

  new_body_list <- .substitute_template(template_str, module_server_body, check_calls)

  server_body <- substitute({
    assert_reactive(x)
    moduleServer(id, function(input, output, session) server_body)
  }, list(server_body = new_body_list))

  formals(new_server_fun) <- top_level_formals
  body(new_server_fun) <- server_body

  new_ui_fun <- function(id) uiOutput(NS(id, "errors"))

  # todo: check if body need
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
  # todo: discuss this approach vs. having curly braces
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

#
#                        _       _
#                       | |     | |
#  __   __            __| | __ _| |_ __ _ _ __   __ _ _ __ ___   ___  ___
#  \ \ / /           / _` |/ _` | __/ _` | '_ \ / _` | '_ ` _ \ / _ \/ __|
#   \ V /           | (_| | (_| | || (_| | | | | (_| | | | | | |  __/\__ \
#    \_/             \__,_|\__,_|\__\__,_|_| |_|\__,_|_| |_| |_|\___||___/
#           ______
#          |______|
#
#  v_datanames

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

#
#              _ _     _       _                             _   _
#             | (_)   | |     | |                           | | (_)
#  __   ____ _| |_  __| | __ _| |_ ___   _ __ ___  __ _  ___| |_ ___   _____
#  \ \ / / _` | | |/ _` |/ _` | __/ _ \ | '__/ _ \/ _` |/ __| __| \ \ / / _ \
#   \ V / (_| | | | (_| | (_| | ||  __/ | | |  __/ (_| | (__| |_| |\ V /  __/
#    \_/ \__,_|_|_|\__,_|\__,_|\__\___| |_|  \___|\__,_|\___|\__|_| \_/ \___|
#
#
#
#  validate reactive

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

#
#              _ _     _       _   _
#             | (_)   | |     | | (_)
#  __   ____ _| |_  __| | __ _| |_ _  ___  _ __    ___ _ __ _ __ ___  _ __
#  \ \ / / _` | | |/ _` |/ _` | __| |/ _ \| '_ \  / _ \ '__| '__/ _ \| '__|
#   \ V / (_| | | | (_| | (_| | |_| | (_) | | | ||  __/ |  | | | (_) | |
#    \_/ \__,_|_|_|\__,_|\__,_|\__|_|\___/|_| |_| \___|_|  |_|  \___/|_|
#                                             ______
#                                            |______|
#
#  validation_error

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
        sprintf("NEW:: Shiny validation error was raised: %s", x()$message)
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

#
#       _     _                 _ _            _
#      | |   (_)               (_) |          | |
#   ___| |__  _ _ __  _   _ ___ _| | ___ _ __ | |_ ___ _ __ _ __ ___  _ __
#  / __| '_ \| | '_ \| | | / __| | |/ _ \ '_ \| __/ _ \ '__| '__/ _ \| '__|
#  \__ \ | | | | | | | |_| \__ \ | |  __/ | | | ||  __/ |  | | | (_) | |
#  |___/_| |_|_|_| |_|\__, |___/_|_|\___|_| |_|\__\___|_|  |_|  \___/|_|
#                      __/ |
#                     |___/
#
#  shinysilenterror

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
srv_module_check_shinysilenterror <- function(x) {
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

#
#              _ _     _       _         _             _      _       _
#             | (_)   | |     | |       | |           | |    | |     | |
#  __   ____ _| |_  __| | __ _| |_ ___  | |_ ___  __ _| |  __| | __ _| |_ __ _
#  \ \ / / _` | | |/ _` |/ _` | __/ _ \ | __/ _ \/ _` | | / _` |/ _` | __/ _` |
#   \ V / (_| | | | (_| | (_| | ||  __/ | ||  __/ (_| | || (_| | (_| | || (_| |
#    \_/ \__,_|_|_|\__,_|\__,_|\__\___|  \__\___|\__,_|_| \__,_|\__,_|\__\__,_|
#                                                     ______
#                                                    |______|
#
#  validate teal_data

srv_module_check_teal_data <- function(x) {
  moduleServer("check_teal_data", function(input, output, session) {

    reactive({
      if (inherits(x(), "qenv.error")) { # TODO: remove qenv.error
        c(
          "NEW:: Error when executing the `data` module:",
          cli::ansi_strip(x()$message),
          "",
          "Check your inputs or contact app developer if error persists."
        )
      } else if (!inherits(x(), c("teal_data", "error"))) {
        "NEW:: Did not receive `teal_data` object. Cannot proceed further."
      } else {
        TRUE
      }
    })
  })
}

module_validate_teal_data <- module_validate_factory(srv_module_check_teal_data)

#
#              _ _     _       _
#             | (_)   | |     | |
#  __   ____ _| |_  __| | __ _| |_ ___    ___ _ __ _ __ ___  _ __
#  \ \ / / _` | | |/ _` |/ _` | __/ _ \  / _ \ '__| '__/ _ \| '__|
#   \ V / (_| | | | (_| | (_| | ||  __/ |  __/ |  | | | (_) | |
#    \_/ \__,_|_|_|\__,_|\__,_|\__\___|  \___|_|  |_|  \___/|_|
#
#
#
#  validate condition

srv_module_check_condition <- function(x, validate_shiny_silent_error = TRUE) {
  moduleServer("check_error", function(input, output, session) {

    reactive({
      # TODO: remove qenv.error
      if (validate_shiny_silent_error && inherits(x(), "error") && !inherits(x(), c("qenv.error", "shiny.silent.error"))) {
        c("NEW:: Error detected", x()$message)
      } else {
        TRUE
      }
    })
  })
}

module_validate_condition <- module_validate_factory(srv_module_check_condition)

#
#                                    ___
#                                   |__ \
#   _ __ ___ _ __ ___   _____   _____  ) |
#  | '__/ _ \ '_ ` _ \ / _ \ \ / / _ \/ /
#  | | |  __/ | | | | | (_) \ V /  __/_|
#  |_|  \___|_| |_| |_|\___/ \_/ \___(_)
#
#
#
#  todo: remove?

.substitute_template_curly <- function(template_str, module_server_body, check_calls) {
  call_inject <- if (length(check_calls) > 1) {
    as.call(c(quote(`{`), check_calls))
  } else {
    as.call(check_calls[[1]])
  }

  vv <- substitute({
    collection <- list()
    check_calls

    validate_r <- reactive({
      message_collection <- Reduce(
        function(u, v) if (isTRUE(v())) u else c(u, v()),
        x = collection,
        init = c()
      )

      validate(need(length(message_collection) == 0, message_collection))
      TRUE
    })

    output$errors <- renderUI({
      validate_r()
      NULL
    })

    x
  }, list(check_calls = call_inject))
}

module_validate_factory_single <- function(module_id, check_fun) {
  fun_name <- if (is.character(check_fun)) check_fun else deparse(substitute(check_fun))
  fun_formals <- formals(check_fun)

  server_body <- substitute(
    {
      moduleServer(module_id, function(input, output, session) {
        collection <- list()
        # todo: start with a req() of first argument
        collection <- append(collection, check_call)

        validate_r <- reactive({
          message_collection <- Reduce(
            function(u, v) if (isTRUE(v())) u else c(u, v()),
            x = collection,
            init = c()
          )

          validate(need(length(message_collection) == 0, message_collection))
          TRUE
        })

        output$errors <- renderUI({
          validate_r()
          NULL
        })

        x
      })
    },
    list(
      # Generates call with exact formals of check function
      check_call = rlang::call2(fun_name, !!!lapply(names(fun_formals), as.name)),
      module_id = module_id
    )
  )

  new_server_fun = function() TRUE
  formals(new_server_fun) <- formals(check_fun)
  body(new_server_fun) <- server_body

  new_ui_fun <- function(id) TRUE
  body(new_ui_fun) <- substitute(
    {
      uiOutput(NS(NS(id, module_id), "errors"))
    },
    list(module_id = module_id)
  )

  list(
    ui = new_ui_fun,
    server = new_server_fun
  )
}

#
#                  _          __                                    ___
#                 | |        / _|                                  |__ \
#    ___ _ __   __| |   ___ | |_   _ __ ___ _ __ ___   _____   _____  ) |
#   / _ \ '_ \ / _` |  / _ \|  _| | '__/ _ \ '_ ` _ \ / _ \ \ / / _ \/ /
#  |  __/ | | | (_| | | (_) | |   | | |  __/ | | | | | (_) \ V /  __/_|
#   \___|_| |_|\__,_|  \___/|_|   |_|  \___|_| |_| |_|\___/ \_/ \___(_)
#
#
#
#  end of remove?


module_validate_error <- module_validate_factory(
  srv_module_check_shinysilenterror,
  srv_module_check_validation_error,
  srv_module_check_reactive,
  srv_module_check_condition
)

#' @keywords internal
ui_validate_error <- function(id) {
  ns <- NS(id)
  tagList(
    module_validate_shinysilenterror$ui(ns("validate_shinysilent_error")),
    module_validate_shinysilenterror$ui(ns("validate_validation_error")),
    module_validate_reactive$ui(ns("validate_reactive")),
    # module_validate_teal_data$ui(ns("validate_teal_data")),
    module_validate_condition$ui(ns("validate_condition"))
  )
}

#' @keywords internal
srv_validate_error <- function(id, data, validate_shiny_silent_error) {
  checkmate::assert_string(id)
  checkmate::assert_flag(validate_shiny_silent_error)
  moduleServer(id, function(input, output, session) {
    module_validate_shinysilenterror$server("validate_shinysilent_error", data)
    module_validate_validation_error$server("validate_validation_error", data)
    module_validate_reactive$server("validate_reactive", data)
    module_validate_condition$server("validate_condition", data)

    # # Uncomment line below and choose "validate error"
    # module_validate_reactive$server("validate_reactive", data, types = c("simpleError", "teal_data"))

    # # Old
    # output$message <- renderUI({
    #   is_shiny_silent_error <- inherits(data(), "shiny.silent.error") && identical(data()$message, "")
    #   if (inherits(data(), "qenv.error")) {
    #     validate(
    #       need(
    #         FALSE,
    #         paste(
    #           "Error when executing the `data` module:",
    #           cli::ansi_strip(paste(data()$message, collapse = "\n")),
    #           "\nCheck your inputs or contact app developer if error persists.",
    #           collapse = "\n"
    #         )
    #       )
    #     )
    #   } else if (inherits(data(), "error")) {
    #     if (is_shiny_silent_error && !validate_shiny_silent_error) {
    #       return(NULL)
    #     }
    #     validate(
    #       need(
    #         FALSE,
    #         sprintf(
    #           "Shiny error when executing the `data` module.\n%s\n%s",
    #           data()$message,
    #           "Check your inputs or contact app developer if error persists."
    #         )
    #       )
    #     )
    #   }
    # })
  })
}
