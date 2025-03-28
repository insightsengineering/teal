#' @keywords internal
ui_validate_error <- function(id) {
  ns <- NS(id)
  # uiOutput(NS(id, ns("message")))
  tagList(
    ui_module_validate_shinysilenterror(ns("validate_shinysilenterror")),
    ui_module_validate_reactive(ns("validate_reactive")),
    ui_module_validate_qenverror(ns("validate_qenverror")),
    ui_module_validate_error(ns("validate_error"))
  )
}

#' @keywords internal
srv_validate_error <- function(id, data, validate_shiny_silent_error) {
  checkmate::assert_string(id)
  checkmate::assert_flag(validate_shiny_silent_error)
  moduleServer(id, function(input, output, session) {
    srv_module_validate_shinysilenterror(data)
    srv_module_validate_reactive(data)
    srv_module_validate_qenverror(data)
    srv_module_validate_error(data)
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

# #############################################################################
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
# ############################################################################

srv_module_validate_reactive <- function(x, types = character(0L), null.ok = FALSE) {
  moduleServer("validate_reactive", function(input, output, session) {
    collection <- list()
    collection <- append(collection, srv_module_check_reactive(x, types = types, null.ok = null.ok))

    validate_r <- reactive({
      message_collection <- clean_collection(collection)
      validate(need(length(message_collection) == 0, message_collection))
      TRUE
    })

    output$errors <- renderUI({
      validate_r()
      NULL
    })


    x
  })
}

ui_module_validate_reactive <- function(id) uiOutput(NS(id, "errors"))

srv_module_check_reactive <- function(x, types = character(0L), null.ok = FALSE) {
  reactive_message <- check_reactive(x, null.ok = null.ok)
  moduleServer("check_reactive", function(input, output, session) {

    reactive({
      if (isTRUE(reactive_message)) {
        if (length(types) > 0 && !inherits(x(), types)) {
          sprintf(
            "Reactive value's class may only of the following types: %s, but it is '%s'",
            paste("{", types, "}", sep = "", collapse = ", "),
            class(x())
          )
        } else {
          TRUE
        }
      } else {
        reactive_message
      }
    })
  })
}

# ###########################################################################
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
# ##########################################################################

srv_module_validate_shinysilenterror <- function(x) {
  moduleServer("validate_shinysilenterror", function(input, output, session) {
    collection <- list()
    collection <- append(collection, srv_module_check_shinysilenterror(x))

    validate_r <- reactive({
      message_collection <- clean_collection(collection)
      validate(need(length(message_collection) == 0, message_collection))
      TRUE
    })

    output$errors <- renderUI({
      validate_r()
      NULL
    })

    x
  })
}

ui_module_validate_shinysilenterror <- function(id) uiOutput(NS(id, "errors"))

srv_module_check_shinysilenterror <- function(x) {
  moduleServer("check_shinysilenterror", function(input, output, session) {
    reactive({
      if (inherits(x(), "shiny.silent.error") && identical(x()$message, "")) {
        "Shiny silent error was raised"
      } else {
        TRUE
      }
    })
  })
}

# ###############################################################
#
#              _ _     _       _
#             | (_)   | |     | |
#  __   ____ _| |_  __| | __ _| |_ ___    __ _  ___ _ ____   __
#  \ \ / / _` | | |/ _` |/ _` | __/ _ \  / _` |/ _ \ '_ \ \ / /
#   \ V / (_| | | | (_| | (_| | ||  __/ | (_| |  __/ | | \ V /
#    \_/ \__,_|_|_|\__,_|\__,_|\__\___|  \__, |\___|_| |_|\_/
#                                           | |
#                                           |_|
#
#  validate qenv
# ##############################################################

srv_module_validate_qenverror <- function(x) {
  srv_module_validate_generic("validate_qenverror", srv_module_check_qenverror, x)
}

ui_module_validate_qenverror <- function(id) uiOutput(NS(id, "errors"))

srv_module_check_qenverror <- function(x) {
  moduleServer("check_qenverror", function(input, output, session) {

    reactive({
      if (inherits(x(), "qenv.error")) {
        c(
          "Error when executing the `data` module:",
          cli::ansi_strip(x()$message),
          "",
          "Check your inputs or contact app developer if error persists."
        )
      } else {
        TRUE
      }
    })
  })
}


#

srv_module_validate_error <- function(x) {
  srv_module_validate_generic("validate_error", srv_module_check_error, x)
}

ui_module_validate_error <- function(id) {
  uiOutput(NS(id, "errors"))
}

srv_module_check_error <- function(x) {
  moduleServer("check_error", function(input, output, session) {

    reactive({
      if (inherits(x(), "error") && !inherits(x(), c("qenv.error", "shiny.silent.error"))) {
        c("Error detected", x()$message)
      } else {
        TRUE
      }
    })
  })
}

# Aux

srv_module_validate_generic <- function(id, fun, x, ...) {
  moduleServer(id, function(input, output, session) {
    collection <- list()
    collection <- append(collection, fun(x, ...))

    validate_r <- reactive({
      message_collection <- clean_collection(collection)
      validate(need(length(message_collection) == 0, message_collection))
      TRUE
    })

    output$errors <- renderUI({
      validate_r()
      NULL
    })

    x
  })
}

clean_collection <- function(collection) {
  Reduce(
    function(u, v) {
      el <- v()
      if (isTRUE(el)) {
        u
      } else {
        c(u, el)
      }
    },
    x = collection,
    init = c()
  )
}
