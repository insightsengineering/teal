# This file adds a splash screen for delayed data loading on top of teal

#' UI to show a splash screen in the beginning, then delegate to [srv_teal()]
#'
#' @description `r lifecycle::badge("stable")`
#' The splash screen could be used to query for a password to fetch the data.
#' [init()] is a very thin wrapper around this module useful for end-users which
#' assumes that it is a top-level module and cannot be embedded.
#' This function instead adheres to the Shiny module conventions.
#'
#' If data is obtained through delayed loading, its splash screen is used. Otherwise,
#' a default splash screen is shown.
#'
#' Please also refer to the doc of [init()].
#'
#' @param id (`character(1)`)\cr
#'   module id
#' @inheritParams init
#' @export
ui_teal_with_splash <- function(id,
                                data,
                                title,
                                header = tags$p("Add Title Here"),
                                footer = tags$p("Add Footer Here")) {
  checkmate::assert_multi_class(data, c("teal_data", "teal_data_module"))
  ns <- NS(id)

  # Startup splash screen for delayed loading
  # We use delayed loading in all cases, even when the data does not need to be fetched.
  # This has the benefit that when filtering the data takes a lot of time initially, the
  # Shiny app does not time out.
  splash_ui <- if (inherits(data, "teal_data_module")) {
    data$ui(ns("teal_data_module"))
  } else if (inherits(data, "teal_data")) {
    div()
  }
  ui_teal(
    id = ns("teal"),
    splash_ui = div(splash_ui, uiOutput(ns("error"))),
    title = title,
    header = header,
    footer = footer
  )
}

#' Server function that loads the data through reactive loading and then delegates
#' to [srv_teal()].
#'
#' @description `r lifecycle::badge("stable")`
#' Please also refer to the doc of [init()].
#'
#' @inheritParams init
#' @param modules `teal_modules` object containing the output modules which
#'   will be displayed in the teal application. See [modules()] and [module()] for
#'   more details.
#' @inheritParams shiny::moduleServer
#' @return `reactive` containing `teal_data` object when data is loaded.
#' If data is not loaded yet, `reactive` returns `NULL`.
#' @export
srv_teal_with_splash <- function(id, data, modules, filter = teal_slices()) {
  checkmate::check_multi_class(data, c("teal_data", "teal_data_module"))

  moduleServer(id, function(input, output, session) {
    logger::log_trace("srv_teal_with_splash initializing module with data.")

    if (getOption("teal.show_js_log", default = FALSE)) {
      shinyjs::showLog()
    }

    # teal_data_rv contains teal_data object
    # either passed to teal::init or returned from teal_data_module
    teal_data_rv <- if (inherits(data, "teal_data_module")) {
      data <- data$server(id = "teal_data_module")
      if (!is.reactive(data)) {
        stop("The `teal_data_module` must return a reactive expression.", call. = FALSE)
      }
      data
    } else if (inherits(data, "teal_data")) {
      reactiveVal(data)
    }

    teal_data_rv_validate <- reactive({
      # custom module can return error
      data <- tryCatch(teal_data_rv(), error = function(e) e)

      # there is an empty reactive cycle on init!
      if (inherits(data, "shiny.silent.error") && identical(data$message, "")) {
        return(NULL)
      }

      # to handle qenv.error
      if (inherits(data, "qenv.error")) {
        validate(
          need(
            FALSE,
            paste(
              "Error when executing `teal_data_module`:\n ",
              paste(data$message, collapse = "\n"),
              "\n Check your inputs or contact app developer if error persists."
            )
          )
        )
      }

      # to handle module non-qenv errors
      if (inherits(data, "error")) {
        validate(
          need(
            FALSE,
            paste(
              "Error when executing `teal_data_module`:\n ",
              paste(data$message, collpase = "\n"),
              "\n Check your inputs or contact app developer if error persists."
            )
          )
        )
      }

      validate(
        need(
          inherits(data, "teal_data"),
          paste(
            "Error: `teal_data_module` did not return `teal_data` object",
            "\n Check your inputs or contact app developer if error persists"
          )
        )
      )

      if (!length(teal.data::datanames(data))) {
        warning("`data` object has no datanames. Default datanames are set using `teal_data`'s environment.")
      }

      is_modules_ok <- check_modules_datanames(modules, teal_data_datanames(data))
      if (!isTRUE(is_modules_ok)) {
        logger::log_warn(is_modules_ok)
      }

      is_filter_ok <- check_filter_datanames(filter, teal_data_datanames(data))
      if (!isTRUE(is_filter_ok)) {
        showNotification(
          "Some filters were not applied because of incompatibility with data. Contact app developer.",
          type = "warning",
          duration = 10
        )
        logger::log_warn(is_filter_ok)
      }

      teal_data_rv()
    })

    output$error <- renderUI({
      teal_data_rv_validate()
      NULL
    })


    res <- srv_teal(id = "teal", modules = modules, teal_data_rv = teal_data_rv_validate, filter = filter)
    logger::log_trace("srv_teal_with_splash initialized module with data.")
    return(res)
  })
}
