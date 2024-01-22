# This file adds a splash screen for delayed data loading on top of teal

#' UI to show a splash screen in the beginning, then delegate to [srv_teal()]
#'
#' @description `r lifecycle::badge("stable")`
#' The splash screen could be used to query for a password to fetch the data.
#' [init()] is a very thin wrapper around this module useful for end-users which
#' assumes that it is a top-level module and cannot be embedded.
#' This function instead adheres to the `shiny` module conventions.
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
                                title = build_app_title(),
                                header = tags$p(),
                                footer = tags$p()) {
  checkmate::assert_character(id, max.len = 1, any.missing = FALSE)
  checkmate::assert_multi_class(data, c("teal_data", "teal_data_module"))
  checkmate::assert(
    .var.name = "title",
    checkmate::check_string(title),
    checkmate::check_multi_class(title, c("shiny.tag", "shiny.tag.list", "html"))
  )
  checkmate::assert(
    .var.name = "header",
    checkmate::check_string(header),
    checkmate::check_multi_class(header, c("shiny.tag", "shiny.tag.list", "html"))
  )
  checkmate::assert(
    .var.name = "footer",
    checkmate::check_string(footer),
    checkmate::check_multi_class(footer, c("shiny.tag", "shiny.tag.list", "html"))
  )

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
#'   will be displayed in the `teal` application. See [modules()] and [module()] for
#'   more details.
#' @inheritParams shiny::moduleServer
#' @return `reactive` containing `teal_data` object when data is loaded.
#' If data is not loaded yet, `reactive` returns `NULL`.
#' @export
srv_teal_with_splash <- function(id, data, modules, filter = teal_slices()) {
  checkmate::assert_character(id, max.len = 1, any.missing = FALSE)
  checkmate::check_multi_class(data, c("teal_data", "teal_data_module"))
  checkmate::assert_class(modules, "teal_modules")
  checkmate::assert_class(filter, "teal_slices")

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
        stop("The `teal_data_module` passed to `data` must return a reactive expression.", call. = FALSE)
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
              "Error when executing `teal_data_module` passed to `data`:\n ",
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
              "Error when executing `teal_data_module` passed to `data`:\n ",
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
            "Error: `teal_data_module` passed to `data` failed to return `teal_data` object, returned",
            toString(sQuote(class(data))),
            "instead.",
            "\n Check your inputs or contact app developer if error persists."
          )
        )
      )

      if (!length(teal.data::datanames(data))) {
        warning("`data` object has no datanames. Default datanames are set using `teal_data`'s environment.")
      }

      is_modules_ok <- check_modules_datanames(modules, teal_data_datanames(data))
      if (!isTRUE(is_modules_ok)) {
        validate(need(isTRUE(is_modules_ok), sprintf("%s. Contact app developer.", is_modules_ok)))
      }

      is_filter_ok <- check_filter_datanames(filter, teal_data_datanames(data))
      if (!isTRUE(is_filter_ok)) {
        showNotification(
          "Some filters were not applied because of incompatibility with data. Contact app developer.",
          type = "warning",
          duration = 10
        )
        warning(is_filter_ok)
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
