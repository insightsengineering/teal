#' Data Module for teal
#'
#' This module manages the `data` argument for `srv_teal`. The `teal` framework uses [teal.data::teal_data()],
#' which can be provided in various ways:
#' 1. Directly as a [teal.data::teal_data()] object. This will automatically convert it into a `reactive` `teal_data`.
#' 2. As a `reactive` object that returns a [teal.data::teal_data()] object.
#'
#' @details
#' ## Reactive `teal_data`:
#'
#' The data in the application can be reactively updated, prompting [srv_teal()] to rebuild the
#' content accordingly. There are two methods for creating interactive `teal_data`:
#' 1. Using a `reactive` object provided from outside the `teal` application. In this scenario,
#' reactivity is controlled by an external module, and `srv_teal` responds to changes.
#' 2. Using [teal_data_module()], which is embedded within the `teal` application, allowing data to
#' be resubmitted by the user as needed.
#'
#' Since the server of [teal_data_module()] must return a `reactive` `teal_data` object, both
#' methods (1 and 2) produce the same reactive behavior within a `teal` application. The distinction
#' lies in data control: the first method involves external control, while the second method
#' involves control from a custom module within the app.
#'
#' For more details, see [`module_teal_data`].
#'
#' @inheritParams module_teal
#'
#' @return A `reactive` object that returns:
#' Output of the `data`. If `data` fails then returned error is handled (after [tryCatch()]) so that
#' rest of the application can respond to this respectively.
#'
#' @rdname module_init_data
#' @name module_init_data
#' @keywords internal
NULL

#' @rdname module_init_data
ui_init_data <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    id = ns("content"),
    style = "display: inline-block; width: 100%;",
    uiOutput(ns("data"))
  )
}

#' @rdname module_init_data
srv_init_data <- function(id, data) {
  checkmate::assert_character(id, max.len = 1, any.missing = FALSE)
  checkmate::assert_multi_class(data, c("teal_data", "teal_data_module", "reactive"))

  moduleServer(id, function(input, output, session) {
    logger::log_debug("srv_data initializing.")
    data_out <- if (inherits(data, "teal_data_module")) {
      output$data <- renderUI(data$ui(id = session$ns("teal_data_module")))
      data$server("teal_data_module")
    } else if (inherits(data, "teal_data")) {
      reactiveVal(data)
    } else if (test_reactive(data)) {
      data
    }

    data_handled <- reactive({
      tryCatch(data_out(), error = function(e) e)
    })

    # We want to exclude teal_data_module elements from bookmarking as they might have some secrets
    observeEvent(data_handled(), {
      if (inherits(data_handled(), "teal_data")) {
        app_session <- .subset2(shiny::getDefaultReactiveDomain(), "parent")
        setBookmarkExclude(
          session$ns(
            grep(
              pattern = "teal_data_module-",
              x = names(reactiveValuesToList(input)),
              value = TRUE
            )
          ),
          session = app_session
        )
      }
    })

    data_handled
  })
}

#' Adds signature protection to the `datanames` in the data
#' @param data (`teal_data`)
#' @return `teal_data` with additional code that has signature of the `datanames`
#' @keywords internal
.add_signature_to_data <- function(data) {
  hashes <- .get_hashes_code(data)
  tdata <- do.call(
    teal.data::teal_data,
    c(
      list(code = trimws(c(teal.code::get_code(data), hashes), which = "right")),
      list(join_keys = teal.data::join_keys(data)),
      as.list(data, all.names = TRUE)
    )
  )
  tdata@verified <- data@verified
  tdata
}

#' Get code that tests the integrity of the reproducible data
#'
#' @param data (`teal_data`) object holding the data
#' @param datanames (`character`) names of `datasets`
#'
#' @return A character vector with the code lines.
#' @keywords internal
#'
.get_hashes_code <- function(data, datanames = names(data)) {
  vapply(
    datanames,
    function(dataname, datasets) {
      x <- data[[dataname]]

      code <- if (is.function(x) && !is.primitive(x)) {
        x <- deparse1(x)
        bquote(rlang::hash(deparse1(.(as.name(dataname)))))
      } else {
        bquote(rlang::hash(.(as.name(dataname))))
      }
      sprintf(
        "stopifnot(%s == %s) # @linksto %s",
        deparse1(code),
        deparse1(rlang::hash(x)),
        dataname
      )
    },
    character(1L),
    USE.NAMES = TRUE
  )
}
