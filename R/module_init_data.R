#' Data Module for teal
#'
#' This module manages the `data` argument for `srv_teal`. The `teal` framework uses [teal_data()],
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
#' @inheritParams init
#'
#' @param data (`teal_data`, `teal_data_module`, or `reactive` returning `teal_data`)
#' The `ui` component of this module does not require `data` if `teal_data_module` is not provided.
#' The `data` argument in the `ui` is included solely for the `$ui` function of the
#' `teal_data_module`. Otherwise, it can be disregarded, ensuring that `ui_teal` does not depend on
#' the reactive data of the enclosing application.
#'
#' @return A `reactive` object that returns:
#' - `teal_data` when the object is validated
#' - `shiny.silent.error` when not validated.
#'
#' @rdname module_init_data
#' @name module_init_data
#' @keywords internal
NULL

#' @rdname module_init_data
ui_init_data <- function(id, data) {
  ns <- shiny::NS(id)
  shiny::div(
    id = ns("content"),
    style = "display: inline-block; width: 100%;",
    if (inherits(data, "teal_data_module")) {
      ui_teal_data(ns("teal_data_module"), data_module = data)
    } else {
      NULL
    }
  )
}

#' @rdname module_init_data
srv_init_data <- function(id, data, modules, filter = teal_slices()) {
  checkmate::assert_character(id, max.len = 1, any.missing = FALSE)
  checkmate::assert_multi_class(data, c("teal_data", "teal_data_module", "reactive"))
  checkmate::assert_class(modules, "teal_modules")
  checkmate::assert_class(filter, "teal_slices")

  moduleServer(id, function(input, output, session) {
    logger::log_debug("srv_data initializing.")

    if (getOption("teal.show_js_log", default = FALSE)) {
      shinyjs::showLog()
    }

    # data_rv contains teal_data object
    # either passed to teal::init or returned from teal_data_module
    data_validated <- if (inherits(data, "teal_data_module")) {
      srv_teal_data(
        "teal_data_module",
        data = reactive(teal_data()),
        data_module = data,
        modules = modules,
        validate_shiny_silent_error = FALSE
      )
    } else if (inherits(data, "teal_data")) {
      reactiveVal(data)
    } else if (test_reactive(data)) {
      data
    }

    if (inherits(data, "teal_data_module")) {
      shinyjs::disable(selector = sprintf(".teal-body:has('#%s') .nav li a", session$ns("content")))
    }

    observeEvent(data_validated(), {
      req(inherits(data_validated(), "teal_data"))

      if (isTRUE(attr(data, "once")) && !.is_empty_teal_data(data_validated())) {
        # Hiding the data module tab.
        shinyjs::hide(
          selector = sprintf(
            ".teal-body:has('#%s') a[data-value='teal_data_module']",
            session$ns("content")
          )
        )
        # Clicking the second tab, which is the first module.
        shinyjs::runjs(
          sprintf(
            "document.querySelector('.teal-body:has(#%s) .nav li:nth-child(2) a').click();",
            session$ns("content")
          )
        )
      }

      if (.is_empty_teal_data(data_validated())) {
        shinyjs::disable(selector = sprintf(".teal-body:has('#%s') .nav li a", session$ns("content")))
      }
      is_filter_ok <- check_filter_datanames(filter, .teal_data_datanames(data_validated()))
      if (!isTRUE(is_filter_ok)) {
        showNotification(
          "Some filters were not applied because of incompatibility with data. Contact app developer.",
          type = "warning",
          duration = 10
        )
        warning(is_filter_ok)
      }
    })

    observeEvent(data_validated(), once = TRUE, {
      # Excluding the ids from teal_data_module using full namespace and global shiny app session.
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
    })

    reactive({
      if (inherits(data_validated(), "teal_data")) {
        .add_signature_to_data(data_validated())
      } else if (inherits(data_validated(), "qenv.error")) {
        data_validated()
      } else {
        teal_data()
      }
    })
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
      sapply(
        ls(teal.code::get_env(data)),
        teal.code::get_var,
        object = data,
        simplify = FALSE
      )
    )
  )

  tdata@verified <- data@verified
  teal.data::datanames(tdata) <- teal.data::datanames(data)
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
.get_hashes_code <- function(data, datanames = .teal_data_datanames(data)) {
  # todo: this should be based on data_rv object not on datasets
  vapply(
    datanames,
    function(dataname, datasets) {
      hash <- rlang::hash(data[[dataname]])
      sprintf(
        "stopifnot(%s == %s) # @linksto %s",
        deparse1(bquote(rlang::hash(.(as.name(dataname))))),
        deparse1(hash),
        dataname
      )
    },
    character(1L),
    USE.NAMES = TRUE
  )
}
