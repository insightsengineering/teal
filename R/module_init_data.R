#' Data module for teal
#'
#' Module handles `data` argument to the `srv_teal`. `teal` uses [teal_data()] within
#' the whole framework and it could be provided in several way:
#' 1. Directly as a [teal.data::teal_data()] object. This will be automatically converted
#' to `reactive` `teal_data`.
#' 2. As a `reactive` object returning [teal.data::teal_data()]. [See section](#reactive-teal_data).
#'
#' @section Reactive `teal_data`:
#'
#' Data included to the application can be reactively changed and [srv_teal()] will rebuild
#' the content respectively. There are two ways of making interactive `teal_data`:
#' 1. Using a `reactive` object passed from outside the `teal` application. In this case, reactivity
#' is controlled by external module and `srv_teal` will trigger accordingly to the changes.
#' 2. Using [teal_data_module()] which is embedded in the `teal` application and data can be
#' resubmitted when needed by the user.
#'
#' Since server of [teal_data_module()] must return `reactive` `teal_data` object, it means that
#' both scenarios (1) and (2) are having the same effect for the reactivity of a `teal` application.
#' The difference is that in the first case the data is controlled from outside the app and in the
#' second case the data is controlled from custom module called inside of the app.
#'
#' see [`module_teal_data`] for more details.
#'
#' @inheritParams init
#'
#' @param data (`teal_data`, `teal_data_module` or `reactive` returning `teal_data`)
#' @return A `reactive` which returns:
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
    style = "display: inline-block;",
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
  checkmate::assert_multi_class(data, c("teal_data", "teal_data_module", "reactive", "reactiveVal"))
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
        data = reactive(req(FALSE)), # to .fallback_on_failure to shiny.silent.error
        data_module = data,
        modules = modules,
        validate_shiny_silent_error = FALSE
      )
    } else if (inherits(data, "teal_data")) {
      reactiveVal(data)
    } else if (inherits(data, c("reactive", "reactiveVal"))) {
      .fallback_on_failure(this = data, that = reactive(req(FALSE)), label = "Reactive data")
    }

    if (inherits(data, "teal_data_module")) {
      shinyjs::disable(selector = sprintf(".teal-body:has('#%s') .nav li a", session$ns("content")))
    }

    observeEvent(data_validated(), {
      showNotification("Data loaded successfully.", duration = 5)
      shinyjs::enable(selector = sprintf(".teal-body:has('#%s') .nav li a", session$ns("content")))
      if (isTRUE(attr(data, "once"))) {
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

      is_filter_ok <- check_filter_datanames(filter, teal_data_datanames(data_validated()))
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

    # Adds signature protection to the datanames in the data
    reactive(.add_signature_to_data(data_validated()))
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
.get_hashes_code <- function(data, datanames = teal_data_datanames(data)) {
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
