#' `teal_data` transform/load module
#'
#' Module consumes `teal_data_module` elements and returns validated data:
#' - `srv/ui_teal_data_module`: executes a single `teal_data_module`
#' - `srv/ui_teal_data_modules` executes multiple `teal_data_module` elements successively by passing
#' output of previous module to the next one.
#'
#' This is a low level module to handle data-loading or data-transformation as in both cases output
#' is a reactive and validated `teal_data`. Data loading can be considered as transformation module
#' of empty (initial) data object.
#'
#' Output `reactive` `teal_data` is validated by [`validate_reactive_teal_data`].
#' Module makes sure that returned data doesn't break an app, so the [.fallback_on_failure()] is
#' implemented.
#'
#' @param id (`character(1)`) Module id
#' @param data (`reactive teal_data`)
#' @param transformers,transformer (`list of teal_data_module` or `teal_data_module`)
#' @param modules (`teal_modules` or `teal_module`) For `datanames` validation purpose
#' @param validate_shiny_silent_error (`logical(1)`)
#' @param class (`character`) Additional CSS class for whole wrapper div (optional)
#'
#' @return `reactive` `teal_data`
#'
#' @rdname module_teal_data_module
#' @name module_teal_data_module
#' @keywords internal
NULL

#' @rdname module_teal_data_module
#' @keywords internal
ui_teal_data_modules <- function(id, transformers, class = "") {
  checkmate::assert_string(id)
  checkmate::assert_list(transformers, "teal_data_module", null.ok = TRUE)
  ns <- NS(id)

  labels <- lapply(transformers, function(x) attr(x, "label"))
  ids <- get_unique_labels(labels)
  names(transformers) <- ids

  lapply(
    names(transformers),
    function(name) {
      data_mod <- transformers[[name]]
      div( # todo: accordion?
        # class .teal_validated changes the color of the boarder on error in ui_validate_reactive_teal_data
        #   For details see tealValidate.js file.
        class = c(class, "teal_validated"),
        title = attr(data_mod, "label"),
        tags$span(
          class = "text-primary mb-4",
          icon("square-pen", lib = "font-awesome"),
          attr(data_mod, "label")
        ),
        actionLink(
          inputId = ns(sprintf("minimize_%s", name)),
          label = NULL,
          icon = icon("angle-down", lib = "font-awesome"),
          title = "Minimise panel",
          class = "remove pull-right"
        ),
        div(
          id = ns(sprintf("wrapper_%s", name)),
          ui_teal_data_module(id = ns(name), transformer = transformers[[name]])
        )
      )
    }
  )
}

#' @rdname module_teal_data_module
#' @keywords internal
srv_teal_data_modules <- function(id, data, transformers, modules) {
  checkmate::assert_string(id)
  checkmate::assert_class(data, "reactive")
  checkmate::assert_list(transformers, "teal_data_module", null.ok = TRUE)
  checkmate::assert_class(modules, "teal_module")

  if (length(transformers) == 0L) {
    return(data)
  }

  labels <- lapply(transformers, function(x) attr(x, "label"))
  ids <- get_unique_labels(labels)
  names(transformers) <- ids

  moduleServer(id, function(input, output, session) {
    logger::log_trace("srv_teal_data_modules initializing.")
    # todo: move this to javascript so that server only returns data
    lapply(
      names(transformers),
      function(name) {
        element_id <- sprintf("minimize_%s", name)
        observeEvent(input[[element_id]], {
          shinyjs::toggle(sprintf("wrapper_%s", name))
          toggle_icon(session$ns(element_id), c("fa-angle-right", "fa-angle-down"))
          toggle_title(session$ns(element_id), c("Restore panel", "Minimise Panel"))
        })
      }
    )

    Reduce(
      function(x, name) {
        srv_teal_data_module(
          id = name,
          data = x,
          transformer = transformers[[name]],
          modules = modules
        )
      },
      names(transformers),
      init = data
    )
  })
}

#' @rdname module_teal_data_module
#' @keywords internal
ui_teal_data_module <- function(id, transformer) {
  checkmate::assert_string(id)
  checkmate::assert_class(transformer, "teal_data_module")
  ns <- NS(id)
  shiny::tagList(
    transformer$ui(id = ns("data")),
    ui_validate_reactive_teal_data(ns("validate"))
  )
}


#' @rdname module_teal_data_module
#' @keywords internal
srv_teal_data_module <- function(id,
                                 data,
                                 transformer,
                                 modules = NULL,
                                 validate_shiny_silent_error = TRUE) {
  checkmate::assert_string(id)
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(transformer, "teal_data_module")
  checkmate::assert_multi_class(modules, c("teal_modules", "teal_module"), null.ok = TRUE)

  moduleServer(id, function(input, output, session) {
    logger::log_trace("srv_teal_data_module initializing.")

    data_out <- if (is_arg_used(transformer$server, "data")) {
      transformer$server(id = "data", data = data)
    } else {
      transformer$server(id = "data")
    }
    data_validated <- srv_validate_reactive_teal_data(
      id = "validate",
      data = data_out,
      modules = modules,
      validate_shiny_silent_error = validate_shiny_silent_error
    )
    .fallback_on_failure(
      this = data_validated,
      that = data,
      label = sprintf("Data element '%s' for module '%s'", id, modules$label)
    )
  })
}

#' Fallback on failure
#'
#' Function returns the previous reactive if the current reactive is invalid (throws error or returns NULL).
#' Application: In `teal` we try to prevent the error from being thrown and instead we replace failing
#' transform module data output with data input from the previous module (or from previous `teal` reactive
#' tree elements).
#'
#' @param this (`reactive`) Current reactive
#' @param that (`reactive`) Previous reactive
#' @return `reactive` `teal_data`
.fallback_on_failure <- function(this, that, label) {
  checkmate::assert_class(this, "reactive")
  checkmate::assert_class(that, "reactive")
  checkmate::assert_string(label)
  reactive({
    evaluated <- tryCatch(this(), error = function(e) e)
    if (!inherits(evaluated, "error") && !is.null(evaluated)) {
      logger::log_trace("{ label } evaluated successfully.")
      this()
    } else {
      logger::log_trace("{ label } failed, falling back to previous data.")
      that()
    }
  })
}
