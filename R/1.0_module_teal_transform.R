#' Execute teal_data_module
#'
#' Function executes the `teal_data_module` and returns the modified data.
#' Modules' execution order is determined by the order provided to `...` argument.
#' Reactive data output of the previous module is used as input for the next module, so the final
#' data is the product of all consecutive transformations.
#' @name module_teal_transform_module
#'
#' @param class (`character`) Additional CSS class for whole wrapper div (optional)
#'
#' @return `reactive` `teal_data`
#' @export
ui_teal_transform_module <- function(id, transformers, class = "") {
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
          ui_teal_data(id = ns(name), transformer = transformers[[name]])
        )
      )
    }
  )
}

#' @rdname module_teal_module
#' @export
srv_teal_transform_module <- function(id, data, transformers, modules) {
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
    logger::log_trace("srv_teal_transform_module initializing.")
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
        srv_teal_data(
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

#' `teal_data` transform/load module
#'
#'
#' @param id (`character(1)`) Module id
#' @param data (`reactive teal_data`)
#' @param transformer (`list of teal_data_module` or `teal_data_module`)
#' @param modules (`teal_modules` or `teal_module`) For `datanames` validation purpose
#'
#' @return `reactive` `teal_data`
#'
#' @rdname module_teal_data
#' @name module_teal_data
#' @keywords internatl
ui_teal_data <- function(id, transformer) {
  checkmate::assert_string(id)
  checkmate::assert_class(transformer, "teal_data_module")
  ns <- NS(id)
  shiny::tagList(
    transformer$ui(id = ns("data")),
    ui_validate_reactive_teal_data(ns("validate"))
  )
}

# todo: filter formal can be moved away from here as it doesn't throw validate error (just notification)
# it could be moved to more appropriate place (place related with filter panel)
srv_teal_data <- function(id,
                          data,
                          transformer,
                          modules = NULL,
                          validate_shiny_silent_error = TRUE) {
  checkmate::assert_string(id)
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(transformer, "teal_data_module")
  checkmate::assert_multi_class(modules, c("teal_modules", "teal_module"), null.ok = TRUE)

  moduleServer(id, function(input, output, session) {
    logger::log_trace("srv_teal_data initializing.")

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
