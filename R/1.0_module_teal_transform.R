#' Execute teal_data_module
#'
#' Function executes the `teal_data_module` and returns the modified data.
#' Modules' execution order is determined by the order provided to `...` argument.
#' Reactive data output of the previous module is used as input for the next module, so the final
#' data is the product of all consecutive transformations.
#' @name module_teal_module
#'
#' @param id (`character(1)`) Module id
#' @param data (`reactive`) `teal_data`
#' @param ... (`teal_data_module`)
#'
#' @return `reactive` `teal_data`
#' @export
ui_teal_data_module <- function(id, transformers, class = "") {
  checkmate::assert_string(id)
  checkmate::assert_list(transformers, "teal_data_module", null.ok = TRUE)
  ns <- NS(id)
  lapply(
    seq_along(transformers),
    function(i) {
      data_mod <- transformers[[i]]
      div( # todo: accordion?
        class = class,
        title = attr(data_mod, "label"),
        tags$span(
          class = "text-primary mb-4",
          icon("square-pen", lib = "font-awesome"),
          attr(data_mod, "label")
        ),
        actionLink(
          inputId = ns(sprintf("minimize_%d", i)),
          label = NULL,
          icon = icon("angle-down", lib = "font-awesome"),
          title = "Minimise panel",
          class = "remove pull-right"
        ),
        div(
          id = ns(sprintf("wrapper_data_%d", i)),
          data_mod$ui(id = ns(sprintf("data_%d", i))),
          ui_validate_reactive_teal_data(ns(sprintf("validate_%d", i)))
        )
      )
    }
  )
}

#' @rdname module_teal_module
#' @export
srv_teal_data_module <- function(id, teal_data, transformers, modules) {
  checkmate::assert_string(id)
  checkmate::assert_class(teal_data, "reactive")
  checkmate::assert_list(transformers, "teal_data_module", min.len = 0)
  checkmate::assert_class(modules, "teal_module")

  moduleServer(id, function(input, output, session) {
    lapply(
      seq_along(transformers),
      function(i) {
        element_id <- sprintf("minimize_%d", i)
        observeEvent(input[[element_id]], {
          shinyjs::toggle(sprintf("wrapper_data_%d", i))
          teal.slice:::toggle_icon(session$ns(element_id), c("fa-angle-right", "fa-angle-down"))
          teal.slice:::toggle_title(session$ns(element_id), c("Restore panel", "Minimise Panel"))
        })
      }
    )

    Reduce(
      function(x, i) {
        data <- transformers[[i]]$server(id = sprintf("data_%d", i), data = x)
        data_validated <- srv_validate_reactive_teal_data(
          id = sprintf("validate_%d", i),
          data = data,
          modules = modules(modules)
        )
        reactive({
          if (!inherits(tryCatch(data_validated(), error = function(e) e), "error")) {
            data_validated()
          } else {
            x()
          }
        })
      },
      seq_along(transformers),
      init = teal_data
    )
  })
}
