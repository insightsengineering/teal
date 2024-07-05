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
ui_teal_data_module <- function(id, transformers) {
  checkmate::assert_string(id)
  checkmate::assert_list(transformers, "teal_data_module", null.ok = TRUE)
  ns <- NS(id)
  lapply(
    seq_along(transformers),
    function(i) {
      data_mod <- transformers[[i]]
      div( # todo: accordion?
        title = attr(data_mod, "label"),
        tags$span(
          class= "text-primary mb-4",
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
          class = "has-error",
          span(
            class = "help-block",
            textOutput(ns(sprintf("error_%d", i)))
          )
        ),
        div(
          id = ns(sprintf("wrapper_data_%d", i)),
          data_mod$ui(id = ns(sprintf("data_%d", i)))
        )
      )
    }
  )
}

#' @rdname module_teal_module
#' @export
srv_teal_data_module <- function(id, data, transformers) {
  checkmate::assert_string(id)
  checkmate::assert_list(transformers, "teal_data_module", min.len = 0)

  moduleServer(id, function(input, output, session) {
#
#     observeEvent(input[[sprintf("minimize_%d", 1)]], {
#       print("me")
#       element_id <- sprintf("wrapper_data_%d", 1)
#       shinyjs::toggle(element_id)
#       teal.slice:::toggle_icon(session$ns(sprintf("minimize_%d", 1)), c("fa-angle-right", "fa-angle-down"))
#       teal.slice:::toggle_title(session$ns(sprintf("minimize_%d", 1)), c("Restore panel", "Minimise Panel"))
#     })

    lapply(
      seq_along(transformers),
      function(ix) {
        element_id <- sprintf("minimize_%d", ix)
        observeEvent(input[[element_id]], {
          shinyjs::toggle(sprintf("wrapper_data_%d", ix))
          teal.slice:::toggle_icon(session$ns(element_id), c("fa-angle-right", "fa-angle-down"))
          teal.slice:::toggle_title(session$ns(element_id), c("Restore panel", "Minimise Panel"))
        })
      }
    )

    Reduce(
      function(x, ix) {
        res <- transformers[[ix]]$server(id = sprintf("data_%d", ix), data = x)
        output[[sprintf("error_%d", ix)]] <- renderText({
          if (!inherits(x(), "qenv.error") && inherits(res(), "qenv.error")) {
            "An error occured with this transform. Please check the inputs."
          }
        })
        res
      },
      seq_along(transformers),
      init = data
    )
  })
}
