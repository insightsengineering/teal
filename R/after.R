#' Executes modifications to the result of a module
#'
#'
#' `r lifecycle::badge("experimental")`
#' Modify the module's ui or server functions.
#' Primary used to modify the output of the report.
#'
#' @details
#' This function could overwrite the input or output of existing modules, or
#' show different code on report than the one used on the module.
#'
#' @param x (`teal_module` or `teal_modules`), which in case `teal_modules` will apply
#' `after` to each module in the list.
#' @param ui (`function(id, elem, ...)`) function to receive output (`shiny.tag`) from `x$ui`.
#' @param server (`function(input, output, session, data, ...)`) function to receive output data from `x$server`.
#' @param ... additional argument passed to `ui` and `server` by matching their formals names.
#' @return A `teal_module` object with the modifications.
#' New element ids are under `wrapper` namespace, old elements' ids are on the `wrapped` namespace.
#' @export
#' @seealso To modify just the output see [`teal_transform_module`].
#' @examples
#' library("teal.reporter")
#' app <- init(
#'   data = teal_data(IRIS = iris, MTCARS = mtcars),
#'   modules = example_module() |>
#'     after(
#'       ui = function(id, elem) {
#'         ns <- NS(id)
#'         check_box <- checkboxInput(ns("src"), "Include R Code in the report", TRUE)
#'         htmltools::tagAppendChild(elem, check_box,
#'           .cssSelector = ".standard-layout .sidebar .sidebar-content"
#'         )
#'       },
#'       server = function(input, output, session, data) {
#'         teal_card(data) <- c(teal_card(data), teal_card("Modification"))
#'         if (!input$`wrapper-src`) {
#'           teal_card(data) <- Filter(function(x) !inherits(x, "code_chunk"), teal_card(data))
#'         }
#'         data
#'       }
#'     )
#' )
#' if (interactive()) {
#'   runApp(app)
#' }
after <- function(x,
                  ui = function(id, elem) elem,
                  server = function(input, output, session, data) data,
                  ...) {
  UseMethod("after", x)
}

#' @export
after.teal_modules <- function(x,
                               ui = function(id, elem) elem,
                               server = function(input, output, session, data) data,
                               ...) {
  checkmate::assert_multi_class(x, "teal_modules")
  x$children <- lapply(x$children, after, ui = ui, server = server, ...)
  x
}

#' @export
after.teal_module <- function(x,
                              ui = function(id, elem) elem,
                              server = function(input, output, session, data) data,
                              ...) {
  # todo: make a method for teal_app and remove teal_extend_server?
  checkmate::assert_multi_class(x, "teal_module")
  # Check ui && server have required arguments but nothing else
  if (!is.function(ui) || !all(names(formals(ui)) %in% c("id", "elem"))) {
    stop("ui should be a function of `id` and `elem`.")
  }
  if (!is.function(server) || !all(names(formals(server)) %in% c("input", "output", "session", "data"))) {
    stop("server should be a function of `input`, `output`, `session` and `data`")
  }

  additional_args <- list(...)
  new_x <- x # because overwriting x$ui/server will cause infinite recursion
  new_x$ui <- .after_ui(x$ui, ui, additional_args)
  new_x$server <- .after_server(x$server, server, additional_args)
  new_x
}

.after_ui <- function(old_ui, new_ui, additional_args) {
  # add `_`-prefix to make sure objects are not masked in the wrapper functions
  `_old_ui` <- old_ui # nolint: object_name.
  `_new_ui` <- new_ui # nolint: object_name.
  new_x <- function(id, ...) {
    original_args <- as.list(environment())
    if ("..." %in% names(formals(`_old_ui`))) {
      original_args <- c(original_args, list(...))
    }
    ns <- NS(id)
    original_args$id <- ns("wrapped")
    original_out <- do.call(`_old_ui`, original_args, quote = TRUE)

    wrapper_args <- c(
      additional_args,
      list(id = ns("wrapper"), elem = original_out)
    )
    do.call(`_new_ui`, args = wrapper_args[names(formals(`_new_ui`))])
  }
  formals(new_x) <- formals(`_old_ui`)
  new_x
}

.after_server <- function(old_server, new_server, additional_args) {
  # add `_`-prefix to make sure objects are not masked in the wrapper functions
  `_old_server` <- old_server # nolint: object_name.
  `_new_server` <- new_server # nolint: object_name.
  new_x <- function(id, ...) {
    original_args <- as.list(environment())
    original_args$id <- "wrapped"
    if ("..." %in% names(formals(`_old_server`))) {
      original_args <- c(original_args, list(...))
    }
    moduleServer(id, function(input, output, session) {
      original_out <- if (all(c("input", "output", "session") %in% names(formals(`_old_server`)))) {
        original_args$module <- `_old_server`
        do.call(shiny::callModule, args = original_args)
      } else {
        do.call(`_old_server`, original_args)
      }

      wrapper_args <- utils::modifyList(
        additional_args,
        list(
          id = "wrapper", input = input, output = output,
          session = session
        )
      )

      reactive({
        output <- if (is.reactive(original_out)) {
          original_out()
        } else {
          original_out
        }
        wrapper_args$data <- output
        do.call(`_new_server`, wrapper_args[names(formals(`_new_server`))], quote = TRUE)
      })
    })
  }
  formals(new_x) <- formals(old_server)
  new_x
}



#' Disable the sidebar globally for all modules
#'
#' Convenience function that disables the sidebar functionality globally
#' by hiding the sidebar and making the main content use full width.
#' This should be applied at the modules() level.
#' 
#' @param x (`teal_modules`) a `teal_modules` object.
#' @return A modified `teal_modules` object with disabled sidebar functionality.
#' @export
#' @examples
#' app <- init(
#'   data = within(teal_data(), iris <- iris),
#'   modules = modules(
#'     example_module(label = "example teal module")
#'   ) |> disable_sidebar()
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
disable_sidebar <- function(x) {
  checkmate::assert_class(x, "teal_modules")
  
  sidebar_css <- tags$style(HTML("
    /* Hide the entire sidebar globally */
    [id^='bslib-sidebar-'],
    aside[id^='bslib-sidebar-'],
    .sidebar {
      display: none !important;
      width: 0 !important;
      min-width: 0 !important;
    }
    /* Adjust the sidebar layout to use full width globally */
    .bslib-sidebar-layout {
      display: block !important;
      grid-template-columns: none !important;
    }
    .bslib-sidebar-layout > .main {
      width: 100% !important;
      max-width: 100% !important;
      margin-left: 0 !important;
      margin-right: 0 !important;
      padding-left: 0 !important;
      padding-right: 0 !important;
    }
    /* Ensure the main content container uses full width globally */
    .main,
    .main > *,
    .main .container-fluid,
    .main .row,
    .main .col {
      width: 100% !important;
      max-width: 100% !important;
    }
    /* Hide the collapse toggle button since sidebar is gone globally */
    .collapse-toggle {
      display: none !important;
    }
  "))

  modified_children <- lapply(x$children, function(module) {
    after(module, 
      ui = function(id, elem) {
        htmltools::tagAppendChild(elem, sidebar_css)
      }
    )
  })

  x$children <- modified_children
  x
}
