#' Executes modifications to the result of a module
#'
#' Primarily used to modify the output object of module to change the containing
#' report.
#' @param x (`teal_data`)
#' @param ui (`function(id, elem, ...)`) function to receive output (`shiny.tag`) from `x$ui`
#' @param server (`function(input, output, session, data, ...)`) function to receive output data from `x$server`
#' @param ... additional argument passed to `ui` and `server` by matching their formals names.
#' @return A `teal_report` object with the result of the server function.
#' @export
after <- function(x,
                  ui = function(id, elem) elem,
                  server = function(input, output, session, data) data,
                  ...) {
  # todo: make a method for teal_app and remove teal_extend_server?
  checkmate::assert_multi_class(x, "teal_module")
  if (!is.function(ui) || !all(names(formals(ui)) %in% c("id", "elem"))) {
    stop("ui should be a function of id and elem")
  }
  if (!is.function(server) || !all(names(formals(server)) %in% c("input", "output", "session", "data"))) {
    stop("server should be a function of `input` and `output`, `session`, `data`")
  }

  additional_args <- list(...)
  new_x <- x # because overwriting x$ui/server will cause infinite recursion
  new_x$ui <- .after_ui(x$ui, ui, additional_args)
  new_x$server <- .after_server(x$server, server, additional_args)
  new_x
}

.after_ui <- function(x, y, additional_args) {
  # add `_`-prefix to make sure objects are not masked in the wrapper functions
  `_x` <- x # nolint: object_name.
  `_y` <- y # nolint: object_name.
  new_x <- function(id, ...) {
    original_args <- as.list(environment())
    if ("..." %in% names(formals(`_x`))) {
      original_args <- c(original_args, list(...))
    }
    ns <- NS(id)
    original_args$id <- ns("wrapped")
    original_out <- do.call(`_x`, original_args, quote = TRUE)

    wrapper_args <- c(
      additional_args,
      list(id = ns("wrapper"), elem = original_out)
    )
    do.call(`_y`, args = wrapper_args[names(formals(`_y`))])
  }
  formals(new_x) <- formals(x)
  new_x
}

.after_server <- function(x, y, additional_args) {
  # add `_`-prefix to make sure objects are not masked in the wrapper functions
  `_x` <- x # nolint: object_name.
  `_y` <- y # nolint: object_name.
  new_x <- function(id) {
    original_args <- as.list(environment())
    original_args$id <- "wrapped"
    if ("..." %in% names(formals(`_x`))) {
      original_args <- c(original_args, list(...))
    }
    moduleServer(id, function(input, output, session) {
      original_out <- if (all(c("input", "output", "session") %in% names(formals(`_x`)))) {
        original_args$module <- `_x`
        do.call(call_module, args = original_args)
      } else {
        do.call(`_x`, original_args)
      }
      original_out_r <- reactive(
        if (is.reactive(original_out)) {
          original_out()
        } else {
          original_out
        }
      )
      wrapper_args <- utils::modifyList(
        additional_args,
        list(id = "wrapper", input = input, output = output, session = session)
      )
      reactive({
        req(original_out_r())
        wrapper_args$data <- original_out()
        do.call(`_y`, wrapper_args[names(formals(`_y`))], quote = TRUE)
      })
    })
  }
  formals(new_x) <- formals(x)
  new_x
}
