#' Executes modifications to the result of a module
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Exported to be able to use methods not to be used directly by module-developers or app-users.
#' Primarily used to modify the output object of module.
#' @seealso [disable_src()], [disable_report()]
#' @param tm (`teal_module`).
#' @param server (`function(input, output, session, data, ...)`) function to receive output data from `tm$server`.
#'  Must return data
#' @param ... additional argument passed to `ui` and `server` by matching their formals names.
#' @return A `teal_report` object with the result of the server function.
#' @export
#' @keywords internal
#' @noRd
#' @examples
#' library("teal.reporter")
#' hide_code <- function(input, output, session, data) {
#'   teal_card(data) <- Filter(function(x) !inherits(x, "code_chunk"), teal_card(data))
#'   data
#' }
#' app <- init(
#'   data = teal_data(IRIS = iris, MTCARS = mtcars),
#'   modules = example_module() |>
#'     after(server = hide_code)
#' )
#'
#' if (interactive()) {
#'   runApp(app)
#' }
after <- function(tm,
                  server = function(input, output, session, data) data,
                  ...) {
  UseMethod("after")
}


#' @export
after.default <- function(tm,
                          server = function(input, output, session, data) data,
                          ...) {
  stop("`after` is only implemented for `teal_module` and `teal_modules` objects.")
}

#' @export
after.teal_modules <- function(tm,
                               server = function(input, output, session, data) data,
                               ...) {
  tm$children <- lapply(tm$children, after,
    ui = function(id, elem) {
      elem
    }, server = server, ...
  )
  tm
}

#' @export
after.teal_module <- function(tm,
                              server = function(input, output, session, data) data,
                              ...) {
  checkmate::assert_multi_class(tm, "teal_module")

  names_srv <- names(formals(server))
  args_callModule <- c("input", "output", "session", "data") # nolint object_name_linter.
  if (!is.function(server) || !(!all(identical(names_srv, c("id", "data"))) || !all(names_srv %in% args_callModule))) {
    stop("server should be a function of `input`, `output`, `session` and `data`")
  }

  additional_args <- list(...)
  tm$ui <- after_ui(tm$ui, function(id, elem) {
    elem
  }, additional_args)
  tm$server <- after_srv(tm$server, server, additional_args)
  tm
  tm
}

after_ui <- function(old, new, additional_args) {
  new_ui <- function(id, ...) {
    original_args <- as.list(environment())
    if ("..." %in% names(formals(old))) {
      original_args <- c(original_args, list(...))
    }
    ns <- NS(id)
    original_args$id <- ns("wrapped")
    original_out <- do.call(old, original_args, quote = TRUE)

    wrapper_args <- c(
      additional_args,
      list(id = ns("wrapper"), elem = original_out)
    )
    do.call(new, args = wrapper_args[names(formals(new))])
  }
  formals(new_ui) <- formals(old)
  new_ui
}

after_srv <- function(old, new, additional_args) {
  new_srv <- function(id, ...) {
    original_args <- as.list(environment())
    original_args$id <- "wrapped"
    if ("..." %in% names(formals(old))) {
      original_args <- c(original_args, list(...))
    }
    moduleServer(id, function(input, output, session) {
      original_out <- if (all(c("input", "output", "session") %in% names(formals(old)))) {
        original_args$module <- old
        do.call(shiny::callModule, args = original_args)
      } else {
        do.call(old, original_args)
      }

      wrapper_args <- utils::modifyList(
        additional_args,
        list(id = "wrapper", input = input, output = output, session = session)
      )
      reactive({
        req(original_out())
        wrapper_args$data <- original_out()
        do.call(new, wrapper_args[names(formals(new))], quote = TRUE)
      })
    })
  }
  formals(new_srv) <- formals(old)
  new_srv
}
