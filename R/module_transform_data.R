#' Apply `teal_transform_module` decorators to reactive `teal_data`
#'
#' @description
#' Shiny module pair (`srv_transform_teal_data` / `ui_transform_teal_data`) that runs a sequence
#' of [teal_transform_module()] decorators against a reactive `teal_data` object.
#' Decorators are applied one after another via `Reduce`, so each one receives the output of the
#' previous as its `data` argument.
#' Failed transformators are tracked and any downstream transformator is automatically disabled
#' until the failure is resolved.
#' An optional `expr` argument allows additional code to be evaluated on the final decorated output.
#'
#' @inheritParams module_validate_error
#' @inheritParams teal_modules
#' @param transformators (`list` of `teal_transform_module`) decorator modules to apply sequentially
#'   to `data`. Each transformator receives the output of the previous one as input.
#' @return `reactive` `teal_data`
#' @examples
#' library(shiny)
#' library(teal.data)
#'
#' # A decorator that sets a fixed title on a ggplot2 object named `plot`
#' static_decorator <- teal_transform_module(
#'   label = "Static decorator",
#'   server = function(id, data) {
#'     moduleServer(id, function(input, output, session) {
#'       reactive({
#'         req(data())
#'         within(data(), {
#'           plot <- plot + ggplot2::ggtitle("Decorated title")
#'         })
#'       })
#'     })
#'   }
#' )
#'
#' if (interactive()) {
#'   shinyApp(
#'     ui = fluidPage(
#'       ui_transform_teal_data("decorate", transformators = list(static_decorator)),
#'       plotOutput("plot")
#'     ),
#'     server = function(input, output, session) {
#'       data <- reactive(
#'         teal_data(
#'           plot = ggplot2::ggplot(iris, ggplot2::aes(Sepal.Length, Sepal.Width)) +
#'             ggplot2::geom_point()
#'         )
#'       )
#'       decorated <- srv_transform_teal_data(
#'         "decorate",
#'         data = data,
#'         transformators = list(static_decorator)
#'       )
#'       output$plot <- renderPlot(decorated()[["plot"]])
#'     }
#'   )
#' }
#' @name module_transform_data
NULL

#' @rdname module_transform_data
#' @param expr (`expression` or `reactive`) optional expression evaluated on top of the decorated
#'   output. Useful for post-processing after all transformators have run.
#' @param modules `r lifecycle::badge("deprecated")` No longer used.
#' @param is_transform_failed `r lifecycle::badge("deprecated")` No longer used.
#' @export
srv_transform_teal_data <- function(id,
                                    data,
                                    transformators,
                                    modules = NULL,
                                    is_transform_failed = reactiveValues(),
                                    expr) {
  checkmate::assert_class(data, classes = "reactive")

  if (!missing(modules)) {
    lifecycle::deprecate_warn(
      when = "1.2.0",
      what = "srv_transform_teal_data(modules)",
      details = "Argument is used only in the internal of teal and will be removed in the next release."
    )
  }

  if (!missing(is_transform_failed)) {
    lifecycle::deprecate_warn(
      when = "1.2.0",
      what = "srv_transform_teal_data(is_transform_failed)",
      details = "Argument is used only in the internal of teal and will be removed in the next release."
    )
  }

  decorated_output <- .srv_transform_teal_data(
    id,
    data = data,
    transformators = transformators,
    modules = modules,
    is_transform_failed = is_transform_failed
  )

  no_expr <- missing(expr)

  reactive({
    data_out <- try(data(), silent = TRUE)
    if (inherits(data_out, "qenv.error")) {
      data()
    } else {
      # ensure original errors are displayed and `eval_code` is never executed with NULL
      req(data(), decorated_output())
      if (no_expr) {
        decorated_output()
      } else {
        expr_r <- if (is.reactive(expr)) expr else reactive(expr)
        teal.code::eval_code(decorated_output(), expr_r())
      }
    }
  })
}


#' @rdname module_transform_data
#' @param class `r lifecycle::badge("deprecated")` No longer used.
#' @param ... additional arguments passed to `.ui_transform_teal_data` (e.g. `class`).
#' @return A `list` of `bslib::accordion` UI elements, one per transformator, or `NULL` if
#'   `transformators` is empty.
#' @export
ui_transform_teal_data <- function(id, transformators, class = "well", ...) {
  if (!missing(class)) {
    lifecycle::deprecate_warn(
      when = "1.2.0",
      what = "ui_transform_teal_data(class)",
      details = "Ability to choose class will be removed in the next release."
    )
  }
  .ui_transform_teal_data(
    id,
    transformators = transformators,
    class = class,
    ...
  )
}

#' Internal module handling list of [teal_transform_module()]
#'
#' This module calls [teal_transform_module()] sequentially by passing output from one step to the next.
#' Modules have error/warning handling feature and highlight containers when something goes wrong.
#' @name module_transform_teal_data_impl
NULL

#' @rdname module_transform_teal_data_impl
#' @inheritParams srv_transform_teal_data
#' @return `shiny.tag`
#' @keywords internal
.ui_transform_teal_data <- function(id, transformators, class = "well") {
  checkmate::assert_string(id)
  if (length(transformators) == 0L) {
    return(NULL)
  }
  if (inherits(transformators, "teal_transform_module")) {
    transformators <- list(transformators)
  }
  checkmate::assert_list(transformators, "teal_transform_module")
  names(transformators) <- sprintf("transform_%d", seq_along(transformators))

  lapply(
    names(transformators),
    function(name) {
      child_id <- NS(id, name)
      ns <- NS(child_id)
      data_mod <- transformators[[name]]
      transform_wrapper_id <- ns(sprintf("wrapper_%s", name))

      display_fun <- if (is.null(data_mod$ui)) shinyjs::hidden else function(x) x

      display_fun(
        bslib::accordion(
          bslib::accordion_panel(
            attr(data_mod, "label"),
            icon = bsicons::bs_icon("palette-fill"),
            tags$div(
              id = transform_wrapper_id,
              if (is.null(data_mod$ui)) {
                return(NULL)
              } else {
                data_mod$ui(id = ns("transform"))
              },
              div(
                id = ns("validate_messages"),
                class = "teal_validated",
                uiOutput(ns("error_wrapper"))
              )
            )
          )
        )
      )
    }
  )
}

#' @rdname module_transform_teal_data_impl
#' @inheritParams ui_transform_teal_data
#' @return `reactive` `teal_data` object
#' @keywords internal
.srv_transform_teal_data <- function(id, data, transformators, modules = NULL, is_transform_failed = reactiveValues()) {
  checkmate::assert_string(id)
  assert_reactive(data)
  checkmate::assert_class(modules, "teal_module", null.ok = TRUE)
  if (length(transformators) == 0L) {
    return(data)
  }
  if (inherits(transformators, "teal_transform_module")) {
    transformators <- list(transformators)
  }
  checkmate::assert_list(transformators, "teal_transform_module", null.ok = TRUE)
  names(transformators) <- sprintf("transform_%d", seq_along(transformators))

  moduleServer(id, function(input, output, session) {
    module_output <- Reduce(
      function(data_previous, name) {
        moduleServer(name, function(input, output, session) {
          logger::log_debug("srv_transform_teal_data@1 initializing module for { name }.")

          data_out <- reactiveVal()
          .call_once_when(inherits(data_previous(), "teal_data"), {
            logger::log_debug("srv_teal_transform_teal_data@2 triggering a transform module call for { name }.")
            data_unhandled <- transformators[[name]]$server("transform", data = data_previous)
            data_handled <- reactive(tryCatch(data_unhandled(), error = function(e) e))

            observeEvent(data_handled(), {
              if (inherits(data_handled(), "teal_data")) {
                if (!identical(data_handled(), data_out())) {
                  data_out(data_handled())
                }
              }
            })

            is_transform_failed[[name]] <- FALSE
            observeEvent(data_handled(), {
              if (inherits(data_handled(), "teal_data")) {
                is_transform_failed[[name]] <- FALSE
              } else {
                is_transform_failed[[name]] <- TRUE
              }
            })

            is_previous_failed <- reactive({
              idx_this <- which(names(is_transform_failed) == name)
              is_transform_failed_list <- reactiveValuesToList(is_transform_failed)
              idx_failures <- which(unlist(is_transform_failed_list))
              any(idx_failures < idx_this)
            })

            srv_validate_error("silent_error", data_handled, validate_shiny_silent_error = FALSE)
            srv_check_class_teal_data("class_teal_data", data_handled)
            if (!is.null(modules)) {
              srv_check_module_datanames("datanames_warning", data_handled, modules)
            }

            # When there is no UI (`ui = NULL`) it should still show the errors
            observe({
              if (!inherits(data_handled(), "teal_data") && !is_previous_failed()) {
                shinyjs::show("wrapper")
              }
            })

            transform_wrapper_id <- sprintf("wrapper_%s", name)
            output$error_wrapper <- renderUI({
              if (is_previous_failed()) {
                shinyjs::disable(transform_wrapper_id)
                tags$div(
                  "One of previous transformators failed. Please check its inputs.",
                  class = "teal-output-warning"
                )
              } else {
                shinyjs::enable(transform_wrapper_id)
                shiny::tagList(
                  ui_validate_error(session$ns("silent_error")),
                  ui_check_class_teal_data(session$ns("class_teal_data")),
                  ui_check_module_datanames(session$ns("datanames_warning"))
                )
              }
            })
          })

          # Ignoring unwanted reactivity breaks during initialization
          reactive({
            req(data_out())
          })
        })
      },
      x = names(transformators),
      init = data
    )

    module_output
  })
}
