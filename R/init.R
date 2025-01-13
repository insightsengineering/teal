# This is the main function from teal to be used by the end-users. Although it delegates
# directly to `module_teal_with_splash.R`, we keep it in a separate file because its documentation is quite large
# and it is very end-user oriented. It may also perform more argument checking with more informative
# error messages.

#' Create the server and UI function for the `shiny` app
#'
#' @description `r lifecycle::badge("stable")`
#'
#' End-users: This is the most important function for you to start a
#' `teal` app that is composed of `teal` modules.
#'
#' @param data (`teal_data` or `teal_data_module`)
#'   For constructing the data object, refer to [teal.data::teal_data()] and [teal_data_module()].
#' @param modules (`list` or `teal_modules` or `teal_module`)
#'   Nested list of `teal_modules` or `teal_module` objects or a single
#'   `teal_modules` or `teal_module` object. These are the specific output modules which
#'   will be displayed in the `teal` application. See [modules()] and [module()] for
#'   more details.
#' @param filter (`teal_slices`) Optionally,
#'   specifies the initial filter using [teal_slices()].
#' @param title (`shiny.tag` or `character(1)`) `r lifecycle::badge("deprecated")` Optionally,
#'   the browser window title. Defaults to a title "teal app" with the icon of NEST.
#'   Can be created using the `build_app_title()` or
#'   by passing a valid `shiny.tag` which is a head tag with title and link tag.
#'   This parameter is deprecated. Use `modify_title()` on the teal app object instead.
#' @param header (`shiny.tag` or `character(1)`) `r lifecycle::badge("deprecated")` Optionally,
#'   the header of the app.
#'   This parameter is deprecated. Use `modify_header()` on the teal app object instead.
#' @param footer (`shiny.tag` or `character(1)`) `r lifecycle::badge("deprecated")` Optionally,
#'   the footer of the app.
#'   This parameter is deprecated. Use `modify_footer()` on the teal app object instead.
#' @param id (`character`) Optionally,
#'   a string specifying the `shiny` module id in cases it is used as a `shiny` module
#'   rather than a standalone `shiny` app. This is a legacy feature.
#'
#' @return Named list containing server and UI functions.
#'
#' @export
#'
#' @include modules.R
#'
#' @examples
#' app <- init(
#'   data = within(
#'     teal_data(),
#'     {
#'       new_iris <- transform(iris, id = seq_len(nrow(iris)))
#'       new_mtcars <- transform(mtcars, id = seq_len(nrow(mtcars)))
#'     }
#'   ),
#'   modules = modules(
#'     module(
#'       label = "data source",
#'       server = function(input, output, session, data) {},
#'       ui = function(id, ...) tags$div(p("information about data source")),
#'       datanames = "all"
#'     ),
#'     example_module(label = "example teal module"),
#'     module(
#'       "Iris Sepal.Length histogram",
#'       server = function(input, output, session, data) {
#'         output$hist <- renderPlot(
#'           hist(data()[["new_iris"]]$Sepal.Length)
#'         )
#'       },
#'       ui = function(id, ...) {
#'         ns <- NS(id)
#'         plotOutput(ns("hist"))
#'       },
#'       datanames = "new_iris"
#'     )
#'   ),
#'   filter = teal_slices(
#'     teal_slice(dataname = "new_iris", varname = "Species"),
#'     teal_slice(dataname = "new_iris", varname = "Sepal.Length"),
#'     teal_slice(dataname = "new_mtcars", varname = "cyl"),
#'     exclude_varnames = list(new_iris = c("Sepal.Width", "Petal.Width")),
#'     module_specific = TRUE,
#'     mapping = list(
#'       `example teal module` = "new_iris Species",
#'       `Iris Sepal.Length histogram` = "new_iris Species",
#'       global_filters = "new_mtcars cyl"
#'     )
#'   ),
#'   title = "App title",
#'   header = tags$h1("Sample App"),
#'   footer = tags$p("Sample footer")
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
init <- function(data,
                 modules,
                 filter = teal_slices(),
                 title = lifecycle::deprecated(),
                 header = lifecycle::deprecated(),
                 footer = lifecycle::deprecated(),
                 id = character(0)) {
  logger::log_debug("init initializing teal app with: data ('{ class(data) }').")

  # argument checking (independent)
  ## `data`
  checkmate::assert_multi_class(data, c("teal_data", "teal_data_module"))

  ## `modules`
  checkmate::assert(
    .var.name = "modules",
    checkmate::check_multi_class(modules, c("teal_modules", "teal_module")),
    checkmate::check_list(modules, min.len = 1, any.missing = FALSE, types = c("teal_module", "teal_modules"))
  )
  if (inherits(modules, "teal_module")) {
    modules <- list(modules)
  }
  if (checkmate::test_list(modules, min.len = 1, any.missing = FALSE, types = c("teal_module", "teal_modules"))) {
    modules <- do.call(teal::modules, modules)
  }

  ## `filter`
  checkmate::assert_class(filter, "teal_slices")
  checkmate::assert_character(id, max.len = 1, any.missing = FALSE)

  # log
  teal.logger::log_system_info()

  ## `filter` - set app_id attribute unless present (when restoring bookmark)
  if (is.null(attr(filter, "app_id", exact = TRUE))) attr(filter, "app_id") <- create_app_id(data, modules)

  ## `filter` - convert teal.slice::teal_slices to teal::teal_slices
  filter <- as.teal_slices(as.list(filter))

  # argument checking (interdependent)
  ## `filter` - `modules`
  if (isTRUE(attr(filter, "module_specific"))) {
    module_names <- unlist(c(module_labels(modules), "global_filters"))
    failed_mod_names <- setdiff(names(attr(filter, "mapping")), module_names)
    if (length(failed_mod_names)) {
      stop(
        sprintf(
          "Some module names in the mapping arguments don't match module labels.\n %s not in %s",
          toString(failed_mod_names),
          toString(unique(module_names))
        )
      )
    }

    if (anyDuplicated(module_names)) {
      # In teal we are able to set nested modules with duplicated label.
      # Because mapping argument bases on the relationship between module-label and filter-id,
      # it is possible that module-label in mapping might refer to multiple teal_module (identified by the same label)
      stop(
        sprintf(
          "Module labels should be unique when teal_slices(mapping = TRUE). Duplicated labels:\n%s ",
          toString(module_names[duplicated(module_names)])
        )
      )
    }
  }

  ## `data` - `modules`
  if (inherits(data, "teal_data")) {
    if (length(data) == 0) {
      stop("The environment of `data` is empty.")
    }

    is_modules_ok <- check_modules_datanames(modules, names(data))
    if (!isTRUE(is_modules_ok) && length(unlist(extract_transformators(modules))) == 0) {
      warning(is_modules_ok, call. = FALSE)
    }

    is_filter_ok <- check_filter_datanames(filter, names(data))
    if (!isTRUE(is_filter_ok)) {
      warning(is_filter_ok)
      # we allow app to continue if applied filters are outside
      # of possible data range
    }
  }

  reporter <- teal.reporter::Reporter$new()$set_id(attr(filter, "app_id"))
  if (is_arg_used(modules, "reporter") && length(extract_module(modules, "teal_module_previewer")) == 0) {
    modules <- append_module(
      modules,
      reporter_previewer_module(server_args = list(previewer_buttons = c("download", "reset")))
    )
  }

  if (lifecycle::is_present(title)) {
    checkmate::assert_multi_class(title, c("shiny.tag", "shiny.tag.list", "html", "character"))
    lifecycle::deprecate_warn(
      when = "0.15.3",
      what = "init(title)",
      details = "Use `modify_title()` on the teal app object instead."
    )
  } else {
    title <- tags$head(
      tags$title("teal app"),
      tags$link(
        rel = "icon",
        href = "https://raw.githubusercontent.com/insightsengineering/hex-stickers/main/PNG/nest.png",
        sizes = "any"
      )
    )
  }
  if (lifecycle::is_present(header)) {
    checkmate::assert_multi_class(header, c("shiny.tag", "shiny.tag.list", "html", "character"))
    lifecycle::deprecate_warn(
      when = "0.15.3",
      what = "init(header)",
      details = paste(
        "Use `modify_header()` on the teal app object instead."
      )
    )
  } else {
    header <- tags$p()
  }
  if (lifecycle::is_present(footer)) {
    checkmate::assert_multi_class(footer, c("shiny.tag", "shiny.tag.list", "html", "character"))
    lifecycle::deprecate_warn(
      when = "0.15.3",
      what = "init(footer)",
      details = paste(
        "Use `modify_footer()` on the teal app object instead."
      )
    )
  } else {
    footer <- tags$p()
  }

  # argument transformations
  ## `modules` - landing module
  landing <- extract_module(modules, "teal_module_landing")
  modules <- drop_module(modules, "teal_module_landing")

  # Note: UI must be a function to support bookmarking.
  res <- list(
    ui = function(request) {
      ui_teal(
        id = "teal",
        modules = modules,
        title = title,
        header = header,
        footer = footer
      )
    },
    server = function(input, output, session) {
      srv_teal(id = "teal", data = data, modules = modules, filter = deep_copy_filter(filter))
    }
  )

  if (length(landing) == 1L) {
    res <- add_custom_server(res, function(input, output, session) {
      do.call(landing[[1L]]$server, c(list(id = "landing_module_shiny_id")))
    })
    lifecycle::deprecate_warn(
      when = "0.15.3",
      what = "landing_popup_module()",
      details = paste(
        "landing_popup_module() is deprecated.",
        "Use add_landing_popup() on the teal app object instead."
      )
    )
  } else if (length(landing) > 1L) {
    stop("Only one `landing_popup_module` can be used.")
  }

  logger::log_debug("init teal app has been initialized.")

  res
}

#' Add a custom Title to `teal` application
#'
#' @param title (`shiny.tag` or `character(1)`) The title to be used
#' @return The modified app object
#' @export
#' @examples
#' app <- init(
#'   data = teal_data(IRIS = iris, MTCARS = mtcars),
#'   modules = modules(example_module())
#' ) |>
#'   modify_title("Custom title")
#'
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
modify_title <- function(
    app,
    title = "teal app",
    favicon = "https://raw.githubusercontent.com/insightsengineering/hex-stickers/main/PNG/nest.png") {
  res <- app
  res$ui <- function(request) {
    title_tag <- tags$div(
      id = "teal-title",
      tags$head(
        tags$title(title),
        tags$link(
          rel = "icon",
          href = favicon,
          sizes = "any"
        )
      )
    )
    ui_tq <- htmltools::tagQuery(app$ui(request = request))
    ui_tq$find("#teal-title")$replaceWith(title_tag)$allTags()
  }
  res
}

#' Add a Header to `teal` Application
#'
#' @description Adds a header to the `teal` app.
#'
#' @param app (`environment`) The `teal` app environment.
#' @param header (`shiny.tag` or `character(1)`) The header content to set. Defaults to an empty paragraph tag.
#' @export
#' @examples
#' app <- init(
#'   data = teal_data(IRIS = iris),
#'   modules = modules(example_module())
#' ) |>
#'   modify_header(
#'     tags$div(
#'       h3("Custom header")
#'     )
#'   )
#'
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
modify_header <- function(app, header = tags$p()) {
  res <- app
  res$ui <- function(request) {
    ui_tq <- htmltools::tagQuery(app$ui(request = request))
    ui_tq$find("#teal-header")$replaceWith(tags$header(id = "teal-header", header))$allTags()
  }
  res
}

#' Add a Footer to `teal` Application
#'
#' @description Adds a footer to the `teal` app.
#'
#' @param app (`environment`) The `teal` app environment.
#' @param footer (`shiny.tag` or `character(1)`) The footer content to set. Defaults to an empty paragraph tag.
#' @export
#' @examples
#' app <- init(
#'   data = teal_data(IRIS = iris),
#'   modules = modules(example_module())
#' ) |>
#'   modify_footer("Custom footer")
#'
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
modify_footer <- function(app, footer = tags$p()) {
  res <- app
  res$ui <- function(request) {
    ui_tq <- htmltools::tagQuery(app$ui(request = request))
    ui_tq$find("#teal-footer")$replaceWith(tags$div(id = "teal-footer", footer))$allTags()
  }
  res
}

#' Add a Landing Popup to `teal` Application
#'
#' @description Adds a landing popup to the `teal` app. This popup will be shown when the app starts.
#'
#' This modifier is used to display a popup dialog when the application starts.
#' The dialog blocks access to the application and must be closed with a button before the application can be viewed.
#'
#' @param app (`environment`) The `teal` app environment.
#' @param id (`character(1)`) The ID of the modal dialog.
#' @param label (`character(1)`) Label of the module.
#' @param title (`character(1)`) Text to be displayed as popup title.
#' @param content (`character(1)`, `shiny.tag` or `shiny.tag.list`) with the content of the popup.
#'  Passed to `...` of `shiny::modalDialog`. See examples.
#' @param buttons (`shiny.tag` or `shiny.tag.list`) Typically a `modalButton` or `actionButton`. See examples.
#' @export
#' @examples
#' app <- init(
#'   data = teal_data(IRIS = iris, MTCARS = mtcars),
#'   modules = modules(example_module())
#' ) |>
#'   add_landing_popup(
#'     title = "Welcome",
#'     content = "This is a landing popup.",
#'     buttons = modalButton("Accept")
#'   )
#'
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
add_landing_popup <- function(
    app,
    id = "landingpopup",
    title = NULL,
    content = NULL,
    buttons = modalButton("Accept")) {
  custom_server <- function(input, output, session) {
    showModal(
      modalDialog(
        id = id,
        title = title,
        content,
        footer = buttons
      )
    )
  }
  app <- add_custom_server(app, custom_server)
  app
}

#' Add a Custom Server Logic to `teal` Application
#'
#' @description Adds a custom server function to the `teal` app. This function can define additional server logic.
#'
#' @param app (`environment`) The `teal` app environment.
#' @param custom_server (`function`) The custom server function to set.
#' @export
#' @examples
#' app <- init(
#'   data = teal_data(IRIS = iris),
#'   modules = modules(example_module())
#' ) |>
#'   add_custom_server(function(input, output, session) {
#'     print("injected server logic")
#'   })
#'
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
add_custom_server <- function(app, custom_server) {
  old_server <- app$server

  app$server <- function(input, output, session) {
    old_server(input, output, session)
    custom_server(input, output, session)
  }
  app
}
