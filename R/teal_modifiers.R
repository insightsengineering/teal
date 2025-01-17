#' Replace UI Elements in `teal` UI objects
#'
#' @param x (`teal_app`) A `teal_app` object created using the `init` function.
#' @param selector (`character(1)`) CSS selector to find elements to replace
#' @param element Replacement UI element (shiny tag or HTML)
#' @param title (`shiny.tag` or `character(1)`) The new title to be used.
#' @param favicon (`character`) The path for the icon for the title.
#' The image/icon path can be remote or the static path accessible by `shiny`, like the `www/`
#' @name teal_modifiers
#' @rdname teal_modifiers
#'
#' @keywords internal
#'
NULL


#' @rdname teal_modifiers
#' @keywords internal
#' @param x One of:
#'   - A `teal_app` object created using the `init` function.
#'   - A `teal_module`, `teal_data_module`, or `teal_transform_module` object.
#'   - A Shiny module UI function with `id` parameter
teal_replace_ui <- function(x, selector, element) {
  if (inherits(x, c("teal_app", "teal_module", "teal_data_module", "teal_transform_module"))) {
    x$ui <- teal_replace_ui(x$ui, selector, element)
    x
  } else if (checkmate::test_function(x, args = "request")) {
    # shiny ui function from teal_app
    function(request) {
      ui_tq <- htmltools::tagQuery(x(request = request))
      ui_tq$find(selector)$empty()$append(element)$allTags()
    }
  } else if (checkmate::test_function(x, args = "id")) {
    # shiny module ui function
    function(id, ...) {
      ui_tq <- htmltools::tagQuery(x(id = id, ...))
      if (grepl("^#[a-zA-Z0-9_-]+$", selector)) {
        selector <- paste0("#", NS(id, gsub("^#", "", selector)))
      }
      ui_tq$find(selector)$empty()$append(element)$allTags()
    }
  } else {
    stop("Invalid UI object")
  }
}

#' Add a custom Title to `teal` application
#'
#' @rdname teal_modifiers
#' @export
#' @examples
#' app <- init(
#'   data = teal_data(IRIS = iris, MTCARS = mtcars),
#'   modules = modules(example_module())
#' ) |>
#'   modify_title(title = "Custom title")
#'
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
modify_title <- function(
    x,
    title = "teal app",
    favicon = "https://raw.githubusercontent.com/insightsengineering/hex-stickers/main/PNG/nest.png") {
  checkmate::assert_multi_class(x, "teal_app")
  checkmate::assert_multi_class(title, c("shiny.tag", "shiny.tag.list", "html", "character"))
  checkmate::assert_string(favicon)
  teal_replace_ui(
    x,
    "#teal-app-title",
    tags$head(
      tags$title(title),
      tags$link(
        rel = "icon",
        href = favicon,
        sizes = "any"
      )
    )
  )
}

#' Add a Header to `teal` Application
#'
#' @rdname teal_modifiers
#' @export
#' @examples
#' app <- init(
#'   data = teal_data(IRIS = iris),
#'   modules = modules(example_module())
#' ) |>
#'   modify_header(element = tags$div(h3("Custom header")))
#'
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
modify_header <- function(x, element = tags$p()) {
  checkmate::assert_multi_class(element, c("shiny.tag", "shiny.tag.list", "html", "character"))
  teal_replace_ui(x, "#teal-header-content", element)
}

#' Add a Footer to `teal` Application
#'
#' @rdname teal_modifiers
#' @export
#' @examples
#' app <- init(
#'   data = teal_data(IRIS = iris),
#'   modules = modules(example_module())
#' ) |>
#'   modify_footer(element = "Custom footer")
#'
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
modify_footer <- function(x, element = tags$p()) {
  checkmate::assert_multi_class(element, c("shiny.tag", "shiny.tag.list", "html", "character"))
  teal_replace_ui(x, "#teal-footer-content", element)
}

#' Add a Landing Popup to `teal` Application
#'
#' @description Adds a landing popup to the `teal` app. This popup will be shown when the app starts.
#' The dialog blocks access to the application and must be closed with a button before the application can be viewed.
#'
#' @param x (`teal_app`) A `teal_app` object created using the `init` function.
#' @param title (`character(1)`) Text to be displayed as popup title.
#' @param content (`character(1)`, `shiny.tag` or `shiny.tag.list`) with the content of the popup.
#'  Passed to `...` of `shiny::modalDialog`.
#' @param buttons (`shiny.tag` or `shiny.tag.list`) Typically a `modalButton` or `actionButton`.
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
    x,
    title = NULL,
    content = NULL,
    buttons = modalButton("Accept")) {
  checkmate::assert_class(x, "teal_app")
  custom_server <- function(input, output, session) {
    checkmate::assert_string(title, null.ok = TRUE)
    checkmate::assert_multi_class(
      content,
      classes = c("character", "shiny.tag", "shiny.tag.list", "html"), null.ok = TRUE
    )
    checkmate::assert_multi_class(buttons, classes = c("shiny.tag", "shiny.tag.list"))
    showModal(
      modalDialog(
        id = "landingpopup",
        title = title,
        content,
        footer = buttons
      )
    )
  }
  teal_extend_server(x, custom_server)
}

#' Add a Custom Server Logic to `teal` Application
#'
#' @description Adds a custom server function to the `teal` app. This function can define additional server logic.
#'
#' @param x (`teal_app`) A `teal_app` object created using the `init` function.
#' @param custom_server (`function(input, output, session)` or `function(input, output, session)`)
#'    The custom server function or server module to set.
#' @param module_id (`character(1)`) The ID of the module when a module server function is passed.
#' @keywords internal
teal_extend_server <- function(x, custom_server, module_id = character(0)) {
  checkmate::assert_class(x, "teal_app")
  checkmate::assert_function(custom_server)
  old_server <- x$server

  x$server <- function(input, output, session) {
    old_server(input, output, session)
    if (all(c("input", "output", "session") %in% names(formals(custom_server)))) {
      callModule(custom_server, module_id)
    } else if ("id" %in% names(formals(custom_server))) {
      custom_server(module_id)
    }
  }
  x
}
