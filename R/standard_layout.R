#' @rdname standard_layout
#' @export
standard_layout2 <- function(output,
                             encoding = NULL,
                             forms = NULL,
                             pre_output = NULL,
                             post_output = NULL) {
  checkmate::assert_multi_class(output, c("shiny.tag", "shiny.tag.list", "html"))
  checkmate::assert_multi_class(encoding, c("shiny.tag", "shiny.tag.list", "html"), null.ok = TRUE)
  checkmate::assert_multi_class(pre_output, c("shiny.tag", "shiny.tag.list", "html"), null.ok = TRUE)
  checkmate::assert_multi_class(post_output, c("shiny.tag", "shiny.tag.list", "html"), null.ok = TRUE)

  # if encoding=NULL then forms is placed below output

  tag_output <- tags$div(
    class = "teal standard-layout",
    tags$div(class = "standard-layout-pre-output", pre_output),
    tags$div(class = "standard-layout-output", output),
    tags$div(class = "standard-layout-post-output", post_output)
  )


  output_panel <- tags$div(output, class = "teal standard-layout output-panel")
  out <- if (!is.null(encoding)) {
    tags$div(
      tags$nav(
        class = "navbar navbar-expand-lg navbar-light bg-light",
        tags$div(
          class = "navbar-nav d-flex flex-row flex-nowrap teal standard-layout encoding-panel",
          style = "width: 100%;",
          encoding
        )
      ),
      output_panel
    )
  } else {
    output_panel
  }


  bslib::page_fluid(out, class = "teal standard-layout-wrapper")
}

#' @export
teal_nav_item <- function(label = NULL, ...) {
  checkmate::assert_list(list(...), c("shiny.tag", "shiny.tag.list", "html", "character"))
  checkmate::assert_multi_class(label, c("shiny.tag", "shiny.tag.list", "html", "character"), null.ok = TRUE)
  tags$div(
    class = "nav-item",
    style = "min-width: 150px;",
    label,
    tagList(list(...))
  )
}

#' Dropdown badge
#'
#' Dropdown button in a form of a badge with `bg-primary` as default style
#' Clicking badge shows a dropdown containing any `HTML` element. Folded dropdown
#' doesn't trigger display output which means that items rendered using `render*`
#' will be recomputed only when dropdown is show.
#'
#' @param id (`character(1)`) shiny module's id
#' @param label (`shiny.tag`) Label displayed on a badge.
#' @param ... (`shiny.tag`) Content of a dropdown.
#' @export
badge_dropdown <- function(id, label, content) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    htmltools::singleton(htmltools::tags$head(
      htmltools::includeCSS(system.file("css", "badge-dropdown.css", package = "teal")),
      htmltools::includeScript(system.file("js", "badge-dropdown.js", package = "teal"))
    )),
    htmltools::tags$div(
      class = "badge-dropdown-wrapper",
      htmltools::tags$span(
        id = ns("summary_badge"),
        class = "badge bg-primary rounded-pill badge-dropdown",
        tags$span(class = "badge-dropdown-label", label),
        tags$span(class = "badge-dropdown-icon", bsicons::bs_icon("caret-down-fill")),
        onclick = sprintf("toggleBadgeDropdown('%s', '%s')", ns("summary_badge"), ns("inputs_container"))
      ),
      htmltools::tags$div(
        content,
        id = ns("inputs_container"),
        style = "visibility: hidden; opacity: 0; pointer-events: none; position: absolute; background: white; border: 1px solid #ccc; border-radius: 4px; box-shadow: 0 2px 10px rgba(0,0,0,0.1); padding: 10px; z-index: 1000; min-width: 200px; transition: opacity 0.2s ease;",
      )
    )
  )
}
