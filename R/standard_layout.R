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
