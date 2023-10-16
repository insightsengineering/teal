tm_landing_popup <- function(label = "Landing Popup Module",
                             title = NULL,
                             content = NULL,
                             buttons = modalButton("Accept")) {
  checkmate::assert_string(label)
  checkmate::assert_string(title, null.ok = TRUE)
  checkmate::assert_multi_class(
    content,
    classes = c("character", "shiny.tag", "shiny.tag.list", "html"), null.ok = TRUE
  )
  checkmate::assert_multi_class(buttons, classes = c("shiny.tag", "shiny.tag.list"), null.ok = TRUE)

  logger::log_info("Initializing tm_landing_popup")

  ans <- module(
    label = label,
    server = srv_landing_popup,
    ui = ui_landing_popup,
    ui_args = NULL,
    server_args = list(title = title, content = content, buttons = buttons),
    datanames = NULL
  )
  class(ans) <- c("landing_module", class(ans))
  ans
}

srv_landing_popup <- function(id, title, content, buttons) {
  moduleServer(id, function(input, output, session) {
    showModal(
      modalDialog(
        id = "landing_popup",
        title = title,
        content,
        footer = buttons
      )
    )
  })
}

ui_landing_popup <- function(id, ...) {
  NULL
}
