#' @export
reporter_previewer_module <- function(label = "Report previewer") {

  srv <- function(id, datasets, reporter, ...){
    teal.reporter::reporter_previewer_srv(id, reporter, ...)
  }

  ui <- function(id, datasets, ...){
    teal.reporter::reporter_previewer_ui(id, ...)
  }

  module(
    label = label,
    server = srv, ui = ui,
    server_args = list(), ui_args = list(), filters = NULL
  )
}
