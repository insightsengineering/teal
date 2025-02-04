#' `teal` user session info module
#'
#' Module to display the user session info popup and to download a lockfile. Module is included
#' when running [init()] but skipped when using [`module_teal`]. Please be aware that session info
#' contains R session information, so multiple module's calls will share the same information.
#'
#' @rdname module_session_info
#' @name module_session_info
#'
#' @inheritParams module_teal
#'
#' @examplesShinylive
#' library(teal)
#' interactive <- function() TRUE
#' {{ next_example }}
#' @examples
#' ui <- fluidPage(
#'   ui_session_info("session_info")
#' )
#'
#' server <- function(input, output, session) {
#'   srv_session_info("session_info")
#' }
#'
#' if (interactive()) {
#'   shinyApp(ui, server)
#' }
#'
#' @return `NULL` invisibly
NULL

#' @rdname module_session_info
#' @export
ui_session_info <- function(id) {
  ns <- NS(id)
  tags$div(
    teal.widgets::verbatim_popup_ui(ns("sessionInfo"), "Session Info", type = "link"),
    br(),
    ui_teal_lockfile(ns("lockfile")),
    textOutput(ns("identifier"))
  )
}

#' @rdname module_session_info
#' @export
srv_session_info <- function(id) {
  moduleServer(id, function(input, output, session) {
    srv_teal_lockfile("lockfile")

    output$identifier <- renderText(
      paste0("Pid:", Sys.getpid(), " Token:", substr(session$token, 25, 32))
    )

    teal.widgets::verbatim_popup_srv(
      "sessionInfo",
      verbatim_content = utils::capture.output(utils::sessionInfo()),
      title = "SessionInfo"
    )
  })
}
