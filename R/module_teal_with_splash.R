#' UI and server modules of `teal`
#'
#' @description `r lifecycle::badge("deprecated")`
#' Please use [`module_teal`] instead.
#'
#' @inheritParams ui_teal
#' @inheritParams srv_teal
#' @inheritParams init
#'
#' @return
#' Returns a `reactive` expression containing a `teal_data` object when data is loaded or `NULL` when it is not.
#' @name module_teal_with_splash
#'
NULL

#' @export
#' @rdname module_teal_with_splash
ui_teal_with_splash <- function(id,
                                data,
                                modules,
                                title = build_app_title(),
                                header = tags$p(),
                                footer = tags$p()) {
  lifecycle::deprecate_soft(
    when = "0.16.0",
    what = "ui_teal_with_splash()",
    details = "Please use `?ui_teal` instead"
  )
  ns <- shiny::NS(id)
  fluidPage(
    title = tags$div(
      id = ns("teal-app-title"),
      tags$head(
        tags$title("teal app"),
        tags$link(
          rel = "icon",
          href = .teal_favicon,
          sizes = "any"
        )
      )
    ),
    tags$header(id = ns("teal-header-content")),
    ui_teal(id = id, modules = modules),
    tags$footer(
      id = "teal-footer",
      tags$div(id = "teal-footer-content"),
      ui_session_info(ns("teal-footer-session_info"))
    )
  )
}

#' @export
#' @rdname module_teal_with_splash
srv_teal_with_splash <- function(id, data, modules, filter = teal_slices()) {
  lifecycle::deprecate_soft(
    when = "0.16.0",
    what = "srv_teal_with_splash()",
    details = "Deprecated, please use `?srv_teal` instead"
  )
  srv_teal(id = id, data = data, modules = modules, filter = filter)
  srv_session_info("teal-footer-session_info")
}
