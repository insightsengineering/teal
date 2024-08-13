#' UI and server modules of `teal`
#'
#' @description `r lifecycle::badge("deprecated")`
#' Please use [`module_teal`] instead.
#'
#' @inheritParams ui_teal
#' @inheritParams srv_teal
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
                                title = build_app_title(),
                                header = tags$p(),
                                footer = tags$p()) {
  lifecycle::deprecate_soft(
    when = "0.16",
    what = "ui_teal_with_splash()",
    details = "Deprecated, please use `ui_teal` instead"
  )
  ui_teal(id = id, data = data, title = title, header = header, footer = footer)
}

#' @export
#' @rdname module_teal_with_splash
srv_teal_with_splash <- function(id, data, modules, filter = teal_slices()) {
  lifecycle::deprecate_soft(
    when = "0.16",
    what = "srv_teal_with_splash()",
    details = "Deprecated, please use `srv_teal` instead"
  )
  srv_teal(id = id, data = data, modules = modules, filter = filter)
}
