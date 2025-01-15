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
    when = "0.15.3",
    what = "ui_teal_with_splash()",
    details = "Deprecated, please use `ui_teal` instead"
  )
  res <- ui_teal(id = id)
  res <- modify_title(res, title)
  res <- modify_header(res, header)
  modify_footer(res, footer)
}

#' @export
#' @rdname module_teal_with_splash
srv_teal_with_splash <- function(id, data, modules, filter = teal_slices()) {
  lifecycle::deprecate_soft(
    when = "0.15.3",
    what = "srv_teal_with_splash()",
    details = "Deprecated, please use `srv_teal` instead"
  )
  srv_teal(id = id, data = data, modules = modules, filter = filter)
}
