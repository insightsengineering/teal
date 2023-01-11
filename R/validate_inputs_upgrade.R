
#' @param ... either any number of `InputValidator` objects
#'            or an optionally named `list` of `InputValidator` objects, see `Details`
#' @param header `character(1)` optional generic validation message
validate_inputs_upgrade <- function(..., header = "Some inputs require attention") {
  dots <- list(...)
  checkmate::assert(
    checkmate::check_list(dots, types = "InputValidator"),
    checkmate::check_list(unlist(dots), types = "InputValidator")
  )
  checkmate::assert_string(header, null.ok = TRUE)

  # determine input type: is it validator objects or a list of validator objects?
  if (all(vapply(dots, inherits, logical(1L), what = "InputValidator"))) {
    input <- "validators"
  } else if (all(vapply(unlist(dots), inherits, logical(1L), what = "InputValidator"))) {
    input <- "list"
  } else {
    stop("validate_inputs cannot handle this type of input")
  }
  # obtain a list of validators
  vals <- switch(
    input,
    "validators" = dots,
    "list" = unlist(dots)
  )
  # drop disabled validators
  if (!all(vapply(vals, validator_enabled, logical(1L)))) {
    logger::log_warn("Some validators are disabled and will be omitted.")
    vals <- Filter(validator_enabled, vals)
  }
  # extract input validation messages
  failings <- switch(
    input,
    "validators" = vis(vals, header),
    "list" = vil(vals)
  )
  # trigger output validation
  shiny::validate(shiny::need(is.null(failings), failings))
}

# internals
#' @keywords internal
# collates messages from multiple validators under common header
vis <- function(ivl, header) {
  add_header(unlist(lapply(ivl, gather_messages)), header)
}
#' @keywords internal
# prints messages from multiple validators under separate headers
vil <- function(ivl) {
  fail_messages <- vector("list", length(ivl))
  for (v in seq_along(ivl)) {
    fail_messages[[v]] <- gather_and_add(ivl[[v]], names(ivl)[v])
  }
  unlist(fail_messages)
}
