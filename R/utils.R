#' Get Client Timezone
#'
#' Local timezone in the browser may differ from the system timezone from the server.
#'   This script can be run to register a shiny input which contains information about
#'   the timezone in the browser.
#'
#' @param ns (`function`) namespace function passed from the `session` object in the
#'   Shiny server. For Shiny modules this will allow for proper name spacing of the
#'   registered input.
#'
#' @return (`Shiny`) input variable accessible with `input$tz` which is a (`character`)
#'  string containing the timezone of the browser/client.
#' @keywords internal
get_client_timezone <- function(ns) {
  script <- sprintf(
    "Shiny.setInputValue(`%s`, Intl.DateTimeFormat().resolvedOptions().timeZone)",
    ns("timezone")
  )
  shinyjs::runjs(script) # function does not return anything
  return(invisible(NULL))
}



#' Resolve the expected bootstrap theme
#' @keywords internal
get_teal_bs_theme <- function() {
  bs_theme <- getOption("teal.bs_theme")
  if (is.null(bs_theme)) {
    NULL
  } else if (!inherits(bs_theme, "bs_theme")) {
    warning("teal.bs_theme has to be of a bslib::bs_theme class, the default shiny bootstrap is used.")
    NULL
  } else {
    bs_theme
  }
}



#' Add condition to rule function.
#'
#' Adds a condition to a rule function to be used in an `InputValidator`.
#'
#' When building a `shinyvalidate::InputValidator`, some rules may only need to be checked
#' under specific conditions. Since an input value must not be validated by multiple
#' validator objects (as per `shinyvalidate` documentation),
#' some validation scenarios cannot be covered by adding `condition`s to a whole validator.
#'
#' This function takes a function or formula that can be used as a validation rule
#' and incorporates `condition` into its body, upstream of the actual test.
#'
#' In cases where `condition` relies in input values, it is safer to wrap `condition`
#' in an `isTRUE` call so that NA or NULL does not crash evaluation.
#' For example, `input$id == "x"` will return `logical(0)` if input$id is NULL
#' and `NA` if input$id is NA, whereas `isTRUE(input$id == "x")` will reliably return `FALSE`.
#'
#' @param rule `function` or `formula` that specifies a validation rule and a failing message
#' @param condition `call` that specifies when to check `rule`, see `Details`
#' @param ... additional arguments passed to `rule`
#'
#' @return
#' Returns a closure, ready to be placed into a `iv$add_rule` call as a rule function.
#'
#' @seealso `[shinyvalidate::InputValidator]`
#'
#' @examples
#' \dontrun{
#' library(shinyvalidate)
#'
#' set <- c("element1", "element2")
#'
#' custom_rule <- function(value) {
#'   if (! value %in% set) sprintf("id must be a set of %s", paste(set, collapse = ", "))
#' }
#'
#' iv <- InputValidator$new()
#' iv$add_rule("id", sv_required())
#' iv$add_rule("id", crule(custom_rule, !is.null(set)))
#' }
#'
#' @export
#'
crule <- function(rule, condition, ...) {
  checkmate::assert_multi_class(rule, c("function", "formula"))
  checkmate::assert_class(substitute(condition), "call")

  if (inherits(rule, "formula")) {
    rule <- rlang::as_function(rule)
  }

  body(rule) <- call(
    "{",
    substitute(if (isFALSE(condition)) return(NULL)),
    body(rule)
  )
  rule
}
