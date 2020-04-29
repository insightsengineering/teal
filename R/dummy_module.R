# This file contains mostly debugging modules

#' Dummy module to test right encoding panel
#'
#' The `Show R Code` functionality is not available here because
#' it is only defined in `teal.devel`.
#'
#' Please do not remove, this is useful for debugging teal without
#' dependencies and simplifies `devtools::load_all` which otherwise fails
#' and avoids session restarts!
#'
#' @md
#' @param label `character` label of module
#' @param active_datanames (`character` vector) active datasets that this module
#'   depends on
#'
#' Not for end users, so do not export.
#'
#' @examples
#' library(random.cdisc.data)
#' ADSL <- radsl(cached = TRUE)
#' # by testing with NA values, we will see whether the filtering really works when
#' # we add and remove filters
#' ADSL$SEX[1:150] <- NA
#' \dontrun{
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset(
#'       dataname = "ADSL",
#'       data = ADSL
#'     ),
#'     code = "ADSL <- radsl(cached = TRUE)"
#'   ),
#'   modules = root_modules(
#'     teal:::dummy_module()
#'   ),
#'   header = "Simple teal app"
#' )
#' shinyApp(app$ui, app$server)
#' }
dummy_module <- function(label = "Dummy module", active_datanames = "all") {
  stopifnot(is_character_single(label))
  stopifnot(identical(active_datanames, "all") || is_character_vector(active_datanames))

  module(
    label = label,
    server = function(input, output, session, datasets) {
      output$filter_calls <- renderText({
        if (identical(active_datanames, "all")) {
          active_datanames <- datasets$datanames()
        }
        paste(lapply(
          make_adsl_first(active_datanames),
          function(dataname) paste(datasets$get_filter_call(dataname), collapse = "\n")
        ), collapse = "\n\n")
      })
    },
    ui = function(id, ...) {
      ns <- NS(id)
      div(
        h2("This is a dummy module."),
        p("The following filter calls are generated:"),
        verbatimTextOutput(ns("filter_calls"))
      )
    },
    filters = active_datanames
  )
}


