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
dummy_module <- function(label = "Dummy module") {
  module(
    label = label,
    server = function(input, output, session, datasets) {
      output$filter_calls <- renderText({
        paste(lapply(
          make_adsl_first(datasets$datanames()), # todo: add this again
          #make_adsl_first(intersect(datasets$datanames(), c("ADSL", "ADAE"))),
          function(dataname) paste(datasets$get_filter_call(dataname, merge = TRUE, adsl = FALSE), collapse = "\n")
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
    filters = c("ADSL", "ADAE") #"all"
  )
}
