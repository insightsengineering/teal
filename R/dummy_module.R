# todo: put into teal.devel because it relies on data_extract_input
#' #' Display data extract inputs in the left encoding panel
#' #'
#' #' Also has the ShowRCode button to see the code generated
#' #' from the right encoding panel
#' #'
#' #' @param ... data_extract inputs to display in the encoding panel
#' #' @param module `character` module label
#' #'
#' #' Please do not remove, this is useful for debugging teal without
#' #' dependencies and simplifies devtools::load_all which otherwise fails
#' #' and avoids session restarts!
#' #'
#' #' Not for end users, so do not export
#' #'
#' #' #todo: showR Code
#' dummy_module_with_dataextracts <- function(..., label = "Dummy module") {
#'   data_extracts <- list(...)
#'   # check names are unique and fully named
#'   stopifnot(length(unique(names(data_extracts))) == length(data_extracts))
#'   stopifnot(is_character_single(label))
#'
#'   browser()
#'
#'   module(
#'     label = label,
#'     server = function(input, output, session, datasets) {
#'       data_extracts <- data_extracts[!vapply(data_extracts, is.null, logical(1))]
#'       merged_data <- reactive({
#'         data_merge_module(
#'           datasets = datasets,
#'           data_extract = data_extracts,
#'           input_id = names(data_extracts)
#'         )
#'       })
#'
#'       #init_chunks()
#'
#'       output$table <- renderTable({
#'         merged_data()()$data()
#'       })
#'     },
#'     ui = function(id, ...) {
#'       ns <- NS(id)
#'       standard_layout(
#'         output = white_small_well(
#'           tableOutput(ns("table"))
#'         ),
#'         encoding = do.call(div, c(
#'           list(tags$label("Encodings", class = "text-primary")),
#'           unname(Map(
#'             function(name, spec) data_extract_input(
#'               id = ns(name),
#'               label = name,
#'               data_extract_spec = spec
#'             ),
#'             names(data_extracts), data_extracts
#'           ))
#'         ))
#'       )
#'     },
#'     filters = "all"
#'   )
#' }

#' Dummy module to test right encoding panel
#'
#' ShowRCode not available here because only defined in teal.devel.
#'
#' Please do not remove, this is useful for debugging teal without
#' dependencies and simplifies devtools::load_all which otherwise fails
#' and avoids session restarts!
#'
#' @param label `character` label of module
#'
#'x Not for end users, so do not export.
#'
#' @examples
#' library(random.cdisc.data)
#' ADSL <- radsl(cached = TRUE)
#' # by testing with NA values, we will see whether the filtering really works when
#' # we add and remove filters
#' ADSL$SEX[1:150] = NA
#'
#' devtools::load_all(); app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset(dataname = "ADSL",
#'                   data = ADSL),
#'     code = "ADSL <- radsl(cached = TRUE)"),
#'   modules = root_modules(
#'     dummy_module()
#'   ),
#'   header = "Simple teal app"
#' ); shinyApp(app$ui, app$server)
dummy_module <- function(label = "Dummy module") {
  module(
    label = label,
    server = function(input, output, session, datasets) {
      # todo: showRCode
    },
    ui = function(id, ...) {
      h2("This is a dummy module.")
    },
    filters = "all"
  )
}
