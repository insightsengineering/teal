# Module to add a variable for filtering


#' UI to select among the column names of a dataset to add as a filter variable
#'
#' Once something is selected, it sets the `filter_state` (from inexisting) to `NULL`
#' for that variable in the `datasets` object. This essentially makes it available
#' for filtering.
#' Indeed, the top right panel (see `\link{module_filter_items}`) picks this up,
#' so you can adjust the filtering for that variable. The selection is then undone and
#' the choices are updated, so other variables can be added for filtering.
#' Variables that cannot be filtered are not available in the selection.
#'
#' @md
#' @param id module id
#' @param dataname name of dataset whose columns should be filtered
#'
#' @importFrom shinyWidgets pickerOptions
#'
#' @examples
#' library(random.cdisc.data)
#' library(dplyr)
#'
#' ADSL <- radsl(cached = TRUE)
#' attr(ADSL, "keys") <- get_cdisc_keys("ADSL")
#' ADAE <- radlb(cached = TRUE)
#' attr(ADAE, "keys") <- get_cdisc_keys("ADAE")
#'
#' datasets <- teal:::FilteredData$new()
#' # these filters will no longer be available for selection
#' isolate({
#'   datasets$set_data("ADSL", ADSL)
#'   datasets$set_filter_state("ADSL", varname = NULL, list(
#'     AGE = list(range = c(33, 44), keep_na = FALSE),
#'     SEX = list(choices = "M", keep_na = TRUE)
#'   ))
#'   datasets$set_data("ADAE", ADAE)
#'   datasets$set_filter_state("ADAE", varname = NULL, list(
#'     CHG = list(range = c(20, 35), keep_na = FALSE)
#'   ))
#' })
#'
#' shinyApp(ui = function() {
#'   fluidPage(
#'     include_teal_css_js(),
#'     ui_add_filter_variable("filter_ADSL", "ADSL"),
#'     ui_add_filter_variable("filter_ADAE", "ADAE"),
#'     p("The following variables are filtered:"),
#'     verbatimTextOutput("info")
#'   )
#' }, server = function(input, output, session) {
#'   callModule(srv_add_filter_variable, "filter_ADSL", datasets, "ADSL")
#'   callModule(
#'     srv_add_filter_variable, "filter_ADAE", datasets, "ADAE",
#'     omit_vars = reactive(colnames(datasets$get_data("ADSL", filtered = FALSE)))
#'   )
#'   output$info <- renderText({
#'     paste0(
#'       datasets$datanames(), ": ",
#'       lapply(datasets$datanames(), function(dataname) toString(names(datasets$get_filter_state(dataname)))),
#'       collapse = "\n"
#'     )
#'   })
#' }) %>% invisible() # invisible so it does not run
ui_add_filter_variable <- function(id, dataname) {
  stopifnot(
    is_character_single(dataname)
  )

  ns <- NS(id)

  div(
    optionalSelectInput(
      ns("new_filter_var"),
      label = dataname,
      choices = NULL,
      options = pickerOptions(
        liveSearch = TRUE,
        noneSelectedText = "Select a variable"
      )
    ),
  )
}

#' Server function to add a filter variable to `datasets`
#'
#' For the return value, note that the currently selected input will be
#' reset in the UI for the user to select another variable.
#' So, it will be `NULL` again in the next reactive flush.
#'
#' @md
#' @inheritParams srv_shiny_module_arguments
#' @inheritParams ui_add_filter_variable
#' @param omit_vars `function / reactive returning a character vector` variables that are
#'   not available for filtering
#' @return `reactive` which returns the currently selected filter variable
srv_add_filter_variable <- function(input, output, session, datasets, dataname, omit_vars = function() c()) {
  stopifnot(
    is(datasets, "FilteredData"),
    is_character_single(dataname),
    is.function(omit_vars)
  )

  # currently active filter vars for this dataset
  active_filter_vars <- reactive({
    names(datasets$get_filter_state(dataname))
  })

  # observe input$new_filter_var: update the filter state of the datasets
  # this will update active_filter_vars, which then triggers an update of the choices
  observeEvent(input$new_filter_var, {
    # if NULL, it was just reset (to select a new variable to filter); at startup, this is also called with NULL
    var_to_add <- input$new_filter_var
    if (!is.null(var_to_add)) {
      stopifnot(datasets$is_filterable(dataname, var_to_add))
      .log("add filter variable", var_to_add)
      datasets$restore_filter(dataname, varname = var_to_add)
      datasets$.__enclos_env__$private$validate() # todo1: remove or keep in checking mode?
    }
  })

  # remove selected option from choices and set again to unselected, so a new
  # variable can be selected
  # reacts both when data changed and when active_filter_vars is updated
  observe({
    # we add this dependency here so that the choices update once the UI is set up (which triggers an event)
    # even though the new_filter_var is set to NULL with `updateOptionalSelectInput`, this only happens once all
    # observers were executed, so the above that adds it to the filtered variables still has its non-NULL value
    input$new_filter_var

    .log("updating choices to add filter variables for", dataname)
    choices <- setdiff(
      names(datasets$get_data(dataname, filtered = FALSE)),
      c(active_filter_vars(), omit_vars())
    )
    choices <- choices[
      vapply(choices, function(varname) datasets$is_filterable(dataname, varname = varname), logical(1))
    ]
    updateOptionalSelectInput(
      session,
      "new_filter_var",
      choices = choices,
      selected = NULL # unselect option
    )
  })

  return(reactive(input$new_filter_var))
}
