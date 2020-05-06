# This file contains modules useful for debugging and developing teal.

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
#'     teal:::filter_calls_module()
#'   ),
#'   header = "Simple teal app"
#' )
#' shinyApp(app$ui, app$server)
#' }
filter_calls_module <- function(label = "Dummy module", active_datanames = "all") {
  stopifnot(is_character_single(label))
  stopifnot(identical(active_datanames, "all") || is_character_vector(active_datanames))

  module(
    label = label,
    server = function(input, output, session, datasets) {
      output$filter_calls <- renderText({
        active_datanames <- handle_active_datanames(datasets, active_datanames)
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

#' Bookmarking module
#'
#' Presents a button that bookmarks the current state.
#' Note that Shiny does not bookmark uploaded files except when
#' `enableBookmarking` is set to `server`.
#' The module also prints a nice error message if bookmarking is not enabled.
#'
#' @md
#' @inheritParams filter_calls_module
bookmark_module <- function(label = "Bookmark module") {
  stopifnot(is_character_single(label))

  module(
    label = label,
    server = function(input, output, session, datasets) {
      # needs to be stored on server because datasets are too big to be URL-encoded
      # done in server for now
      #stopifnot(identical(getShinyOption("bookmarkStore"), "server"))

      observeEvent(input$bookmark, {
        if (!isTRUE(getShinyOption("bookmarkStore") %in% c("url", "server"))) { # isTRUE because may be NULL
          showModal(modalDialog(
            title = "Bookmarking not enabled",
            paste0(
              "Shiny bookmarking option must be enabled, its value currently is: '",
              getShinyOption("bookmarkStore"), "'."
            )
          ))
        } else {
          session$doBookmark()
        }
      })
    },
    ui = function(id, ...) {
      ns <- NS(id)
      div(
        h2("Bookmark"),
        bookmarkButton(id = ns("bookmark"))
      )
    },
    # we show all filters so the user sees in which state the datasets will be bookmarked
    filters = "all"
  )
}

#' Module that calls `browser()` on button click
#'
#' The module presents a button that will call `browser()`.
#' This is useful as breakpoints or global variables can be set in this way.
#' When you are developing another module and find a function not working
#' as expected, without restarting the app, you can call `debug(your_fcn)` and
#' then resume execution. On the next invocation of the function, it will
#' debug it.
#'
#' @md
#' @inheritParams filter_calls_module
debug_browser_module <- function(label = "Debug with browser()") {
  stopifnot(is_character_single(label))

  module(
    label = label,
    server = function(input, output, session, datasets) {
      observeEvent(input$call_browser, {
        browser()
      })
    },
    ui = function(id, ...) {
      ns <- NS(id)
      div(
        h2("Debugging"),
        p("Once in the console, you can type `debug(your_fcn)` and resume execution. This will then debug the function the next time it is called. For example, `debugonce(session$doBookmark)`. You can also access the datasets."), #nolintr
        actionButton(ns("call_browser"), "Call browser()")
      )
    },
    filters = "all"
  )
}

#' Reset filters for the specified datasets
#'
#' The module presents a group of checkboxes to select the datasets for which to reset
#' all filters.
#' todo: ideally, this button should also be on the right filtering panel with
#' similar trash icon as for single variable right now
reset_filters_module <- function(label = "Reset filters", active_datanames = "all") {
  stopifnot(is_character_single(label))
  stopifnot(identical(active_datanames, "all") || is_character_vector(active_datanames))

  module(
    label = label,
    server = function(input, output, session, datasets) {
      # reactive, processed version of datanames
      active_datanames_r <- reactive(handle_active_datanames(datasets, active_datanames))
      observeEvent(active_datanames_r(), {
        updateCheckboxGroupInput(
          session, "datasets_to_reset",
          choices = active_datanames_r(), selected = active_datanames_r()
        )
      })

      observeEvent(input$reset, {
        lapply(
          input$datasets_to_reset,
          function(dataname) datasets$set_filter_state(dataname, varname = NULL, state = list())
        )
      })
    },
    ui = function(id, ...) {
      ns <- NS(id)
      div(
        h2("Reset filters"),
        checkboxGroupInput(ns("datasets_to_reset"), "Datasets to reset:"),
        actionButton(ns("reset"), "Reset")
      )
    },
    filters = active_datanames
  )
}

# when active_datanames is "all", sets them to all datanames
# otherwise, it makes sure that it is a subset of the available datanames
handle_active_datanames <- function(datasets, active_datanames) {
  if (identical(active_datanames, "all")) {
    active_datanames <- datasets$datanames()
  }
  return(intersect(datasets$datanames(), active_datanames))
}
