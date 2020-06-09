# This module is the main teal module that puts everything together.


# todo: document these functions
#todo: move into other file
#' Teal app UI
#'
#' This is the main teal UI that puts everything together.
#'
#' It displays the splash UI which is used to fetch the data, possibly
#' prompting for a password input to fetch the data. Once the data is ready,
#' the splash screen is replaced by the actual teal UI that is tabsetted and
#' has a filter panel with datanames that are relevant for the current tab.
#' Nested tabs are possible, but we limit it to two nesting levels for reasons
#' of clarity of the UI.
#'
#' The splash screen functionality can also be used
#' for non-delayed data which takes time to load into memory, avoiding
#' Shiny session timeouts.
#'
#' Bookmarking is supported, i.e. the datasets filter state that this class
#' is responsible for is stored in and restored from the bookmarked state.
#'
#' It is written as a Shiny module so it can be added into other apps as well.
#'
#' @md
#' @param id module id
#' @param splash_ui `shiny.tag` UI to display initially,
#'   can be a splash screen or a Shiny module UI. For the latter, the corresponding
#'   `callModule` must be called to read the values from the splash module UI,
#'   probably to provide the value to `raw_data` in the server. #todo
#' @param header `shiny.tag or character` header to display above the app
#' @param footer `shiny.tag or character` footer to display below the app
#'
#' @return `HTML` for Shiny module UI
ui_teal <- function(id, splash_ui, header = tags$p("Title here"), footer = tags$p("Title here")) {
  if (is_character_single(header)) {
    header <- tags$h1(header)
  }
  if (is_character_single(footer)) {
    footer <- tags$p(footer)
  }
  stopifnot(
    inherits(splash_ui, "shiny.tag"),
    inherits(header, "shiny.tag"),
    inherits(footer, "shiny.tag")
  )

  ns <- NS(id)
  # Once the data is loaded, we will remove this element and add the real teal UI instead
  splash_ui <- div(
    id = ns("main_ui_container"), # id so we can remove the splash screen once ready
    splash_ui
  )

  # show busy icon when shiny session is busy computing stuff
  # based on https://stackoverflow.com/questions/17325521/r-shiny-display-loading-message-while-function-is-running/22475216#22475216 #nolint
  shiny_busy_message_panel <- conditionalPanel(
    condition = "(($('html').hasClass('shiny-busy')) && (document.getElementById('shiny-notification-panel') == null))", # nolint
    div(
      icon("sync", "spin fa-spin"),
      "Computing ...",
      # CSS defined in `custom.css`
      class = "shinybusymessage"
    )
  )

  # must be a function of request for bookmarking
  return(function(request) {
    shinyUI(
      fluidPage(
        include_teal_css_js(),
        tags$header(header),
        tags$hr(style = "margin: 7px 0;"),
        shiny_busy_message_panel,
        splash_ui,
        tags$hr(),
        tags$footer(footer)
      )
    )
  })
}

#' Server function corresponding to teal
#'
#' It evaluates the `raw_data` (delayed data mechanism).
#' Once it is ready and non-NULL, the splash screen is replaced by the
#' main teal UI that depends on the data.
#' The currently active tab is tracked and the right filter panel
#' updates the displayed datasets to filter for according to the active datanames
#' of the tab.
#' The initially displayed filter states can be provided, bookmarked filter
#' states always take precedence over them.
#'
#' For more doc, see `\link{ui_teal}`.
#'
#' @md
#' @param input `Shiny input object`
#' @param output `Shiny output object`
#' @param session `Shiny session object`
#' @param modules `teal_module or teal_modules` modules to display,
#'   tab-nested for `teal_modules`
#' @param raw_data `reactive` to fetch the data, only evaluated once,
#'   `NULL` value is ignored
#' @param initial_filter_states `list`, only used if not restored from
#'   bookmarked state
#'
#' # todo: examples
srv_teal <- function(input, output, session, modules, raw_data, initial_filter_states) {
  if (!modules_depth(modules) %in% c(1, 2)) {
    # although there is no technical limitation on the depth in the current
    # implementation, we don't allow deeper nesting for clarity of the apps
    stop("teal currently only supports module nesting of depth one or two.")
  }
  stopifnot(is.reactive(raw_data))

  # Javascript code ----

  if (getOption("teal_show_js_log", default = FALSE)) {
    shinyjs::showLog() # to show Javascript console logs in the R console
  }
  run_js_files(files = "init.js") # Javascript code to make the clipboard accessible


  # figure out active tab and deduce active_datanames to hide / show filters ----

  # the call to ui_modules_with_filters creates inputs that watch the tabs prefixed by teal_modules
  # we observe them and react whenever a tab is clicked by:
  # - displaying only the relevant datasets in the right hand filter in the
  # sections: filter info, filtering vars per dataname and add filter var per dataname
  call_filter_modules <- function(datasets) {
    # recursively goes down tabs to figure out the active module
    figure_out_active_module <- function(modules, idprefix) {
      id <- label_to_id(modules$label, idprefix)
      return(switch(
        class(modules)[[1]],
        teal_modules = {
          # id is the id of the tabset, the corresponding input element states which tab is selected
          active_submodule_label <- input[[id]]
          stopifnot(!is.null(active_submodule_label))
          figure_out_active_module(modules$children[[active_submodule_label]], idprefix = id)
        },
        teal_module = {
          stopifnot(is.null(input[[id]])) # id should not exist
          modules
        },
        stop("unknown module class ", class(modules))
      ))
    }

    active_datanames <- reactive_on_changes(reactive({
      # inputs may be NULL when UI hasn't loaded yet, but this expression still triggered
      req(!is.null(input[[label_to_id(modules$label, idprefix = "teal_modules")]]))

      active_datanames <- figure_out_active_module(modules, idprefix = "teal_modules")$filter
      if (identical(active_datanames, "all")) {
        active_datanames <- datasets$datanames()
      }
      # always add ADSL because the other datasets are filtered based on ADSL
      active_datanames <- union("ADSL", active_datanames)
      return(list_adsl_first(active_datanames))
    }))$value

    callModule(srv_filter_panel, "filter_panel", datasets, active_datanames)
  }


  # Datasets to store filter states and filtered datasets per session
  # Each tab for each user is an independent session and the tabs should be independent
  datasets <- FilteredData$new()

  # Shiny bookmarking ----

  # The Shiny bookmarking functionality by default only stores inputs.
  # We need to add FilteredData to the state so we restore it as well.
  # To test bookmarking, include the `bookmark_module`, click on the bookmark
  # button and then get the link. Keep the Shiny app running and open the
  # obtained link in another browser tab.
  onBookmark(function(state) {
    # this function is isolated  by Shiny
    # We store the entire R6 class with reactive values in it, but set the data to NULL.
    # Note that we cannnot directly do this on datasets as this would trigger
    # reactivity to recompute the filtered datasets, which is not needed.
    state$values$datasets_state <- datasets$get_bookmark_state()
  })
  saved_datasets_state <- NULL # set when restored because data must already be populated
  onRestore(function(state) {
    # The saved datasets mainly contains the filter states as the data
    # was set to NULL before storing. The data should have been set again
    # by the user, so we just need to set the filters.
    saved_datasets_state <<- state$values$datasets_state
  })


  # Replace splash / welcome screen once data is loaded ----

  # ignoreNULL to not trigger at the beginning when data is NULL
  # just handle it once because data obtained through delayed loading should
  # usually not change afterwards
  # todo: remove once = TRUE and adapt insert / remove UI, also need to delete old observers
  # if restored from bookmarked state, `initial_filter_states` is ignored
  observeEvent(raw_data(), ignoreNULL = TRUE, once = TRUE, {
    .log("data loaded successfully")
    data <- raw_data()

    progress <- shiny::Progress$new(session)
    on.exit(progress$close())
    progress$set(0.1, message = "Setting data")
    set_datasets_data(datasets, data)
    progress$set(0.3, message = "Setting filters")

    if (!is.null(saved_datasets_state)) {
      # actual thing to restore
      # cannot call this directly in onRestore because the data is not set at that time
      # for example, the data may only be loaded once a password is provided
      # however, onRestore only runs in the first flush and not in the flush when the
      # password was finally provided
      .log("restoring filter state from bookmarked state - initial_filter_states is ignored")
      tryCatch({
        progress$set(0.5, message = "Restoring from bookmarked state")
        datasets$restore_state_from_bookmark(saved_datasets_state)
      },
      error = function(cnd) {
        showModal(modalDialog(
          div(
            p("Could not restore the session: "),
            tags$pre(id = "error_msg", cnd$message),
          ),
          title = "Error restoring the bookmarked state",
          footer = tagList(
            actionButton("copy_code", "Copy to Clipboard", `data-clipboard-target` = "#error_msg"),
            modalButton("Dismiss")
          ),
          size = "l", easyClose = TRUE
        ))
      }
      )
    } else {
      set_datasets_filters(datasets, initial_filter_states)
    }

    # call server functions for teal modules and filter panel
    .log("initialize modules and filter panel")
    # must make sure that this is only executed once as modules assume their observers are only
    # registered once (calling server functions twice would trigger observers twice each time)
    #call_teal_modules(modules, datasets, idprefix = "teal_modules") # todo: remove
    #call_filter_modules(datasets) # todo: remove

    ui_teal_main <- ui_modules_with_filters("main_ui", modules = modules, datasets = datasets)
    callModule(srv_modules_with_filters, "main_ui", modules = modules, datasets = datasets)

    progress$set(0.7, message = "Replacing UI with main UI")
    # main_ui_container contains splash screen first and we remove it and replace it by the real UI
    removeUI(paste0("#", session$ns("main_ui_container"), " :first-child"))
    cat("############# Id is: ", paste0("#", session$ns("main_ui_container"), " :first-child"))
    insertUI(selector = paste0("#", session$ns("main_ui_container")), where = "beforeEnd", ui = ui_teal_main)

    showNotification("Data loaded - App fully started up")
  })
}
