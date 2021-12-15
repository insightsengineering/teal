# This module is the main teal module that puts everything together.

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
#' @param splash_ui `shiny.tag` UI to display initially,
#'   can be a splash screen or a Shiny module UI. For the latter, see
#'   [init()] about how to call the corresponding server function.
#' @inheritParams ui_teal_with_splash
#'
#' @return `HTML` for Shiny module UI
#'
#' @examples
#' mods <- teal:::get_dummy_modules()
#' raw_data <- reactive(teal:::get_dummy_cdisc_data())
#' app <- shinyApp(
#'   ui = function() {
#'     teal:::ui_teal("dummy")
#'   },
#'   server = function(input, output, session) {
#'     active_module <- teal:::srv_teal(id = "dummy", modules = mods, raw_data = raw_data)
#'   }
#' )
#' \dontrun{
#' runApp(app)
#' }
ui_teal <- function(id,
                    splash_ui = tags$h2("Starting the Teal App"),
                    title = NULL,
                    header = tags$p(""),
                    footer = tags$p("")) {
  if (is_character_single(header)) {
    header <- tags$h1(header)
  }
  if (is_character_single(footer)) {
    footer <- tags$p(footer)
  }
  stopifnot(
    is_html_like(splash_ui),
    is_html_like(header),
    is_html_like(footer)
  )

  ns <- NS(id)
  # Once the data is loaded, we will remove this element and add the real teal UI instead
  splash_ui <- div(
    # id so we can remove the splash screen once ready, which is the first child of this container
    id = ns("main_ui_container"),
    # we put it into a div, so it can easily be removed as a whole, also when it is a tagList (and not
    # just the first item of the tagList)
    div(splash_ui)
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

  res <- shinyUI(
    fluidPage(
      title = title,
      include_teal_css_js(),
      tags$header(header),
      tags$hr(style = "margin: 7px 0;"),
      shiny_busy_message_panel,
      splash_ui,
      tags$hr(),
      tags$footer(div(footer, textOutput(ns("identifier"))))
    )
  )
  return(res)
}

#' Server function corresponding to teal
#'
#' It evaluates the `raw_data` (delayed data mechanism) and creates the
#' `datasets` object that is shared across modules.
#' Once it is ready and non-`NULL`, the splash screen is replaced by the
#' main teal UI that depends on the data.
#' The currently active tab is tracked and the right filter panel
#' updates the displayed datasets to filter for according to the active datanames
#' of the tab.
#' The initially displayed filter states can be provided, bookmarked filter
#' states always take precedence over them.
#'
#' For more doc, see [ui_teal].
#'
#' @inheritParams init
#' @param raw_data (`reactive`)\cr
#'   returns the `TealData`, only evaluated once, `NULL` value is ignored
#'
#' @return `reactive` which returns the currently active module
srv_teal <- function(id, modules, raw_data, filter = list()) {
  stopifnot(is.reactive(raw_data))
  moduleServer(id, function(input, output, session) {
    logger::log_trace("srv_teal initializing the module.")

    output$identifier <- renderText(
      paste0("Pid:", Sys.getpid(), " Token:", substr(session$token, 25, 32))
    )

    # Javascript code ----
    if (getOption("teal_show_js_log", default = FALSE)) {
      shinyjs::showLog() # to show Javascript console logs in the R console
    }
    run_js_files(files = "init.js") # Javascript code to make the clipboard accessible
    # set timezone in shiny app
    # timezone is set in the early beginning so it will be available also
    # for DDL and all shiny modules
    get_client_timezone(session$ns)
    observeEvent(
      eventExpr = input$timezone,
      once = TRUE,
      handlerExpr = {
        session$userData$timezone <- input$timezone
        logger::log_trace("srv_teal@1 Timezone set to client's timezone: { input$timezone }.")
      }
    )

    # Shiny bookmarking ----
    # The Shiny bookmarking functionality by default only stores inputs.
    # We need to add `FilteredData` object to the state so we restore it as well.
    # To test bookmarking, include the `bookmark_module`, click on the bookmark
    # button and then get the link. Keep the Shiny app running and open the
    # obtained link in another browser tab.
    onBookmark(function(state) {
      # this function is isolated  by Shiny
      # We store the entire R6 class with reactive values in it, but set the data to NULL.
      # Note that we cannnot directly do this on datasets as this would trigger
      # reactivity to recompute the filtered datasets, which is not needed.
      logger::log_trace(
        paste(
          "srv_teal@2 saving active filter state for",
          "datasets: { paste(names(datasets_reactive()$get_bookmark_state()), collapse = ' ') }."
        )
      )
      state$values$datasets_state <- datasets_reactive()$get_bookmark_state()

    })
    saved_datasets_state <- reactiveVal(NULL) # set when restored because data must already be populated
    onRestore(function(state) {
      # The saved datasets mainly contains the filter states as the data
      # was set to NULL before storing. The data should have been set again
      # by the user, so we just need to set the filters.
      logger::log_trace(
        paste(
          "srv_teal@2 restoring filter states from the bookmark for",
          "datasets: { paste(names(state$values$datasets_state), collapse = ' ') }."
        )
      )
      saved_datasets_state(state$values$datasets_state)
    })

    # initialize datasets ------
    datasets_reactive <- reactive({
      if (is.null(raw_data())) return(NULL)
      progress <<- shiny::Progress$new(session)
      progress$set(0.25, message = "Setting data")
      # create the FilteredData object (here called 'datasets') whose class depends on the class of raw_data()
      # this is placed in the module scope so that bookmarking can be used with FilteredData object
      datasets <- filtered_data_new(raw_data())
      # transfer the datasets from raw_data() into the FilteredData object
      filtered_data_set(raw_data(), datasets)
      logger::log_trace("Raw Data transferred to FilteredData.")
      datasets
    })

    # Replace splash / welcome screen once data is loaded ----
    # ignoreNULL to not trigger at the beginning when data is NULL
    # just handle it once because data obtained through delayed loading should
    # usually not change afterwards
    # if restored from bookmarked state, `filter` is ignored
    observeEvent(datasets_reactive(), ignoreNULL = TRUE, once = TRUE, {
      logger::log_trace("srv_teal@3 setting main ui after data was pulled")
      on.exit(progress$close())
      # main_ui_container contains splash screen first and we remove it and replace it by the real UI
      progress$set(0.5, message = "Setting up main UI")
      removeUI(sprintf("#%s:first-child", session$ns("main_ui_container")))
      insertUI(
        selector = paste0("#", session$ns("main_ui_container")),
        where = "beforeEnd",
        # we put it into a div, so it can easily be removed as a whole, also when it is a tagList (and not
        # just the first item of the tagList)
        ui = div(ui_tabs_with_filters(session$ns("main_ui"), modules = modules, datasets = datasets_reactive())),
        # needed so that the UI inputs are available and can be immediately updated, otherwise, updating may not
        # have any effect as they are ignored when not present, see note in `module_add_filter_variable.R`
        immediate = TRUE
      )

      if (!is.null(saved_datasets_state())) {
        # actual thing to restore
        # cannot call this directly in onRestore because the data is not set at that time
        # for example, the data may only be loaded once a password is provided
        # however, onRestore only runs in the first flush and not in the flush when the
        # password was finally provided
        tryCatch({
          progress$set(0.75, message = "Restoring from bookmarked state")
          filtered_data_set_filters(datasets_reactive(), saved_datasets_state())

        },
        error = function(cnd) {
          logger::log_error("Attempt to set bookmark state failed.")
          showModal(
            modalDialog(
              div(
                p("Could not restore the session: "),
                tags$pre(id = session$ns("error_msg"), cnd$message)
              ),
              title = "Error restoring the bookmarked state",
              footer = tagList(
                actionButton(
                  "copy_code", "Copy to Clipboard",
                  `data-clipboard-target` = paste0("#", session$ns("error_msg"))
                ),
                modalButton("Dismiss")
              ),
              size = "l",
              easyClose = TRUE
            )
          )
        }
        )
      } else {
        progress$set(0.75, message = "Setting initial filter state")
        logger::log_trace("srv_teal@4 setting the initial filter state.")
        filtered_data_set_filters(datasets_reactive(), filter)
      }
      # must make sure that this is only executed once as modules assume their observers are only
      # registered once (calling server functions twice would trigger observers twice each time)
      # `once = TRUE` ensures this
      active_module <- srv_tabs_with_filters(id = "main_ui", datasets =  datasets_reactive(), modules = modules)

      showNotification("Data loaded - App fully started up")

      return(active_module)
    })
  })
}
