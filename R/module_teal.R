# This module is the main teal module that puts everything together.

#' Teal app UI
#'
#' This is the main teal UI that puts everything together.
#'
#' It displays the splash UI which is used to fetch the data, possibly
#' prompting for a password input to fetch the data. Once the data is ready,
#' the splash screen is replaced by the actual teal UI that is tabsetted and
#' has a filter panel with `datanames` that are relevant for the current tab.
#' Nested tabs are possible, but we limit it to two nesting levels for reasons
#' of clarity of the UI.
#'
#' The splash screen functionality can also be used
#' for non-delayed data which takes time to load into memory, avoiding
#' Shiny session timeouts.
#'
#'
#' It is written as a Shiny module so it can be added into other apps as well.
#'
#' @param splash_ui `shiny.tag` UI to display initially,
#'   can be a splash screen or a Shiny module UI. For the latter, see
#'   [init()] about how to call the corresponding server function.
#' @inheritParams ui_teal_with_splash
#'
#' @return `HTML` for Shiny module UI
#' @keywords internal
#'
#' @examples
#' mods <- teal:::example_modules()
#' raw_data <- reactive(teal:::example_cdisc_data())
#' app <- shinyApp(
#'   ui = function() {
#'     teal:::ui_teal("dummy")
#'   },
#'   server = function(input, output, session) {
#'     active_module <- teal:::srv_teal(id = "dummy", modules = mods, raw_data = raw_data)
#'   }
#' )
#' if (interactive()) {
#'   runApp(app)
#' }
ui_teal <- function(id,
                    splash_ui = tags$h2("Starting the Teal App"),
                    title = NULL,
                    header = tags$p(""),
                    footer = tags$p("")) {
  if (checkmate::test_string(header)) {
    header <- tags$h1(header)
  }
  if (checkmate::test_string(footer)) {
    footer <- tags$p(footer)
  }
  checkmate::assert(
    checkmate::check_class(splash_ui, "shiny.tag"),
    checkmate::check_class(splash_ui, "shiny.tag.list"),
    checkmate::check_class(splash_ui, "html")
  )
  checkmate::assert(
    checkmate::check_class(header, "shiny.tag"),
    checkmate::check_class(header, "shiny.tag.list"),
    checkmate::check_class(header, "html")
  )
  checkmate::assert(
    checkmate::check_class(footer, "shiny.tag"),
    checkmate::check_class(footer, "shiny.tag.list"),
    checkmate::check_class(footer, "html")
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
      icon("arrows-rotate", "spin fa-spin"),
      "Computing ...",
      # CSS defined in `custom.css`
      class = "shinybusymessage"
    )
  )

  res <- fluidPage(
    title = title,
    theme = get_teal_bs_theme(),
    include_teal_css_js(),
    tags$header(header),
    tags$hr(class = "my-2"),
    shiny_busy_message_panel,
    splash_ui,
    tags$hr(),
    tags$footer(
      div(
        footer,
        teal.widgets::verbatim_popup_ui(ns("sessionInfo"), "Session Info", type = "link"),
        textOutput(ns("identifier"))
      )
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
#' updates the displayed datasets to filter for according to the active `datanames`
#' of the tab.
#'
#' For more doc, see [ui_teal()].
#'
#' @inheritParams init
#' @param raw_data (`reactive`)\cr
#'   returns the `TealData`, only evaluated once, `NULL` value is ignored
#'
#' @return `reactive` which returns the currently active module
#' @keywords internal
srv_teal <- function(id, modules, raw_data, filter = teal_slices()) {
  stopifnot(is.reactive(raw_data))
  moduleServer(id, function(input, output, session) {
    logger::log_trace("srv_teal initializing the module.")

    output$identifier <- renderText(
      paste0("Pid:", Sys.getpid(), " Token:", substr(session$token, 25, 32))
    )

    teal.widgets::verbatim_popup_srv(
      "sessionInfo",
      verbatim_content = utils::capture.output(utils::sessionInfo()),
      title = "SessionInfo"
    )

    # `JavaScript` code
    run_js_files(files = "init.js") # `JavaScript` code to make the clipboard accessible
    # set timezone in shiny app
    # timezone is set in the early beginning so it will be available also
    # for `DDL` and all shiny modules
    get_client_timezone(session$ns)
    observeEvent(
      eventExpr = input$timezone,
      once = TRUE,
      handlerExpr = {
        session$userData$timezone <- input$timezone
        logger::log_trace("srv_teal@1 Timezone set to client's timezone: { input$timezone }.")
      }
    )

    # loading the data -----
    env <- environment()
    datasets_reactive <- reactive({
      if (is.null(raw_data())) {
        return(NULL)
      }
      env$progress <- shiny::Progress$new(session)
      env$progress$set(0.25, message = "Setting data")

      # create a list of data following structure of the nested modules list structure.
      # Because it's easier to unpack modules and datasets when they follow the same nested structure.
      datasets_singleton <- teal.slice::init_filtered_data(raw_data())
      datasets_singleton$set_filter_state(filter)
      module_datasets <- function(modules) {
        if (inherits(modules, "teal_modules")) {
          datasets <- lapply(modules$children, module_datasets)
          labels <- vapply(modules$children, `[[`, character(1), "label")
          names(datasets) <- labels
          datasets
        } else if (isTRUE(attr(filter, "module_specific"))) {
          # we should create FilteredData even if modules$filter is null
          # null controls a display of filter panel but data should be still passed
          datanames <- if (is.null(modules$filter)) raw_data()$get_datanames() else modules$filter
          data_objects <- sapply(
            datanames,
            simplify = FALSE,
            FUN = function(dataname) {
              dataset <- raw_data()$get_dataset(dataname)
              list(
                dataset = dataset$get_raw_data(),
                metadata = dataset$get_metadata(),
                label = dataset$get_dataset_label()
              )
            }
          )
          datasets_module <- teal.slice::init_filtered_data(
            data_objects,
            join_keys = raw_data()$get_join_keys(),
            code = raw_data()$get_code_class(),
            check = raw_data()$get_check()
          )

          # set initial filters
          slices <- Filter(x = filter, f = function(x) {
            x$id %in% unique(unlist(attr(filter, "mapping")[c(modules$label, "global_filters")])) &&
              x$dataname %in% datanames
          })
          include_varnames <- attr(slices, "include_varnames")[names(attr(slices, "include_varnames")) %in% datanames]
          exclude_varnames <- attr(slices, "exclude_varnames")[names(attr(slices, "exclude_varnames")) %in% datanames]
          slices$include_varnames <- include_varnames
          slices$exclude_varnames <- exclude_varnames
          datasets_module$set_filter_state(slices)
          datasets_module
        } else {
          datasets_singleton
        }
      }
      datasets <- module_datasets(modules)

      logger::log_trace("srv_teal@4 Raw Data transferred to FilteredData.")
      datasets
    })

    reporter <- teal.reporter::Reporter$new()
    is_any_previewer <- function(modules) {
      if (inherits(modules, "teal_modules")) {
        any(unlist(lapply(modules$children, is_any_previewer), use.names = FALSE))
      } else if (inherits(modules, "teal_module_previewer")) {
        TRUE
      } else {
        FALSE
      }
    }
    if (is_arg_used(modules, "reporter") && !is_any_previewer(modules)) {
      modules <- append_module(modules, reporter_previewer_module())
    }

    # Replace splash / welcome screen once data is loaded ----
    # ignoreNULL to not trigger at the beginning when data is NULL
    # just handle it once because data obtained through delayed loading should
    # usually not change afterwards
    # if restored from bookmarked state, `filter` is ignored
    observeEvent(datasets_reactive(), ignoreNULL = TRUE, once = TRUE, {
      logger::log_trace("srv_teal@5 setting main ui after data was pulled")
      env$progress$set(0.5, message = "Setting up main UI")
      on.exit(env$progress$close())
      # main_ui_container contains splash screen first and we remove it and replace it by the real UI

      removeUI(sprintf("#%s:first-child", session$ns("main_ui_container")))
      insertUI(
        selector = paste0("#", session$ns("main_ui_container")),
        where = "beforeEnd",
        # we put it into a div, so it can easily be removed as a whole, also when it is a tagList (and not
        # just the first item of the tagList)
        ui = div(ui_tabs_with_filters(
          session$ns("main_ui"),
          modules = modules,
          datasets = datasets_reactive(),
          filter = filter
        )),
        # needed so that the UI inputs are available and can be immediately updated, otherwise, updating may not
        # have any effect as they are ignored when not present
        immediate = TRUE
      )

      # must make sure that this is only executed once as modules assume their observers are only
      # registered once (calling server functions twice would trigger observers twice each time)
      active_module <- srv_tabs_with_filters(
        id = "main_ui",
        datasets = datasets_reactive(),
        modules = modules,
        reporter = reporter,
        filter = filter
      )
      return(active_module)
    })
  })
}
