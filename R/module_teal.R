# This module is the main teal module that puts everything together.

#' `teal` main app module
#'
#' This is the main `teal` app that puts everything together.
#'
#' It displays the splash UI which is used to fetch the data, possibly
#' prompting for a password input to fetch the data. Once the data is ready,
#' the splash screen is replaced by the actual `teal` UI that is tabsetted and
#' has a filter panel with `datanames` that are relevant for the current tab.
#' Nested tabs are possible, but we limit it to two nesting levels for reasons
#' of clarity of the UI.
#'
#' The splash screen functionality can also be used
#' for non-delayed data which takes time to load into memory, avoiding
#' `shiny` session timeouts.
#'
#' Server evaluates the `teal_data_rv` (delayed data mechanism) and creates the
#' `datasets` object that is shared across modules.
#' Once it is ready and non-`NULL`, the splash screen is replaced by the
#' main `teal` UI that depends on the data.
#' The currently active tab is tracked and the right filter panel
#' updates the displayed datasets to filter for according to the active `datanames`
#' of the tab.
#'
#' It is written as a `shiny` module so it can be added into other apps as well.
#'
#' @name module_teal
#'
#' @inheritParams module_teal_with_splash
#'
#' @param splash_ui (`shiny.tag`) UI to display initially,
#'   can be a splash screen or a `shiny` module UI. For the latter, see
#'   [init()] about how to call the corresponding server function.
#'
#' @param teal_data_rv (`reactive`)
#'   returns the `teal_data`, only evaluated once, `NULL` value is ignored
#'
#' @return
#' Returns a `reactive` expression which returns the currently active module.
#'
#' @keywords internal
#'
NULL

#' @rdname module_teal
ui_teal <- function(id,
                    splash_ui = tags$h2("Starting the Teal App"),
                    title = build_app_title(),
                    header = tags$p(),
                    footer = tags$p()) {
  checkmate::assert_character(id, max.len = 1, any.missing = FALSE)

  checkmate::assert_multi_class(splash_ui, c("shiny.tag", "shiny.tag.list", "html"))

  if (is.character(title)) {
    title <- build_app_title(title)
  } else {
    validate_app_title_tag(title)
  }

  checkmate::assert(
    .var.name = "header",
    checkmate::check_string(header),
    checkmate::check_multi_class(header, c("shiny.tag", "shiny.tag.list", "html"))
  )
  if (checkmate::test_string(header)) {
    header <- tags$p(header)
  }

  checkmate::assert(
    .var.name = "footer",
    checkmate::check_string(footer),
    checkmate::check_multi_class(footer, c("shiny.tag", "shiny.tag.list", "html"))
  )
  if (checkmate::test_string(footer)) {
    footer <- tags$p(footer)
  }

  ns <- NS(id)

  # Once the data is loaded, we will remove this element and add the real teal UI instead
  splash_ui <- div(
    # id so we can remove the splash screen once ready, which is the first child of this container
    id = ns("main_ui_container"),
    # we put it into a div, so it can easily be removed as a whole, also when it is a tagList (and not
    # just the first item of the tagList)
    div(splash_ui)
  )

  # show busy icon when `shiny` session is busy computing stuff
  # based on https://stackoverflow.com/questions/17325521/r-shiny-display-loading-message-while-function-is-running/22475216#22475216 # nolint
  shiny_busy_message_panel <- conditionalPanel(
    condition = "(($('html').hasClass('shiny-busy')) && (document.getElementById('shiny-notification-panel') == null))", # nolint
    div(
      icon("arrows-rotate", "spin fa-spin"),
      "Computing ...",
      # CSS defined in `custom.css`
      class = "shinybusymessage"
    )
  )

  fluidPage(
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
}


#' @rdname module_teal
srv_teal <- function(id, modules, teal_data_rv, filter = teal_slices()) {
  stopifnot(is.reactive(teal_data_rv))
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
    run_js_files(files = "init.js")

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

    reporter <- teal.reporter::Reporter$new()
    if (is_arg_used(modules, "reporter") && length(extract_module(modules, "teal_module_previewer")) == 0) {
      modules <- append_module(modules, reporter_previewer_module())
    }

    env <- environment()
    datasets_reactive <- eventReactive(teal_data_rv(), {
      env$progress <- shiny::Progress$new(session)
      env$progress$set(0.25, message = "Setting data")

      # create a list of data following structure of the nested modules list structure.
      # Because it's easier to unpack modules and datasets when they follow the same nested structure.
      datasets_singleton <- teal_data_to_filtered_data(teal_data_rv())

      # Singleton starts with only global filters active.
      filter_global <- Filter(function(x) x$id %in% attr(filter, "mapping")$global_filters, filter)
      datasets_singleton$set_filter_state(filter_global)

      module_datasets <- function(modules) {
        if (inherits(modules, "teal_modules")) {
          datasets <- lapply(modules$children, module_datasets)
          labels <- vapply(modules$children, `[[`, character(1), "label")
          names(datasets) <- labels
          datasets
        } else if (isTRUE(attr(filter, "module_specific"))) {
          # we should create FilteredData even if modules$datanames is null
          # null controls a display of filter panel but data should be still passed
          datanames <- if (is.null(modules$datanames) || identical(modules$datanames, "all")) {
            include_parent_datanames(
              teal_data_datanames(teal_data_rv()),
              teal.data::join_keys(teal_data_rv())
            )
          } else {
            modules$datanames
          }
          # todo: subset teal_data to datanames
          datasets_module <- teal_data_to_filtered_data(teal_data_rv(), datanames = datanames)

          # set initial filters
          #  - filtering filters for this module
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
      module_datasets(modules)
    })

    # Replace splash / welcome screen once data is loaded ----
    # ignoreNULL to not trigger at the beginning when data is NULL
    # just handle it once because data obtained through delayed loading should
    # usually not change afterwards
    # if restored from bookmarked state, `filter` is ignored

    observeEvent(datasets_reactive(), once = TRUE, {
      logger::log_trace("srv_teal@5 setting main ui after data was pulled")
      on.exit(env$progress$close())
      env$progress$set(0.5, message = "Setting up main UI")
      datasets <- datasets_reactive()

      # main_ui_container contains splash screen first and we remove it and replace it by the real UI
      removeUI(sprintf("#%s > div:nth-child(1)", session$ns("main_ui_container")))
      insertUI(
        selector = paste0("#", session$ns("main_ui_container")),
        where = "beforeEnd",
        # we put it into a div, so it can easily be removed as a whole, also when it is a tagList (and not
        # just the first item of the tagList)
        ui = div(ui_tabs_with_filters(
          session$ns("main_ui"),
          modules = modules,
          datasets = datasets,
          filter = filter
        )),
        # needed so that the UI inputs are available and can be immediately updated, otherwise, updating may not
        # have any effect as they are ignored when not present
        immediate = TRUE
      )

      # must make sure that this is only executed once as modules assume their observers are only
      # registered once (calling server functions twice would trigger observers twice each time)
      srv_tabs_with_filters(
        id = "main_ui",
        datasets = datasets,
        modules = modules,
        reporter = reporter,
        filter = filter
      )
    })
  })
}
