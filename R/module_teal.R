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
#' @keywords internal
ui_teal <- function(id,
                    modules,
                    data = NULL,
                    title = build_app_title(),
                    header = tags$p(),
                    footer = tags$p()) {
  checkmate::assert_character(id, max.len = 1, any.missing = FALSE)
  checkmate::assert_multi_class(data, "teal_data_module", null.ok = TRUE)
  checkmate::assert(
    .var.name = "title",
    checkmate::check_string(title),
    checkmate::check_multi_class(title, c("shiny.tag", "shiny.tag.list", "html"))
  )
  checkmate::assert(
    .var.name = "header",
    checkmate::check_string(header),
    checkmate::check_multi_class(header, c("shiny.tag", "shiny.tag.list", "html"))
  )
  checkmate::assert(
    .var.name = "footer",
    checkmate::check_string(footer),
    checkmate::check_multi_class(footer, c("shiny.tag", "shiny.tag.list", "html"))
  )

  if (is.character(title)) {
    title <- build_app_title(title)
  } else {
    validate_app_title_tag(title)
  }

  if (checkmate::test_string(header)) {
    header <- tags$p(header)
  }

  if (checkmate::test_string(footer)) {
    footer <- tags$p(footer)
  }

  ns <- NS(id)

  # show busy icon when `shiny` session is busy computing stuff
  # based on https://stackoverflow.com/questions/17325521/r-shiny-display-loading-message-while-function-is-running/22475216#22475216 # nolint: line_length.
  shiny_busy_message_panel <- conditionalPanel(
    condition = "(($('html').hasClass('shiny-busy')) && (document.getElementById('shiny-notification-panel') == null))", # nolint: line_length.
    tags$div(
      icon("arrows-rotate", "spin fa-spin"),
      "Computing ...",
      # CSS defined in `custom.css`
      class = "shinybusymessage"
    )
  )

  data_elem <- ui_data(ns("data"), data = data, title = title, header = header, footer = footer)
  tabs_elem <- ui_teal_module(id = ns("root_module"), modules = modules)

  fluidPage(
    title = title,
    theme = get_teal_bs_theme(),
    include_teal_css_js(),
    tags$header(header),
    tags$hr(class = "my-2"),
    shiny_busy_message_panel,
    tabs_elem,
    tags$div(
      id = "teal-util-icons",
      style = "margin-left: auto;",
      data_elem,
      ui_bookmark_panel(ns("bookmark_manager"), modules),
      tags$button(
        class = "btn action-button filter_hamburger", # see sidebar.css for style filter_hamburger
        href = "javascript:void(0)",
        onclick = "toggleFilterPanel();", # see sidebar.js
        title = "Toggle filter panel",
        icon("fas fa-bars")
      ),
      ui_snapshot_manager_panel(ns("snapshot_manager_panel")),
      ui_filter_manager_panel(ns("filter_manager_panel"))
    ),
    tags$script(HTML("
      $(document).ready(function() {
        $('#teal-util-icons').appendTo('#root_module-active_tab');
      });
    ")),
    tags$hr(),
    tags$footer(
      tags$div(
        footer,
        teal.widgets::verbatim_popup_ui(ns("sessionInfo"), "Session Info", type = "link"),
        br(),
        downloadLink(ns("lockFile"), "Download .lock file"),
        textOutput(ns("identifier"))
      )
    )
  )
}

#' @rdname module_teal
#' @keywords internal
srv_teal <- function(id, data, modules, filter = teal_slices()) {
  checkmate::assert_character(id, max.len = 1, any.missing = FALSE)
  checkmate::assert_multi_class(data, c("teal_data", "teal_data_module", "reactive", "reactiveVal"))
  checkmate::assert_class(modules, "teal_modules")
  checkmate::assert_class(filter, "teal_slices")

  moduleServer(id, function(input, output, session) {
    logger::log_trace("srv_teal initializing.")

    output$identifier <- renderText(
      paste0("Pid:", Sys.getpid(), " Token:", substr(session$token, 25, 32))
    )

    teal.widgets::verbatim_popup_srv(
      "sessionInfo",
      verbatim_content = utils::capture.output(utils::sessionInfo()),
      title = "SessionInfo"
    )

    output$lockFile <- teal_lockfile_downloadhandler()

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

    # todo: introduce option `run_once` to not show data icon when app is loaded (in case when data don't change).
    data_rv <- srv_data("data", data = data, modules = modules, filter = filter)
    datasets_rv <- if (!isTRUE(attr(filter, "module_specific"))) {
      eventReactive(data_rv(), {
        logger::log_trace("srv_teal_module@1 initializing FilteredData")
        # Otherwise, FilteredData will be created in the modules' scope later
        progress_data <- Progress$new(
          max = length(unlist(module_labels(modules)))
        )
        on.exit(progress_data$close())
        progress_data$set(message = "Preparing data filtering", detail = "0%")
        filtered_data <- teal_data_to_filtered_data(data_rv())
        filtered_data
      })
    }

    module_labels <- unlist(module_labels(modules), use.names = FALSE)
    slices_global <- .make_slices_global(filter = filter, module_labels = module_labels)
    srv_filter_manager_panel("filter_manager_panel", slices_global = slices_global)

    srv_snapshot_manager_panel("snapshot_manager_panel", slices_global = slices_global)

    srv_bookmark_panel("bookmark_manager", modules)

    # comment: modules needs to be called after srv_filter_manager_panel
    #          This is because they are using session$slices_global which is set in filter_manager_srv
    srv_teal_module(
      id = "root_module",
      data_rv = data_rv,
      datasets = datasets_rv,
      modules = modules,
      slices_global = slices_global
    )
  })
}
