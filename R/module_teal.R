# This module is the main teal module that puts everything together.

#' `teal` main app module
#'
#' This module is a central point of the `teal` app. It is called by [teal::init()] but can be also
#' used as a standalone module in your custom application. It is responsible for creating the main
#' `shiny` app layout and initializing all the necessary components:
#' - [`module_data`] - for handling the `data`.
#' - [`module_teal_module`] - for handling the `modules`.
#' - [`module_filter_manager`] - for handling the `filter`.
#' - [`module_snapshot_manager`] - for handling the `snapshots`.
#' - [`module_bookmark_manager`] - for handling the `bookmarks`.
#'
#' This module establishes reactive connection between the `data` and every other component in the app.
#' Reactive change of the `data` triggers reload of the app and possibly keeping all inputs settings
#' the same so the user can continue where one left off.
#' Similar applies to [`module_bookmark_manager`] which allows to start a new session with restored
#' inputs.
#'
#' @rdname module_teal
#' @name module_teal
#'
#' @inheritParams module_data
#' @inheritParams init
#'
#' @return
#' Returns a `reactive` expression which returns the currently active module.
#'
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
  tabs_elem <- ui_teal_module(id = ns("teal_modules"), modules = modules)

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
    tags$script(
      HTML(
        sprintf(
          "
            $(document).ready(function() {
              $('#teal-util-icons').appendTo('#%s');
            });
          ",
          ns("teal_modules-active_tab")
        )
      )
    ),
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
srv_teal <- function(id, data, modules, filter = teal_slices()) {
  checkmate::assert_character(id, max.len = 1, any.missing = FALSE)
  checkmate::assert_multi_class(data, c("teal_data", "teal_data_module", "reactive", "reactiveVal"))
  checkmate::assert_class(modules, "teal_modules")
  checkmate::assert_class(filter, "teal_slices")

  moduleServer(id, function(input, output, session) {
    logger::log_debug("srv_teal initializing.")

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
        logger::log_debug("srv_teal@1 Timezone set to client's timezone: { input$timezone }.")
      }
    )

    # todo: introduce option `run_once` to not show data icon when app is loaded (in case when data don't change).
    data_rv <- srv_data("data", data = data, modules = modules, filter = filter)
    datasets_rv <- if (!isTRUE(attr(filter, "module_specific"))) {
      eventReactive(data_rv(), {
        if (!inherits(data_rv(), "teal_data")) {
          stop("data_rv must be teal_data object.")
        }
        logger::log_debug("srv_teal_module@1 initializing FilteredData")
        teal_data_to_filtered_data(data_rv())
      })
    }

    module_labels <- unlist(module_labels(modules), use.names = FALSE)
    slices_global <- slicesGlobal$new(filter, module_labels)
    modules_output <- srv_teal_module(
      id = "teal_modules",
      data_rv = data_rv,
      datasets = datasets_rv,
      modules = modules,
      slices_global = slices_global
    )
    mapping_table <- srv_filter_manager_panel("filter_manager_panel", slices_global = slices_global)
    snapshots <- srv_snapshot_manager_panel("snapshot_manager_panel", slices_global = slices_global)
    srv_bookmark_panel("bookmark_manager", modules)
  })
}
