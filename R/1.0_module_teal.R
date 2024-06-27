ui_teal_1.0 <- function(id,
                        data,
                        modules,
                        title = build_app_title(),
                        header = tags$p(),
                        footer = tags$p()) {
  checkmate::assert_character(id, max.len = 1, any.missing = FALSE)
  checkmate::assert_multi_class(data, c("teal_data", "teal_data_module"))
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
      actionButton(ns("filter_manager"), NULL, icon = icon("filter")),
      actionButton(ns("snapshot_manager"), NULL, icon = icon("floppy-disk")),
      actionButton(ns("bookmark_manager"), NULL, icon = icon("bookmark"))
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

srv_teal_1.0 <- function(id, data, modules, filter = teal_slices()) {
  checkmate::assert_character(id, max.len = 1, any.missing = FALSE)
  checkmate::assert_multi_class(data, c("teal_data", "teal_data_module"))
  checkmate::assert_class(modules, "teal_modules")
  checkmate::assert_class(filter, "teal_slices")

  moduleServer(id, function(input, output, session) {
    logger::log_trace("srv_teal_with_splash initializing module with data.")
    data_reactive <- srv_data("data", data = data, modules = modules, filter = filter)

    observe({
      logger::log_trace("data changed")
      print(data_reactive())
    })

    observeEvent(input$filter_manager, {
      print("filter_manager clicked!")
    })
    observeEvent(input$snapshot_manager, {
      print("snapshot_manager clicked!")
    })
    observeEvent(input$bookmark_manager, {
      print("bookmark_manager clicked!")
    })
  })
}

# srv_teal_with_splash -> srv_teal -> srv_tabs_with_filters -> srv_nested_tabs
# srv_teal
