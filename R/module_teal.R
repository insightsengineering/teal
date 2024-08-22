#' `teal` main module
#'
#' @description
#' `r lifecycle::badge("stable")`
#' Module to create a `teal` app. This module can be called directly instead of [init()] and
#' included in your custom application. Please note that [init()] adds `reporter_previewer_module`
#' automatically, which is not a case when calling `ui/srv_teal` directly.
#'
#' @details
#'
#' Module is responsible for creating the main `shiny` app layout and initializing all the necessary
#' components. This module establishes reactive connection between the input `data` and every other
#' component in the app. Reactive change of the `data` passed as an argument, reloads the app and
#' possibly keeps all input settings the same so the user can continue where one left off.
#'
#' ## data flow in `teal` application
#'
#' This module supports multiple data inputs but eventually, they are all converted to `reactive`
#' returning `teal_data` in this module. On this `reactive teal_data` object several actions are
#' performed:
#' - data loading in [`module_init_data`]
#' - data filtering in [`module_filter_data`]
#' - data transformation in [`module_transform_data`]
#'
#' @rdname module_teal
#' @name module_teal
#'
#' @inheritParams module_init_data
#' @inheritParams init
#'
#' @return `NULL` invisibly
NULL

#' @rdname module_teal
#' @export
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
      icon("arrows-rotate", class = "fa-spin", prefer_type = "solid"),
      "Computing ...",
      # CSS defined in `custom.css`
      class = "shinybusymessage"
    )
  )

  bookmark_panel_ui <- ui_bookmark_panel(ns("bookmark_manager"), modules)
  data_elem <- ui_init_data(ns("data"), data = data)
  if (!is.null(data)) {
    modules$children <- c(list(teal_data_module = data_elem), modules$children)
  }
  tabs_elem <- ui_teal_module(id = ns("teal_modules"), modules = modules)

  fluidPage(
    id = id,
    title = title,
    theme = get_teal_bs_theme(),
    include_teal_css_js(),
    tags$header(header),
    tags$hr(class = "my-2"),
    shiny_busy_message_panel,
    ui_validate_reactive_teal_data(ns("validate")),
    tags$div(
      id = ns("tabpanel_wrapper"),
      class = "teal-body",
      tabs_elem
    ),
    tags$div(
      id = ns("options_buttons"),
      style = "position: absolute; right: 10px;",
      bookmark_panel_ui,
      tags$button(
        class = "btn action-button filter_hamburger", # see sidebar.css for style filter_hamburger
        href = "javascript:void(0)",
        onclick = sprintf("toggleFilterPanel('%s');", ns("tabpanel_wrapper")),
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
              $('#%s').appendTo('#%s');
            });
          ",
          ns("options_buttons"),
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
#' @export
srv_teal <- function(id, data, modules, filter = teal_slices()) {
  checkmate::assert_character(id, max.len = 1, any.missing = FALSE)
  checkmate::assert_multi_class(data, c("teal_data", "teal_data_module", "reactive", "reactiveVal"))
  checkmate::assert_class(modules, "teal_modules")
  checkmate::assert_class(filter, "teal_slices")

  moduleServer(id, function(input, output, session) {
    logger::log_debug("srv_teal initializing.")
    disable_teal_tabs(session$ns, hide_content = !inherits(data, "teal_data_module"))

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

    data_rv_non_validated <- srv_init_data("data", data = data, modules = modules, filter = filter)
    data_validated <- srv_validate_reactive_teal_data(
      id = "validate",
      data = data_rv_non_validated,
      input_data = data,
      modules = modules,
      validate_shiny_silent_error = FALSE
    )
    data_rv <- .fallback_on_failure(
      this = data_validated,
      that = reactive(req(FALSE)),
      label = "test"
    )

    observeEvent(data_rv_non_validated(), {
      if (inherits(data_rv_non_validated(), "qenv.error")) {
        disable_teal_tabs(session$ns, hide_content = FALSE)
      }
    })

    observeEvent(data_rv(), {
      if (isTRUE(attr(data, "once"))) {
        shinyjs::hide(
          selector = sprintf(
            ".teal-body:has('#%s') a[data-value='teal_data_module']",
            session$ns("options_buttons")
          )
        )
        shinyjs::runjs(
          sprintf(
            "document.querySelector('.teal-body:has(#%s) .nav li:nth-child(2) a').click();",
            session$ns("options_buttons")
          )
        )
      }
    })

    if (inherits(data, "reactive")) {
      observeEvent(data(), {
        if (inherits(data(), "qenv.error")) {
          disable_teal_tabs(session$ns)
        }
      })
    }


    datasets_rv <- if (!isTRUE(attr(filter, "module_specific"))) {
      eventReactive(data_rv(), {
        req(inherits(data_rv(), "teal_data"))
        logger::log_debug("srv_teal@1 initializing FilteredData")
        enable_teal_tabs(session$ns)
        teal_data_to_filtered_data(data_rv())
      })
    }

    module_labels <- unlist(module_labels(modules), use.names = FALSE)
    slices_global <- methods::new(".slicesGlobal", filter, module_labels)
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

    if (inherits(data, "teal_data_module")) {
      setBookmarkExclude(c("teal_modules-active_tab"))
    }
  })

  invisible(NULL)
}


#' @rdname module_teal_data
ui_validate_reactive_teal_data <- function(id) {
  tagList(
    uiOutput(NS(id, "shiny_errors")),
    uiOutput(NS(id, "shiny_warnings"))
  )
}

#' @rdname module_teal_data
srv_validate_reactive_teal_data <- function(id, # nolint: object_length
                                            data,
                                            input_data,
                                            modules = NULL,
                                            validate_shiny_silent_error = FALSE) {
  UseMethod("srv_validate_reactive_teal_data", input_data)
}

#' @rdname module_teal_data
srv_validate_reactive_teal_data.default <- function(id, # nolint: object_length
                                                    data,
                                                    input_data,
                                                    modules = NULL,
                                                    validate_shiny_silent_error = FALSE) {
  stop("Unsupported")
}

srv_validate_reactive_teal_data.teal_data <- function(id, # nolint: object_length
                                                      data,
                                                      input_data,
                                                      modules = NULL,
                                                      validate_shiny_silent_error = FALSE) {
  data
}

#' @rdname module_teal_data
srv_validate_reactive_teal_data.teal_data_module <- function(id,
                                                             data,
                                                             input_data,
                                                             modules = NULL,
                                                             validate_shiny_silent_error = FALSE) {
  moduleServer(id, function(input, output, session) {
    if (!is.reactive(data)) {
      stop("The `teal_data_module` passed to `data` must return a reactive expression.", call. = FALSE)
    }

    data_out_rv <- reactive(tryCatch(data(), error = function(e) e))

    data_validated <- reactive({
      # custom module can return error
      data_out <- data_out_rv()

      # there is an empty reactive cycle on init!
      if (inherits(data_out, "shiny.silent.error") && identical(data_out$message, "")) {
        if (!validate_shiny_silent_error) {
          return(NULL)
        } else {
          validate(
            need(
              FALSE,
              paste(
                strip_style(data_out$message),
                "Check your inputs or contact app developer if error persists.",
                sep = ifelse(identical(data_out$message, ""), "", "\n")
              )
            )
          )
        }
      }

      # to handle errors and qenv.error(s)
      if (inherits(data_out, c("qenv.error", "error"))) {
        validate(
          need(
            FALSE,
            paste(
              "Error when executing `teal_data_module` passed to `data`:\n ",
              strip_style(paste(data_out$message, collapse = "\n")),
              "\n Check your inputs or contact app developer if error persists."
            )
          )
        )
      }

      validate(
        need(
          inherits(data_out, "teal_data"),
          paste(
            "Error: `teal_data_module` passed to `data` failed to return `teal_data` object, returned",
            strip_style(toString(sQuote(class(data_out)))),
            "instead.",
            "\n Check your inputs or contact app developer if error persists."
          )
        )
      )

      data_out
    })

    output$shiny_errors <- renderUI({
      data_validated()
      NULL
    })

    output$shiny_warnings <- renderUI({
      if (inherits(data_out_rv(), "teal_data")) {
        is_modules_ok <- check_modules_datanames(modules = modules, datanames = .teal_data_ls(data_validated()))
        if (!isTRUE(is_modules_ok)) {
          tags$div(
            is_modules_ok$html(
              # Show modules prefix on message only in teal_data_module tab
              grepl(sprintf("data-teal_data_module-%s", id), session$ns(NULL), fixed = TRUE)
            ),
            class = "teal-output-warning"
          )
        }
      }
    })

    data_validated
  })
}

srv_validate_reactive_teal_data.reactive <- function(id,
                                                     data,
                                                     input_data,
                                                     modules = NULL,
                                                     validate_shiny_silent_error = FALSE) {
  moduleServer(id, function(input, output, session) {
    if (!is.reactive(data)) {
      stop("The `teal_data_module` passed to `data` must return a reactive expression.", call. = FALSE)
    }
    data_out_rv <- reactive(tryCatch(data(), error = function(e) e))

    output$shiny_errors <- renderUI({
      validate(
        need(
          !inherits(input_data(), "qenv.error"),
          new_data$message
        )
      )
    })

    data_out_rv
  })
}

#' Fallback on failure
#'
#' Function returns the previous reactive if the current reactive is invalid (throws error or returns NULL).
#' Application: In `teal` we try to prevent the error from being thrown and instead we replace failing
#' transform module data output with data input from the previous module (or from previous `teal` reactive
#' tree elements).
#'
#' @param this (`reactive`) Current reactive.
#' @param that (`reactive`) Previous reactive.
#' @param label (`character`) Label for identifying problematic `teal_data_module` transform in logging.
#' @return `reactive` `teal_data`
#' @keywords internal
.fallback_on_failure <- function(this, that, label) {
  checkmate::assert_class(this, "reactive")
  checkmate::assert_class(that, "reactive")
  checkmate::assert_string(label)

  reactive({
    res <- try(this(), silent = TRUE)
    if (inherits(res, "teal_data")) {
      logger::log_debug("{ label } evaluated successfully.")
      res
    } else {
      logger::log_debug("{ label } failed, falling back to previous data.")
      that()
    }
  })
}


disable_teal_tabs <- function(ns, hide_content = TRUE) {
  shinyjs::disable(selector = sprintf(".teal-body:has('#%s') .nav li a", ns("options_buttons")))
  if (hide_content) {
    shinyjs::runjs(
      sprintf(
        '$("#%s .tab-content").hide()',
        ns("tabpanel_wrapper")
      )
    )
  }
}

enable_teal_tabs <- function(ns) {
  shinyjs::enable(selector = sprintf(".teal-body:has('#%s') .nav li a", ns("options_buttons")))
  shinyjs::runjs(
    sprintf(
      '$("#%s .tab-content").show()',
      ns("tabpanel_wrapper")
    )
  )
  shinyjs::runjs(
    sprintf(
      '$("#%s .tab-content").trigger("shown");',
      ns("tabpanel_wrapper")
    )
  )
}
