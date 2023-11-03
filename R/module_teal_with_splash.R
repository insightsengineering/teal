# This file adds a splash screen for delayed data loading on top of teal

#' UI to show a splash screen in the beginning, then delegate to [srv_teal()]
#'
#' @description `r lifecycle::badge("stable")`
#' The splash screen could be used to query for a password to fetch the data.
#' [init()] is a very thin wrapper around this module useful for end-users which
#' assumes that it is a top-level module and cannot be embedded.
#' This function instead adheres to the Shiny module conventions.
#'
#' If data is obtained through delayed loading, its splash screen is used. Otherwise,
#' a default splash screen is shown.
#'
#' Please also refer to the doc of [init()].
#'
#' @param id (`character(1)`)\cr
#'   module id
#' @inheritParams init
#' @export
ui_teal_with_splash <- function(id,
                                data,
                                title,
                                header = tags$p("Add Title Here"),
                                footer = tags$p("Add Footer Here")) {
  checkmate::assert(
    checkmate::check_multi_class(data, c("TealData", "teal_data")),
    check_shiny_module_list(data)
  )
  ns <- NS(id)

  # Startup splash screen for delayed loading
  # We use delayed loading in all cases, even when the data does not need to be fetched.
  # This has the benefit that when filtering the data takes a lot of time initially, the
  # Shiny app does not time out.

  splash_ui <- if (test_shiny_module_list(data)) {
    data$ui(ns("data"))
  } else if (inherits(data, "teal_data")) {
    div()
  } else if (inherits(data, "TealDataAbstract") && teal.data::is_pulled(data)) {
    div()
  } else {
    message("App was initialized with delayed data loading.")
    data$get_ui(ns("startapp_module"))
  }

  ui_teal(id = ns("teal"), splash_ui = splash_ui, title = title, header = header, footer = footer)
}

#' Server function that loads the data through reactive loading and then delegates
#' to [srv_teal()].
#'
#' @description `r lifecycle::badge("stable")`
#' Please also refer to the doc of [init()].
#'
#' @inheritParams init
#' @param modules `teal_modules` object containing the output modules which
#'   will be displayed in the teal application. See [modules()] and [module()] for
#'   more details.
#' @inheritParams shiny::moduleServer
#' @return `reactive` containing `teal_data` object when data is loaded.
#' If data is not loaded yet, `reactive` returns `NULL`.
#' @export
srv_teal_with_splash <- function(id, data, modules, filter = teal_slices()) {
  checkmate::assert(
    checkmate::check_multi_class(data, c("TealData", "teal_data")),
    check_shiny_module_list(data)
  )
  moduleServer(id, function(input, output, session) {
    logger::log_trace("srv_teal_with_splash initializing module with data.")

    if (getOption("teal.show_js_log", default = FALSE)) {
      shinyjs::showLog()
    }

    # raw_data contains teal_data object
    # either passed to teal::init or returned from ddl
    raw_data <- if (test_shiny_module_list(data)) {
      ddl_out <- do.call(
        data$server,
        append(
          list(id = "data"),
          attr(data, "server_args") # might be NULL or list() - both are fine
        ),
        quote = TRUE
      )
    } else if (inherits(data, "teal_data")) {
      reactiveVal(data)
    } else if (inherits(data, "TealDataAbstract") && teal.data::is_pulled(data)) {
      new_data <- do.call(
        teal.data::teal_data,
        c(
          lapply(data$get_datasets(), function(x) x$get_raw_data()),
          code = data$get_code(),
          join_keys = data$get_join_keys()
        )
      )
      reactiveVal(new_data) # will trigger by setting it
    } else {
      raw_data_old <- data$get_server()(id = "startapp_module")
      raw_data <- reactive({
        data <- raw_data_old()
        if (!is.null(data)) {
          # raw_data is a reactive which returns data only when submit button clicked
          # otherwise it returns NULL
          do.call(
            teal.data::teal_data,
            c(
              lapply(data$get_datasets(), function(x) x$get_raw_data()),
              code = data$get_code(),
              join_keys = data$get_join_keys()
            )
          )
        }
      })

      if (!is.reactive(raw_data)) {
        stop("The delayed loading module has to return a reactive object.")
      }
      raw_data
    }

    raw_data_checked <- reactive({
      data <- raw_data()
      if (inherits(data, "qenv.error")) {
        #
        showNotification(sprintf("Error: %s", data$message))
        return(NULL)
      }

      is_modules_ok <- check_modules_datanames(modules, teal.data::datanames(data))
      is_filter_ok <- check_filter_datanames(filter, teal.data::datanames(data))

      if (!isTRUE(is_modules_ok)) {
        showNotification(is_modules_ok)
        # NULL won't trigger observe which waits for raw_data()
        # we will need to consider validate process for filtered data and modules!
        return(NULL)
      }
      if (!isTRUE(is_filter_ok)) {
        showNotification(is_filter_ok)
        # we allow app to continue if applied filters are outside
        # of possible data range
      }

      data
    })


    res <- srv_teal(id = "teal", modules = modules, raw_data = raw_data_checked, filter = filter)
    logger::log_trace("srv_teal_with_splash initialized module with data.")
    return(res)
  })
}
