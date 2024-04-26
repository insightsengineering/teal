# todo:
# x add teal_data_module somewhere
# x make sure global reactive-filters are executed once (log: filtering data dataname: TITANIC)
# x add transform module
# - investigate duplicated logs (retriggered reactives)
# - fix nested tabs
# - make reactive assertions on teal_data (on a transform module output)
# - disable modules when no data is loaded
# - add module-specific-filter-panel
# - add wunderbar buttons
# - connect filter manager with reactive filter panel!
# - add reporter previewer
# - what if data is reloaded? What happens with filter-panel? Is bookmarking working after resubmitting data?
#   I hope resubmitting data doesn't register new observers but rather overwrite previous
ui_teal_1.0 <- function(id, data, modules, title, header, footer) {
  ns <- shiny::NS(id)
  data_ui <- ui_data(id = ns("_data_"), data = data, title = title, header = header, footer = footer)
  bslib::page_fluid(
    title = title,
    bslib::card_header(header),
    bslib::navset_tab(
      id = ns("root_tab"),
      bslib::nav_panel(
        title = "Data",
        icon = bsicons::bs_icon("table"),
        data_ui
      ),
      !!!ui_teal_module(id = ns("root_module"), modules = modules),
      bslib::nav_spacer(),
      bslib::nav_menu(
        title = "App controls",
        icon = bsicons::bs_icon("gear"),
        !!!wunder_bar_ui(id = ns("wunder_bar"))
      )
    ),
    bslib::card_footer(footer)
  )
}

srv_teal_1.0 <- function(id, data, modules, filter) {
  checkmate::assert_character(id, max.len = 1, any.missing = FALSE)
  checkmate::assert_multi_class(data, c("teal_data", "teal_data_module"))
  checkmate::assert_class(modules, "teal_modules")
  checkmate::assert_class(filter, "teal_slices")
  moduleServer(id, function(input, output, session) {
    data_rv <- srv_data(id = "_data_", data = data, modules = modules, filter = filter)
    filtered_data_rv <- reactive_filtered_data(id = "filter_panel", filter = filter, data = data_rv)

    observeEvent(
      ignoreNULL = FALSE,
      data_rv(),
      {
        if (is.null(data_rv())) {
          shinyjs::disable(selector = ".nav-link")
        } else {
          shinyjs::enable(selector = ".nav-link")
        }
      }
    )
    # reactive data created here to keep it in a global context
    # to avoid refiltering for each module
    data_list_rv <- reactive({
      if (is.null(filtered_data_rv())) {
        # disable all tab links under #root_tab
        return(NULL)
      }

      data_list <- sapply(
        filtered_data_rv()$datanames(),
        USE.NAMES = TRUE,
        function(dataname) {
          reactive({
            dataset <- filtered_data_rv()$get_data(dataname, filtered = TRUE)
            # isolate as it is already triggered by the data change
            attr(dataset, "filter_code") <- shiny::isolate(filtered_data_rv()$get_call(dataname))
            dataset
          })
        }
      )
      attr(data_list, "code") <- teal.data::get_code(data_rv())
      attr(data_list, "join_keys") <- teal.data::join_keys(data_rv())

      data_list
    })

    wunder_bar_srv(id = "wunder_bar", datasets = filtered_data_rv(), filter = filter, modules = modules)

    srv_teal_module(
      id = "root_module",
      data = data_list_rv,
      datasets = filtered_data_rv,
      modules = modules
    )
  })
}
