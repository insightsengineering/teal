library(shinytest2)
library(rvest)

default_idle_timeout <- 20000

simple_teal_data <- function() {
  data <- within(teal_data(), {
    iris <- iris
    mtcars <- mtcars
  })
  datanames(data) <- c("iris", "mtcars")
  data
}

report_module <- function(label = "example teal module") {
  module(
    label = label,
    server = function(id, data, reporter) {
      moduleServer(id, function(input, output, session) {
        teal.reporter::simple_reporter_srv(
          id = "reporter",
          reporter = reporter,
          card_fun = function(card) card
        )
        updateSelectInput(session, "dataname", choices = isolate(datanames(data())))
        output$dataset <- renderPrint({
          req(input$dataname)
          data()[[input$dataname]]
        })
      })
    },
    ui = function(id) {
      ns <- NS(id)
      sidebarLayout(
        sidebarPanel(
          teal.reporter::simple_reporter_ui(ns("reporter")),
          selectInput(ns("dataname"), "Choose a dataset", choices = NULL)
        ),
        mainPanel(verbatimTextOutput(ns("dataset")))
      )
    }
  )
}

#' Get all the module tabs in the `AppDriver` object along with their
#' root_ids and module_ids used for name spacing in the shiny ui.
get_app_module_tabs <- function(app) {
  lapply(
    app$get_html(selector = "ul.shiny-bound-input"),
    function(x) {
      el <- rvest::read_html(x)
      root_id <- el %>%
        rvest::html_element("ul") %>%
        rvest::html_attr("id") %>%
        gsub(pattern = "(^teal-main_ui-)|(-active_tab$)", replacement = "")
      tab_id <- el %>%
        rvest::html_elements("li a") %>%
        rvest::html_attr("data-value")
      tab_name <- el %>%
        rvest::html_elements("li a") %>%
        rvest::html_text()
      data.frame(
        root_id = root_id,
        tab_id = tab_id,
        tab_name = tab_name
      )
    }
  ) %>%
    do.call(what = rbind)
}

#' Get the active shiny name space for the Module content and the Filter panel.
#' Note that in the case of the filter panel, the name space is constant when it is not moudle specific.
#' However, module specific filter panel will have the name space linked with the module name space.
get_active_ns <- function(app, component = c("module", "filter_panel", "filter_manager")) {
  component <- match.arg(component)

  if (component %in% c("filter_manager", "filter_panel")) {
    component_selector <- if (identical(component, "filter_manager")) {
      sprintf("#teal-main_ui-%s-show", component)
    } else {
      sprintf("#teal-main_ui-%s", component)
    }

    if (!is.null(app$get_html(component_selector))) {
      return(sprintf("teal-main_ui-%s", component))
    }
    component <- sprintf("module_%s", component)
  }

  all_inputs <- app$get_values()$input
  active_tab_inputs <- all_inputs[grepl("-active_tab$", names(all_inputs))]

  tab_ns <- lapply(names(active_tab_inputs), function(name) {
    gsub(
      pattern = "-active_tab$",
      replacement = sprintf("-%s", active_tab_inputs[[name]]),
      name
    )
  }) %>%
    unlist()
  active_ns <- tab_ns[1]
  if (length(tab_ns) > 1) {
    for (i in 2:length(tab_ns)) {
      next_ns <- tab_ns[i]
      if (grepl(pattern = active_ns, next_ns)) {
        active_ns <- next_ns
      }
    }
  }
  sprintf("%s-%s", active_ns, component)
}
