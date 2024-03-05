simple_teal_data <- within(teal_data(), {
  iris <- iris
  mtcars <- mtcars
})
datanames(simple_teal_data) <- c("iris", "mtcars")

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

test_that("teal app initializes with no errors", {
  app_title <- "Custom Teal App Title"
  app_favicon <- "https://raw.githubusercontent.com/insightsengineering/hex-stickers/main/PNG/teal.png"
  app_header <- "Custom Teal App Header"
  app_footer <- "Custom Teal App Footer"
  app <- get_test_app_object(
    data = simple_teal_data,
    modules = example_module(label = "Example Module"),
    title = build_app_title(
      app_title,
      app_favicon
    ),
    header = app_header,
    footer = app_footer
  )
  app$wait_for_idle(timeout = default_idle_timeout)
  expect_no_shiny_error(app)

  expect_equal(
    app$get_html("head > title")[1] |>
      rvest::read_html() |>
      rvest::html_text(),
    app_title
  )
  expect_equal(
    app$get_html("head > link[rel='icon']") |>
      rvest::read_html() |>
      rvest::html_elements("link") |>
      rvest::html_attr("href"),
    app_favicon
  )
  expect_true(
    grepl(
      app_header,
      app$get_html("header") |>
        rvest::read_html() |>
        rvest::html_text()
    )
  )
  expect_true(
    grepl(
      app_footer,
      app$get_html("footer") |>
        rvest::read_html() |>
        rvest::html_text()
    )
  )
  app$stop()
})


test_that("teal filters are initialized as expected", {
  # App with module specific filters
  app <- get_test_app_object(
    data = simple_teal_data,
    modules = modules(
      example_module(label = "Module_1"),
      example_module(label = "Module_2")
    ),
    filter = teal_slices(
      teal_slice(id = "iris_species", dataname = "iris", varname = "Species", multiple = TRUE),
      teal_slice(id = "mtcars_cyl", dataname = "mtcars", varname = "cyl", selected = c(4, 6)),
      teal_slice(id = "mtcars_drat", dataname = "mtcars", varname = "drat", selected = c(3, 4)),
      teal_slice(id = "mtcars_gear", dataname = "mtcars", varname = "gear"),
      module_specific = TRUE,
      mapping = list(
        "Module_1" = c("iris_species", "mtcars_cyl"),
        "Module_2" = c("iris_species", "mtcars_drat", "mtcars_gear")
      )
    )
  )
  app$wait_for_idle(timeout = default_idle_timeout)

  expect_identical(get_active_data_filters(app, "iris", module_id = "module_1"), "Species")
  expect_identical(get_active_data_filters(app, "mtcars", module_id = "module_1"), "cyl")
  expect_identical(
    get_active_selection_value(app, "iris", "Species", module_id = "module_1"),
    c("setosa", "versicolor", "virginica")
  )
  expect_identical(
    get_active_selection_value(app, "mtcars", "cyl", module_id = "module_1"),
    c("4", "6")
  )
  expect_null(get_active_selection_value(app, "mtcars", "drat", is_numeric = TRUE, module_id = "module_1"))
  expect_null(get_active_selection_value(app, "mtcars", "gear", module_id = "module_1"))

  navigate_teal_tab(app, "module_2")
  app$wait_for_idle(timeout = default_idle_timeout)

  expect_identical(get_active_data_filters(app, "iris", module_id = "module_2"), "Species")
  expect_identical(get_active_data_filters(app, "mtcars", module_id = "module_2"), c("drat", "gear"))
  expect_identical(
    get_active_selection_value(app, "iris", "Species", module_id = "module_2"),
    c("setosa", "versicolor", "virginica")
  )
  expect_identical(
    get_active_selection_value(app, "mtcars", "drat", is_numeric = TRUE, module_id = "module_2"),
    c(3, 4)
  )
  expect_identical(
    get_active_selection_value(app, "mtcars", "gear", module_id = "module_2"),
    c("3", "4", "5")
  )
  expect_null(get_active_selection_value(app, "mtcars", "cyl", module_id = "module_2"))

  set_active_selection_value(app, "iris", "Species", "setosa", module_id = "module_2")
  navigate_teal_tab(app, "module_1")
  app$wait_for_idle(timeout = default_idle_timeout)

  expect_identical(
    get_active_selection_value(app, "iris", "Species", module_id = "module_1"),
    "setosa"
  )
  app$stop()

  # App with global filters
  app <- get_test_app_object(
    data = simple_teal_data,
    modules = modules(
      example_module(label = "Module_1"),
      example_module(label = "Module_2")
    ),
    filter = teal_slices(
      teal_slice(id = "iris_species", dataname = "iris", varname = "Species", multiple = TRUE),
      teal_slice(id = "mtcars_cyl", dataname = "mtcars", varname = "cyl", selected = c(4, 6)),
      teal_slice(id = "mtcars_drat", dataname = "mtcars", varname = "drat", selected = c(3, 4)),
      teal_slice(id = "mtcars_gear", dataname = "mtcars", varname = "gear")
    )
  )

  app$wait_for_idle(timeout = default_idle_timeout)

  expect_identical(get_active_data_filters(app, "iris"), "Species")
  expect_identical(get_active_data_filters(app, "mtcars"), c("cyl", "drat", "gear"))
  expect_identical(
    get_active_selection_value(app, "iris", "Species"),
    c("setosa", "versicolor", "virginica")
  )
  expect_identical(
    get_active_selection_value(app, "mtcars", "cyl"),
    c("4", "6")
  )
  expect_identical(
    get_active_selection_value(app, "mtcars", "drat", is_numeric = TRUE),
    c(3, 4)
  )
  expect_identical(
    get_active_selection_value(app, "mtcars", "gear"),
    c("3", "4", "5")
  )
  app$stop()
})

test_that("module with a reporter creates the reporter tab", {
  app_no_reporter <- get_test_app_object(
    data = simple_teal_data,
    modules = example_module(label = "Example Module")
  )
  app_with_reporter <- get_test_app_object(
    data = simple_teal_data,
    modules = report_module(label = "Module with Reporter")
  )

  teal_tabs <- app_with_reporter$get_html(selector = "#teal-main_ui-root-active_tab") %>%
    rvest::read_html() %>%
    html_nodes("a")
  reporter_tabs <- setNames(
    teal_tabs %>%
      rvest::html_attr("data-value"),
    teal_tabs %>%
      rvest::html_text()
  )
  teal_tabs <- app_no_reporter$get_html(selector = "#teal-main_ui-root-active_tab") %>%
    rvest::read_html() %>%
    html_nodes("a")
  non_reporter_tabs <- setNames(
    teal_tabs %>%
      rvest::html_attr("data-value"),
    teal_tabs %>%
      rvest::html_text()
  )

  expect_identical(
    non_reporter_tabs,
    c("Example Module" = "example_module")
  )
  expect_identical(
    reporter_tabs,
    c("Module with Reporter" = "module_with_reporter", "Report previewer" = "report_previewer")
  )

  app_no_reporter$stop()
  app_with_reporter$stop()
})


test_that("show/hide hamburger works as expected", {
  app <- get_test_app_object(
    data = simple_teal_data,
    modules = example_module()
  )

  get_class_attributes <- function(app, selector) {
    element <- app$get_html(selector = selector) |>
      rvest::read_html() |>
      html_nodes(selector)
    list(
      class = element |>
        html_attr("class"),
      style = element |>
        html_attr("style")
    )
  }

  primary_attrs <- get_class_attributes(app, ".teal_primary_col")
  secondary_attrs <- get_class_attributes(app, ".teal_secondary_col")

  expect_true(grepl("col-sm-9", primary_attrs$class))
  expect_true(grepl("", secondary_attrs$style))

  app$click(selector = ".btn.action-button.filter_hamburger")
  app$wait_for_idle(timeout = default_idle_timeout)
  primary_attrs <- get_class_attributes(app, ".teal_primary_col")
  secondary_attrs <- get_class_attributes(app, ".teal_secondary_col")

  expect_true(grepl("col-sm-12", primary_attrs$class))
  expect_true(grepl("display: none;", secondary_attrs$style))
})

test_that("check the teal app", {
  app <- get_test_app_object(simple_teal_data, all_modules)

  app$wait_for_idle(timeout = default_idle_timeout)
  app$get_url()
  app$view()

  app$stop()

  ns <- module_ns_shiny2(app)

  app$get_value(input = "teal-main_ui-root-active_tab")
  app$get_html(selector = "#teal-main_ui-root-active_tab")

  app$set_inputs(
    !!"teal-main_ui-root-active_tab" := get_unique_labels("Nested_Modules")
  )
  app$set_inputs(
    !!"teal-main_ui-root-active_tab" := get_unique_labels("Module_with_Report")
  )

  app$stop()


  root <- "root"
  teal_tabs <- app$get_html(selector = paste0("#teal-main_ui-", root, "-active_tab")) %>%
    rvest::read_html() %>%
    html_nodes("a")

  tab_selections <- setNames(
    teal_tabs %>%
      rvest::html_attr("data-value"),
    teal_tabs %>%
      rvest::html_text()
  )

  app$set_inputs(
    !!"teal-main_ui-root-active_tab" := tab_selections[["Example_Module"]]
  )


  root <- tab_selections[["Root"]]
  nested_tabs <- app$get_html(
    selector = paste0("#teal-main_ui-root-", root, "-active_tab")
  )
  if (!is.null(nested_tabs)) {
    tabs <- nested_tabs %>%
      rvest::read_html() %>%
      html_nodes("a")
    nested_tabs <- setNames(
      tabs %>%
        rvest::html_attr("data-value"),
      tabs %>%
        rvest::html_text()
    )
    app$set_inputs(
      !!paste0("teal-main_ui-root-", root, "-active_tab") := nested_tabs[["Nested_1"]]
    )
    app$set_inputs(
      !!(paste0("teal-main_ui-root-", root, "-active_tab")) := nested_tabs[["Nested_2"]]
    )
  }



  app$set_inputs(
    !!paste0("teal-main_ui-root-", root, "-active_tab") := nested_tabs[["Nested_1"]]
  )
  app$set_inputs(
    !!(paste0("teal-main_ui-root-", root, "-active_tab")) := nested_tabs[["Nested_2"]]
  )



  "#teal-main_ui-root-nested_modules-active_tab"

  str(asdf)

  htmltools::tagQuery(asdf, "a")


  app$set_inputs(
    !!"teal-main_ui-root-active_tab" := tab_selections[["Nested_Modules"]]
  )






  # Add a new filter variable
  add_new_filter_var(app, "data_frame", "id")
  remove_filter_var(app, "data_frame", "id")

  get_active_selection_value(app, "data_frame", "logical")
  set_active_selection_value(app, "data_frame", "logical", c("FALSE"))
  remove_active_selection(app, "data_frame", "logical")

  res <- app$get_value(output = ns("text"))
  expect_identical(digest::digest(res), "75998fe20286e4767586546ceb01539e")
  # Show R code
  app$click(input = ns("rcode-button"))
})
