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
  app <- get_test_app_object(
    data = simple_teal_data,
    modules = example_module(label = "Example Module")
  )
  app$wait_for_idle(timeout = default_idle_timeout)
  expect_no_shiny_error(app)
  app$stop()
})

test_that("UI parameters of init creates the expected UI", {
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

test_that("expected teal filters are initialized when module specific filters are created", {
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
})

test_that("expected teal filters are initialized when global filters are created", {
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

test_that("reporter tab is only created when a module has reporter", {
  app_without_reporter <- get_test_app_object(
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
  teal_tabs <- app_without_reporter$get_html(selector = "#teal-main_ui-root-active_tab") %>%
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

  app_without_reporter$stop()
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
  expect_false(isTruthy(secondary_attrs$style))

  app$click(selector = ".btn.action-button.filter_hamburger")
  app$wait_for_idle(timeout = default_idle_timeout)
  primary_attrs <- get_class_attributes(app, ".teal_primary_col")
  secondary_attrs <- get_class_attributes(app, ".teal_secondary_col")

  expect_true(grepl("col-sm-12", primary_attrs$class))
  expect_true(grepl("display: none;", secondary_attrs$style))
  app$stop()
})

test_that("filter panel only shows the data supplied using datanames", {
  app <- get_test_app_object(
    data = simple_teal_data,
    modules = modules(
      example_module(label = "mtcars", datanames = "mtcars")
    )
  )
  app$view()
  app$wait_for_idle(timeout = default_idle_timeout)

  expect_identical(
    get_active_filter_vars(app),
    "mtcars"
  )
  app$stop()
})

test_that("filter panel shows all the datasets when datanames is all", {
  app <- get_test_app_object(
    data = simple_teal_data,
    modules = modules(
      example_module(label = "all", datanames = "all"),
      example_module(label = "NULL", datanames = NULL)
    )
  )
  app$view()
  app$wait_for_idle(timeout = default_idle_timeout)

  expect_identical(
    get_active_filter_vars(app),
    "mtcars"
  )
  app$stop()
})

test_that("filter panel is not created when datanames is NULL", {
  app <- get_test_app_object(
    data = simple_teal_data,
    modules = modules(
      example_module(label = "NULL", datanames = NULL)
    )
  )
  app$view()
  app$wait_for_idle(timeout = default_idle_timeout)

  expect_identical(
    app$get_html(selector = "#teal-main_ui-root-example_module-datanames") |>
      rvest::read_html() |>
      rvest::html_text(),
    "iris, mtcars"
  )
  app$stop()
})



test_that("all the nested teal modules are initiated as expected", {
  app <- get_test_app_object(
    data = simple_teal_data,
    modules = modules(
      example_module(label = "Example Module"),
      modules(
        label = "Nested Modules",
        example_module(label = "Nested 1"),
        example_module(label = "Nested 2"),
        modules(
          label = "Sub Nested Modules",
          example_module(label = "Nested 1"),
          example_module(label = "Nested 1")
        )
      )
    )
  )
  app_modules <- get_app_modules(app)
  expect_identical(
    app_modules$tab_name,
    c(
      "Example Module", "Nested Modules", "Nested 1", "Nested 2",
      "Sub Nested Modules", "Nested 1", "Nested 1"
    )
  )
})
