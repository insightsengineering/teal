testthat::test_that("e2e: teal app initializes with no errors", {
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = example_module(label = "Example Module")
  )
  app$wait_for_idle(timeout = default_idle_timeout)
  app$expect_no_shiny_error()
  app$stop()
})

testthat::test_that("e2e: init creates UI containing specified title, favicon, header and footer", {
  app_title <- "Custom Teal App Title"
  app_favicon <- "https://raw.githubusercontent.com/insightsengineering/hex-stickers/main/PNG/teal.png"
  app_header <- "Custom Teal App Header"
  app_footer <- "Custom Teal App Footer"
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = example_module(label = "Example Module"),
    title = build_app_title(
      app_title,
      app_favicon
    ),
    header = app_header,
    footer = app_footer
  )
  app$wait_for_idle(timeout = default_idle_timeout)

  testthat::expect_equal(
    app$get_html("head > title")[1] %>%
      rvest::read_html() %>%
      rvest::html_text(),
    app_title
  )
  testthat::expect_equal(
    app$get_html("head > link[rel='icon']") %>%
      rvest::read_html() %>%
      rvest::html_elements("link") %>%
      rvest::html_attr("href"),
    app_favicon
  )
  testthat::expect_true(
    grepl(
      app_header,
      app$get_html("header") %>%
        rvest::read_html() %>%
        rvest::html_text()
    )
  )
  testthat::expect_true(
    grepl(
      app_footer,
      app$get_html("footer") %>%
        rvest::read_html() %>%
        rvest::html_text()
    )
  )
  app$stop()
})

testthat::test_that("e2e: reporter tab is only created when a module has reporter", {
  app_without_reporter <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = example_module(label = "Example Module")
  )
  app_with_reporter <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = report_module(label = "Module with Reporter")
  )

  teal_tabs <- app_with_reporter$get_html(selector = "#teal-main_ui-root-active_tab") %>%
    rvest::read_html() %>%
    rvest::html_elements("a")
  reporter_tabs <- setNames(
    teal_tabs %>%
      rvest::html_attr("data-value"),
    teal_tabs %>%
      rvest::html_text()
  )
  teal_tabs <- app_without_reporter$get_html(selector = "#teal-main_ui-root-active_tab") %>%
    rvest::read_html() %>%
    rvest::html_elements("a")
  non_reporter_tabs <- setNames(
    teal_tabs %>%
      rvest::html_attr("data-value"),
    teal_tabs %>%
      rvest::html_text()
  )

  testthat::expect_identical(
    non_reporter_tabs,
    c("Example Module" = "example_module")
  )
  testthat::expect_identical(
    reporter_tabs,
    c("Module with Reporter" = "module_with_reporter", "Report previewer" = "report_previewer")
  )

  app_without_reporter$stop()
  app_with_reporter$stop()
})

testthat::test_that("e2e: show/hide hamburger works as expected", {
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = example_module()
  )

  get_class_attributes <- function(app, selector) {
    element <- app$get_html(selector = selector) %>%
      rvest::read_html() %>%
      rvest::html_elements(selector)
    list(
      class = element %>%
        rvest::html_attr("class"),
      style = element %>%
        rvest::html_attr("style")
    )
  }

  primary_attrs <- get_class_attributes(app, ".teal_primary_col")
  secondary_attrs <- get_class_attributes(app, ".teal_secondary_col")

  testthat::expect_true(grepl("col-sm-9", primary_attrs$class))
  testthat::expect_false(isTruthy(secondary_attrs$style))

  app$click(selector = ".btn.action-button.filter_hamburger")
  app$wait_for_idle(timeout = default_idle_timeout)
  primary_attrs <- get_class_attributes(app, ".teal_primary_col")
  secondary_attrs <- get_class_attributes(app, ".teal_secondary_col")

  testthat::expect_true(grepl("col-sm-12", primary_attrs$class))
  testthat::expect_true(grepl("display: none;", secondary_attrs$style))
  app$stop()
})

testthat::test_that("e2e: filter panel only shows the data supplied using datanames", {
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = modules(
      example_module(label = "mtcars", datanames = "mtcars")
    )
  )
  app$wait_for_idle(timeout = default_idle_timeout)

  testthat::expect_identical(
    app$get_active_filter_vars(),
    "mtcars"
  )
  app$stop()
})

testthat::test_that("e2e: filter panel shows all the datasets when datanames is all", {
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = modules(
      example_module(label = "all", datanames = "all")
    )
  )
  app$wait_for_idle(timeout = default_idle_timeout)

  testthat::expect_identical(
    app$get_active_filter_vars(),
    c("iris", "mtcars")
  )
  app$stop()
})

testthat::test_that("e2e: filter panel is not displayed when datanames is NULL", {
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = modules(
      example_module(label = "NULL", datanames = NULL)
    )
  )
  app$wait_for_idle(timeout = default_idle_timeout)

  testthat::expect_identical(
    app$get_html(".teal_secondary_col") %>%
      rvest::read_html() %>%
      rvest::html_element("div") %>%
      rvest::html_attr("style"),
    "display: none;"
  )

  app$stop()
})

testthat::test_that("e2e: all the nested teal modules are initiated as expected", {
  app <- TealAppDriver$new(
    data = simple_teal_data(),
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
  app_modules <- get_app_module_tabs(app)
  testthat::expect_identical(
    app_modules$tab_name,
    c(
      "Example Module", "Nested Modules", "Nested 1", "Nested 2",
      "Sub Nested Modules", "Nested 1", "Nested 1"
    )
  )
  app$stop()
})
