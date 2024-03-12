testthat::test_that("e2e: the module server logic is only triggered when the teal module becomes active", {
  value_export_module <- function(label = "custom module") {
    module(
      label = label,
      server = function(id, data) {
        moduleServer(id, function(input, output, session) {
          shiny::exportTestValues(
            value = rnorm(1)
          )
        })
      },
      ui = function(id) {
        ns <- NS(id)
        h1("Module that exports a random value for testing")
      }
    )
  }

  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = modules(
      value_export_module(label = "Module 1"),
      value_export_module(label = "Module 2")
    )
  )
  app$wait_for_idle(timeout = default_idle_timeout)
  test_exports <- app$get_values()$export

  expect_equal(length(test_exports), 1)

  app$navigate_teal_tab("Module 2")
  test_exports <- app$get_values()$export

  expect_equal(length(test_exports), 2)
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
  app_modules <- sapply(
    app$get_html(selector = "ul.shiny-bound-input"),
    function(x) {
      el <- rvest::read_html(x)
      el %>%
        rvest::html_elements("li a") %>%
        rvest::html_text()
    },
    USE.NAMES = FALSE
  ) %>%
    unlist()
  testthat::expect_identical(
    app_modules,
    c(
      "Example Module", "Nested Modules", "Nested 1", "Nested 2",
      "Sub Nested Modules", "Nested 1", "Nested 1"
    )
  )
  app$stop()
})
