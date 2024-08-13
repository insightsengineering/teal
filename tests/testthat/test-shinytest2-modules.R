testthat::skip_if_not_installed("shinytest2")
testthat::skip_if_not_installed("rvest")

testthat::test_that("e2e: the module server logic is only triggered when the teal module becomes active", {
  skip_if_too_deep(5)
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

  test_exports <- app$get_values()$export

  expect_equal(length(test_exports), 1)

  app$navigate_teal_tab("Module 2")
  test_exports <- app$get_values()$export

  expect_equal(length(test_exports), 2)
  app$stop()
})


testthat::test_that("e2e: filter panel only shows the data supplied using datanames", {
  skip_if_too_deep(5)
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = modules(
      example_module(label = "mtcars", datanames = "mtcars")
    )
  )

  testthat::expect_identical(
    app$get_active_filter_vars(),
    "mtcars"
  )
  app$stop()
})

testthat::test_that("e2e: filter panel shows all the datasets when datanames is all", {
  skip_if_too_deep(5)
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = modules(
      example_module(label = "all", datanames = "all")
    )
  )

  testthat::expect_identical(
    app$get_active_filter_vars(),
    c("iris", "mtcars")
  )
  app$stop()
})


testthat::test_that("e2e: all the nested teal modules are initiated as expected", {
  skip_if_too_deep(5)
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
  app_modules <- app$get_text(selector = "ul.shiny-bound-input li a")
  testthat::expect_identical(
    app_modules,
    c(
      "Example Module", "Nested Modules", "Nested 1", "Nested 2",
      "Sub Nested Modules", "Nested 1", "Nested 1"
    )
  )
  app$stop()
})
