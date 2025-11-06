testthat::skip_if_not_installed("shinytest2")
testthat::skip_if_not_installed("rvest")

skip_if_too_deep(5)

testthat::test_that("e2e: Module with 'Show R Code' initializes with visible button", {
  app <- TealAppDriver$new(
    init(
      data = simple_teal_data(),
      modules = example_module(label = "Example Module")
    )
  )

  # Check if button exists.
  testthat::expect_identical(
    app$get_text(app$namespaces(TRUE)$base_id("source_code_wrapper-source_code-button")),
    "Show R code"
  )
  app$stop()
})

testthat::test_that("e2e: Module with 'Show R Code' has modal with two dismiss and two copy to clipboard buttons", {
  app <- TealAppDriver$new(
    init(
      data = simple_teal_data(),
      modules = example_module(label = "Example Module")
    )
  )

  app$click(selector = app$namespaces(TRUE)$base_id("source_code_wrapper-source_code-button"))

  # Check header and title content.
  testthat::expect_equal(
    app$get_text("#shiny-modal div.modal-header > h4"),
    "Show R Code"
  )

  # There are two Dismiss buttons with similar id and the same label.
  buttons_text <- app$get_text("#shiny-modal button")
  testthat::expect_setequal(buttons_text, c("Dismiss", "Copy to Clipboard", "Dismiss", "Copy to Clipboard"))
  app$stop()
})

testthat::test_that("e2e: Module with 'Show R Code' has code", {
  app <- TealAppDriver$new(
    init(
      data = simple_teal_data(),
      modules = example_module(label = "Example Module")
    )
  )

  app$click(selector = app$namespaces(TRUE)$base_id("source_code_wrapper-source_code-button"))

  # Check R code output.
  testthat::expect_identical(
    strsplit(
      app$get_text(app$namespaces(TRUE)$base_id("source_code_wrapper-source_code-verbatim_content")),
      "\n"
    )[[1]],
    c(
      "iris <- iris",
      "mtcars <- mtcars",
      sprintf('stopifnot(rlang::hash(iris) == "%s") # @linksto iris', rlang::hash(iris)),
      sprintf('stopifnot(rlang::hash(mtcars) == "%s") # @linksto mtcars', rlang::hash(mtcars)),
      ".raw_data <- list2env(list(iris = iris, mtcars = mtcars))",
      "lockEnvironment(.raw_data) # @linksto .raw_data",
      "object <- iris",
      "object"
    )
  )

  app$stop()
})
