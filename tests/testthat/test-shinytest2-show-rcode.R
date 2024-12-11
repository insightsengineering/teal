testthat::skip_if_not_installed("shinytest2")
testthat::skip_if_not_installed("rvest")

testthat::test_that("e2e: teal app initializes with Show R Code modal", {
  skip_if_too_deep(5)
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = example_module(label = "Example Module")
  )

  # Check if button exists.
  button_selector <- app$active_module_element("rcode-button")
  testthat::expect_equal(
    app$get_text(button_selector),
    "Show R code"
  )

  app$click(selector = button_selector)

  # Check header and title content.
  testthat::expect_equal(
    app$get_text("#shiny-modal div.modal-header > h4"),
    "Example Code"
  )

  # There are two Dismiss buttons with similar id and the same label.
  testthat::expect_setequal(
    testthat::expect_length(
      app$get_text("#shiny-modal button[data-dismiss]"),
      2
    ),
    "Dismiss"
  )
  # Check for Copy buttons.
  testthat::expect_equal(
    app$get_text(app$active_module_element("rcode-copy_button1")),
    "Copy to Clipboard"
  )
  testthat::expect_equal(
    app$get_text(app$active_module_element("rcode-copy_button2")),
    "Copy to Clipboard"
  )

  # Check R code output.
  testthat::expect_setequal(
    strsplit(app$get_text(app$active_module_element("rcode-verbatim_content")), "\n")[[1]],
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
