testthat::skip_if_not_installed("shinytest2")
testthat::skip_if_not_installed("rvest")

testthat::describe("e2e: Module with 'Show R Code'", {
  skip_if_too_deep(5)

  it("initializes with visible button", {
    app <- TealAppDriver$new(
      data = simple_teal_data(),
      modules = example_module(label = "Example Module")
    )

    # Check if button exists.
    testthat::expect_identical(
      app$get_text(app$active_module_element("rcode-button")),
      "Show R code"
    )
    app$stop()
  })

  it("has modal with dismiss and copy to clipboard buttons", {
    app <- TealAppDriver$new(
      data = simple_teal_data(),
      modules = example_module(label = "Example Module")
    )

    app$click(selector = app$active_module_element("rcode-button"))

    # Check header and title content.
    testthat::expect_equal(
      app$get_text("#shiny-modal div.modal-header > h4"),
      "Example Code"
    )

    # There are two Dismiss buttons with similar id and the same label.
    dismiss_text <- app$get_text("#shiny-modal button[data-dismiss]")
    testthat::expect_length(dismiss_text, 2)
    testthat::expect_setequal(dismiss_text, "Dismiss")

    # Check for Copy buttons.
    testthat::expect_equal(
      app$get_text(app$active_module_element("rcode-copy_button1")),
      "Copy to Clipboard"
    )
    testthat::expect_equal(
      app$get_text(app$active_module_element("rcode-copy_button2")),
      "Copy to Clipboard"
    )

    app$stop()
  })

  it("has code", {
    app <- TealAppDriver$new(
      data = simple_teal_data(),
      modules = example_module(label = "Example Module")
    )

    app$click(selector = app$active_module_element("rcode-button"))

    # Check R code output.
    testthat::expect_identical(
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
})
