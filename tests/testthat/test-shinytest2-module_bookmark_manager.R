testthat::test_that("bookmark_manager_button is hidden by default", {
  skip_if_too_deep(5)
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = example_module(label = "Example Module")
  )
  on.exit(app$stop())
  app$wait_for_idle(timeout = default_idle_timeout)
  testthat::expect_identical(
    app$get_attr(".bookmark_manager_button", "style"),
    "display: none;"
  )
})


test_that("bookmark_manager_button is hidden when enableBookmarking = 'url'", {
  skip_if_too_deep(5)
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = example_module(label = "Example Module"),
    enableBookmarking = "url"
  )
  on.exit(app$stop())
  app$wait_for_idle(timeout = default_idle_timeout)
  testthat::expect_identical(
    app$get_attr(".bookmark_manager_button", "style"),
    "display: none;"
  )
})
